# Copyright 2024 Fisheries Queensland

# This file is part of SSAND.
# SSAND is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
# SSAND is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
# You should have received a copy of the GNU General Public License along with SSAND. If not, see <https://www.gnu.org/licenses/>.

#' Prepare DDUST data for seasonal catchability pattern
#'
#' @param dd_mle A list of outputs from DDUST::makefullreport() with one list element per scenario. Will automatically reformat as a list if a single DDUST::makefullreport() output (i.e. one scenario) is entered
#' @param dd_mcmc A list of model fits from tmbstan::tmbstan() with one list element per scenario. Only needed if MCMC was used.
#' @param dd_sim A list of outputs from SSAND::simulate_DD() with one list element per scenario. Only required if MCMC was used. Will automatically reformat as a list if a single DDUST::makefullreport() output (i.e. one scenario) is entered
#' @param scenarios A vector of scenarios to plot (numeric). Shows all scenarios if left blank. Can be overridden in the plotting function.
#' @param fleets A vector of which fleet/s to plot (numeric).
#'
#' @return A data frame with q (num), month (int), month_point (int), monthnames (factor), fleet (factor), scenario (factor)
#' @export
#'
#' @examples
#' data <- catchabilityplot_prep_DD(dd_mle)
#' catchabilityplot(data)
#'
#' \dontrun{
#' # MCMC model
#' library(DDUST)
#' dd_sim <- simulate_DDUST(dd_mle,dd_mcmc)
#'
#' data <- catchabilityplot_prep_DD(dd_mle,dd_mcmc,dd_sim)
#' catchabilityplot(data)
#'
#' # Ensemble model:
#' library(DDUST)
#' dd_sim <- simulate_DDUST(dd_mle,dd_mcmc)
#' dd_mcmc_ens <- mcmc_ensemble_DD(dd_mcmc,dd_sim)$dd_mcmc
#' dd_sim_ens <- mcmc_ensemble_DD(dd_mcmc,dd_sim)$dd_sim
#' data <- catchabilityplot_prep_DD(dd_mle,dd_mcmc_ens,dd_sim_ens)
#' catchabilityplot(data)
#' }
catchabilityplot_prep_DD <- function(dd_mle,
                                     dd_mcmc = NULL,
                                     dd_sim = NULL,
                                     scenarios = NULL,
                                     fleets = NULL){

  if (missing(dd_mcmc)) {MCMC = FALSE} else {MCMC = TRUE}
  if (check_scenarios(dd_mle,"DD","MLE")=="single scenario"){dd_mle <- list(dd_mle)}
  if (MCMC && check_scenarios(dd_mcmc,"DD","MCMC")=="single scenario"){dd_mcmc <- list(dd_mcmc)}
  if (MCMC && check_scenarios(dd_sim,"DD","SIM")=="single scenario"){dd_sim <- list(dd_sim)}



  if (missing(scenarios)){scenarios <- 1:length(dd_mle)}

  if (!MCMC) {
    data_all <- data.frame()

    for (scenario in scenarios) {
      q1 <- dd_mle[[scenario]]$q1
      q2 <- dd_mle[[scenario]]$q2
      log_q <- dd_mle[[scenario]]$log_q
      data <- data.frame(q = numeric(),
                         month = numeric(),
                         fleet = factor(),
                         scenario = factor())
      for (j in 1:length(dd_mle[[scenario]]$log_q)){
        log_q <- dd_mle[[scenario]]$log_q[j]
        scenario
        data <- rbind(data,
                      data.frame(
                        q = exp(log_q+q1*cos(2*pi*dd_mle[[scenario]]$data$month_sequence/12)+q2*sin(2*pi*dd_mle[[scenario]]$data$month_sequence/12)),
                        month = 1:12,
                        fleet = as.factor(j),
                        scenario = as.factor(scenario)
                      )
        )
      }

      nmonths <- dd_mle[[scenario]]$data$Number_months_per_timestep

      data <- data |>
        dplyr::mutate(month_point = ifelse(month %% nmonths == 1, month, NA)) |>
        dplyr::mutate(nmonths = nmonths,
                      month_point = ifelse(nmonths==1,month,month_point)) |>
        dplyr::select(-nmonths)

      data_all <- rbind(data_all, data)
    }
  }

  if (MCMC) {

    # Remove annual models
    scenarios <- scenarios[sapply(scenarios,function(i){dd_mle[[i]]$data$Number_months_per_timestep!=12})]

    data_all <- data.frame(q = numeric(),
                           month = numeric(),
                           fleet = factor(),
                           scenario = numeric())

    for (scenario in scenarios) {

      startyr <- dd_mle[[scenario]]$data$first_year_catch
      endyr <- dd_mle[[scenario]]$data$last_year_catch

      # Extract fit_model and model for each scenario, convert fit_model to a matrix
      fit <- as.matrix(dd_mcmc[[scenario]])
      nyears <- dd_mle[[scenario]]$data$last_year_catch - dd_mle[[scenario]]$data$first_year_catch + 1

      # Simulate model for each iteration of MCMC
      sim <- dd_sim[[scenario]]

      # Find median trajectory (median value of last year of Bratio)
      pos <- mcmc_median_position_DD(dd_mle,dd_mcmc,dd_sim)[scenario]

      if ("q1" %in% names(dd_mcmc[[scenario]])){
        q1 <- fit[pos, "q1"]
        q2 <- fit[pos, "q2"]
      } else {
        q1 <- dd_mle[[scenario]]$parameters$q1
        q2 <- dd_mle[[scenario]]$parameters$q2
      }

      log_q <- sim[[pos]]$log_q
      data <- data.frame(q = numeric(),
                         month = numeric(),
                         fleet = factor())

      for (j in 1:length(dd_mle[[scenario]]$log_q)){
        log_q <- sim[[pos]]$log_q[j]
        data <- rbind(data,data.frame(q = exp(log_q+q1*cos(2*pi*dd_mle[[scenario]]$data$month_sequence/12)+q2*sin(2*pi*dd_mle[[scenario]]$data$month_sequence/12)),
                                      month = 1:12,
                                      fleet = as.factor(j))
        )
      }

      nmonths <- dd_mle[[scenario]]$data$Number_months_per_timestep

      data <- data |>
        dplyr::mutate(month_point = ifelse(month %% nmonths == 1, month, NA)) |>
        dplyr::mutate(nmonths = nmonths,
                      month_point = ifelse(nmonths==1,month,month_point)) |>
        dplyr::select(-nmonths)

      data_all <- rbind(data_all, data |> dplyr::mutate(scenario = scenario))
    }

    if (!missing(fleets)) {
      data_all <- data_all |>
        dplyr::filter(fleet %in% fleets)
    }
  }

  data <- data_all |> dplyr::mutate(monthnames = as.factor(month.name[month]))

  rownames(data) <- NULL
  return(data)
}
