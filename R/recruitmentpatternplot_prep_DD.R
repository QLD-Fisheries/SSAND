# Copyright 2024 Fisheries Queensland

# This file is part of SSAND.
# SSAND is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
# SSAND is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
# You should have received a copy of the GNU General Public License along with SSAND. If not, see <https://www.gnu.org/licenses/>.

#' Prepare DDUST data for recruitment pattern plot
#'
#' @param dd_mle A list of outputs from DDUST::makefullreport() with one list element per scenario. Will automatically reformat as a list if a single DDUST::makefullreport() output (i.e. one scenario) is entered
#' @param dd_mcmc A list of model fits from tmbstan::tmbstan() with one list element per scenario. Only needed if MCMC was used.
#' @param dd_sim A list of outputs from SSAND::simulate_DD() with one list element per scenario. Only required if MCMC was used. Will automatically reformat as a list if a single DDUST::makefullreport() output (i.e. one scenario) is entered
#' @param scenarios A vector of scenarios to plot (numeric). Shows all scenarios if left blank. Can be overridden in the plotting function.
#'
#' @return A dataframe with recruitment (num), months (num), monthnames (factor), scenario (factor), format (character)
#' @export
#'
#' @examples
#' data <- recruitmentpatternplot_prep_DD(dd_mle)
#' recruitmentpatternplot(data)
#'
#' \dontrun{
#' library(DDUST)
#' dd_sim <- simulate_DDUST(dd_mle,dd_mcmc)
#' data <- recruitmentpatternplot_prep_DD(dd_mle,dd_mcmc,dd_sim)
#' recruitmentpatternplot(data)
#' }
recruitmentpatternplot_prep_DD <- function(dd_mle,
                                           dd_mcmc = NULL,
                                           dd_sim = NULL,
                                           scenarios = NULL){

  if (missing(dd_mcmc)) {MCMC = FALSE} else {MCMC = TRUE}

  if (check_scenarios(dd_mle,"DD","MLE")=="single scenario"){dd_mle <- list(dd_mle)}
  if (MCMC && check_scenarios(dd_mcmc,"DD","MCMC")=="single scenario"){dd_mcmc <- list(dd_mcmc)}
  if (MCMC && check_scenarios(dd_sim,"DD","SIM")=="single scenario"){dd_sim <- list(dd_sim)}

  if (missing(scenarios)){scenarios <- 1:length(dd_mle)}

  if (!MCMC) {
    data <- data.frame()
    for (scenario in scenarios) {

      recdat <- data.frame(recruitment = dd_mle[[scenario]]$rec_pattern[1:(12/dd_mle[[scenario]]$data$Number_months_per_timestep)],
                           months = seq(1,12,by=dd_mle[[scenario]]$data$Number_months_per_timestep),
                           monthnames = as.factor(month.name[seq(1,12,by=dd_mle[[scenario]]$data$Number_months_per_timestep)]),
                           scenario = as.factor(scenario),
                           area = "1")

      data <- rbind(data, recdat)
    }
  }


  if (MCMC) {
    recdat <- data.frame(recruitment = numeric(),
                         months = numeric(),
                         monthnames = character(),
                         scenario = character())

    for (scenario in scenarios) {
      startyr <- dd_mle[[scenario]]$data$first_year_catch
      endyr <- dd_mle[[scenario]]$data$last_year_catch

      # Extract fit_model and model for each scenario, convert fit_model to a matrix
      fit <- as.matrix(dd_mcmc[[scenario]])
      nyears <- dd_mle[[scenario]]$data$last_year_catch - dd_mle[[scenario]]$data$first_year_catch + 1

      # Simulate model for each iteration of MCMC
      sim <- dd_sim[[scenario]]

      # Find median parameter set
      B_final <- sapply(1:nrow(fit), function(i){sim[[i]]$B_annual_ratio[length(sim[[i]]$B_annual_ratio)]})
      pos <- mcmc_median_position_DD(dd_mle,dd_mcmc,dd_sim)[scenario]

      recdat <- rbind(recdat,data.frame(recruitment = sim[[pos]]$rec_pattern[1:(12/dd_mle[[scenario]]$data$Number_months_per_timestep)],
                                        months = seq(1,12,by=dd_mle[[scenario]]$data$Number_months_per_timestep),
                                        monthnames = month.name[seq(1,12,by=dd_mle[[scenario]]$data$Number_months_per_timestep)],
                                        scenario = scenario))
    }

    data <- recdat |>
      dplyr::mutate(monthnames = as.factor(monthnames)) |>
      dplyr::mutate(format = "points") |>
      dplyr::mutate(area="1")
  }
  rownames(data) <- NULL
  return(data)

}
