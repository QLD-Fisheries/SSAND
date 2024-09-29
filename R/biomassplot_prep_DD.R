# Copyright 2024 Fisheries Queensland

# This file is part of SSAND.
# SSAND is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
# SSAND is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
# You should have received a copy of the GNU General Public License along with SSAND. If not, see <https://www.gnu.org/licenses/>.

#' Prepare DDUST data for biomassplot()
#' If using MCMC, the "interval", "prob_lower" and "prob_upper" column will return NA when med="MCMC".
#'
#' @param dd_mle A list of outputs from DDUST::makefullreport() with one list element per scenario. Will automatically reformat as a list if a single DDUST::makefullreport() output (i.e. one scenario) is entered
#' @param dd_mcmc A list of model fits from tmbstan::tmbstan() with one list element per scenario. Only needed if MCMC was used.
#' @param dd_sim A list of outputs from DDUST::simulate_DD() with one list element per scenario. Only required if MCMC was used. Will automatically reformat as a list if a single DDUST::makefullreport() output (i.e. one scenario) is entered
#' @param scenarios A vector of scenarios to plot (numeric). Shows all scenarios if left blank. Can be overridden in the plotting function.
#' @param intervals A vector of credible interval values to be displayed on banded MCMC plot (numeric). For example, "0.9" denotes the 90% credible interval.
#' @param biomass_type The type of biomass used. Options are "relative" or "absolute" (character).
#'
#' @return If MCMC is being used, returns a data frame with variables called rownum, scenario, year, value, med, interval, prob_lower, prob_upper, biomass_type, biomass_definition. If MLE, returns a data frame with variables called year, value, lower, upper, scenario, biomass_type, biomass_definition.
#' @export
#'
#' @examples
#' data <- biomassplot_prep_DD(dd_mle)
#' biomassplot(data)
#'
#' \dontrun{
#' # MCMC model
#' library(DDUST)
#' dd_sim <- simulate_DDUST(dd_mle,dd_mcmc)
#'
#' data <- biomassplot_prep_DD(dd_mle, dd_mcmc, dd_sim,
#'                             intervals = c(0.2, 0.5, 0.7, 0.9, 0.95))
#' biomassplot(data)
#'
#' # Ensemble model:
#' library(DDUST)
#' dd_sim <- simulate_DDUST(dd_mle,dd_mcmc)
#' dd_mcmc_ens <- mcmc_ensemble_DD(dd_mcmc,dd_sim)$dd_mcmc
#' dd_sim_ens <- mcmc_ensemble_DD(dd_mcmc,dd_sim)$dd_sim
#' data <- biomassplot_prep_DD(dd_mle, dd_mcmc_ens, dd_sim_ens)
#' biomassplot(data)
#' }
biomassplot_prep_DD <- function(dd_mle,
                                dd_mcmc = NULL,
                                dd_sim = NULL,
                                scenarios = NULL,
                                intervals = c(0.2, 0.5, 0.7, 0.9, 0.95),
                                biomass_type = "relative"){

  if (missing(dd_mcmc)) {MCMC = FALSE} else {MCMC = TRUE}

  if (check_scenarios(dd_mle,"DD","MLE")=="single scenario"){dd_mle <- list(dd_mle)}
  if (MCMC && check_scenarios(dd_mcmc,"DD","MCMC")=="single scenario"){dd_mcmc <- list(dd_mcmc)}
  if (MCMC && check_scenarios(dd_sim,"DD","SIM")=="single scenario"){dd_sim <- list(dd_sim)}

  if (MCMC && missing(scenarios)){scenarios <- 1:length(dd_mcmc)}
  if (!MCMC && missing(scenarios)){scenarios <- 1:length(dd_mle)}

  if (MCMC && check_scenarios(dd_mcmc,"DD","MCMC")=="ensemble") {dd_mle <- list(dd_mle[[1]])}

  if (!missing(intervals) & missing(dd_mcmc)) {warning("You've not entered MCMC data yet specified credible intervals for use in an MCMC plot")}

  if (!MCMC) {
    data <- data.frame()
    for (scenario in scenarios) {

      if (biomass_type == "relative") {
        biomass <- data.frame(year = seq(dd_mle[[scenario]]$data$first_year_catch,dd_mle[[scenario]]$data$last_year_catch,1),
                              value = dd_mle[[scenario]]$B_annual_ratio,
                              lower = dd_mle[[scenario]]$B_annual_ratio - 1.96*dd_mle[[scenario]]$B_annual_ratio_sd,
                              upper = dd_mle[[scenario]]$B_annual_ratio + 1.96*dd_mle[[scenario]]$B_annual_ratio_sd,
                              scenario = scenario)
      }
      if (biomass_type == "absolute") {
        biomass <- data.frame(year = seq(dd_mle[[scenario]]$data$first_year_catch,dd_mle[[scenario]]$data$last_year_catch,1),
                              value = dd_mle[[scenario]]$B_annual,
                              lower = dd_mle[[scenario]]$B_annual - 1.96*dd_mle[[scenario]]$B_annual_sd,
                              upper = dd_mle[[scenario]]$B_annual + 1.96*dd_mle[[scenario]]$B_annual_sd,
                              scenario = scenario)
      }

      data <- rbind(data, biomass)
    }
  }


  if (MCMC) {
    data <- data.frame()

    if (check_scenarios(dd_mcmc,"DD","MCMC")=="ensemble") {scenarios <- 1}

    for (scenario in scenarios) {

      startyr <- dd_mle[[scenario]]$data$first_year_catch
      endyr <- dd_mle[[scenario]]$data$last_year_catch

      # Extract fit_model and model for each scenario, convert fit_model to a matrix
      if (check_scenarios(dd_mcmc,"DD","MCMC")=="ensemble") {
        fit <- dd_mcmc[[1]]
      } else {
        fit <- as.matrix(dd_mcmc[[scenario]])
      }

      nyears <- dd_mle[[scenario]]$data$last_year_catch - dd_mle[[scenario]]$data$first_year_catch + 1

      # Simulate model for each iteration of MCMC
      sim <- dd_sim[[scenario]]

      # Find median trajectory (median value of last year of Bratio)
      pos <- mcmc_median_position_DD(dd_mle,dd_mcmc,dd_sim)[scenario]

      # Extract the named element from each inner list using lapply
      if (biomass_type == "relative") {
        extracted_vectors <- lapply(sim, function(inner_list) inner_list[["B_annual_ratio"]])

        # Convert the list of vectors to a data frame
        extracted_vectors_df <- as.data.frame(do.call(rbind, extracted_vectors))

        # Rename columns to represent years
        names(extracted_vectors_df) <- c(startyr:endyr)

        # Get median biomass for each year
        sim_bio <- t(sapply(1:length(sim), function(j){matrix(sim[[j]]$B_annual_ratio, byrow = TRUE)}))
        quant_bio <- apply(sim_bio,2,quantile, na.rm = TRUE, probs = c(0.025, 0.5, 0.975))
      }

      if (biomass_type == "absolute") {
        extracted_vectors <- lapply(sim, function(inner_list) inner_list[["B_annual"]])

        # Convert the list of vectors to a data frame
        extracted_vectors_df <- as.data.frame(do.call(rbind, extracted_vectors))

        # Rename columns to represent years
        names(extracted_vectors_df) <- c(startyr:endyr)

        # Get median biomass for each year
        sim_bio <- t(sapply(1:length(sim), function(j){matrix(sim[[j]]$B_annual, byrow = TRUE)}))
        quant_bio <- apply(sim_bio,2,quantile, na.rm = TRUE, probs = c(0.025, 0.5, 0.975))
      }

      # Generate data frame of all MCMC runs
      all_mcmc <- extracted_vectors_df |>
        dplyr::mutate(rownum = dplyr::row_number(), scenario = scenario) |>
        tidyr::pivot_longer(cols=-c(rownum, scenario), names_to = "year", values_to = "value") |>
        dplyr::mutate(year = as.numeric(year), value = as.numeric(value)) |>
        dplyr::mutate(med = "MCMC") |>
        dplyr::mutate(interval = NA, prob_lower=NA, prob_upper=NA) |>
        dplyr::select(rownum, scenario, year, value, med, interval, prob_lower, prob_upper)

      # Find trajectory that produces the median final biomass
      median_trajectory <- all_mcmc |>
        dplyr::mutate(med = ifelse(rownum==pos,"trajectory","MCMC"),
                      rownum = ifelse(rownum==pos,0,rownum))

      # Add median biomass
      median_annual_biomass <- data.frame(rownum=0,
                                          scenario = scenario,
                                          year = startyr:endyr,
                                          value = quant_bio[2,],
                                          med = "annual_biomass",
                                          interval = as.numeric(NA),
                                          prob_lower=as.numeric(NA),
                                          prob_upper=as.numeric(NA))

      # Banded plot prep
      intervals <- round(intervals,10)
      intervals_desc <- sort(intervals,decreasing = TRUE)

      quant_bio_lower <- apply(sim_bio,2,quantile, na.rm = TRUE, probs = (1-intervals)/2)
      quant_bio_upper <- apply(sim_bio,2,quantile, na.rm = TRUE, probs = 1-(1-intervals)/2)

      prob_lower <- quant_bio_lower |>
        t() |>
        as.data.frame() |>
        dplyr::mutate(year = dd_mle[[scenario]]$data$first_year_catch : dd_mle[[scenario]]$data$last_year_catch) |>
        tidyr::pivot_longer(-year, names_to="interval", values_to = "prob_lower") |>
        dplyr::mutate(interval = as.numeric(stringr::str_sub(interval, end = -2))/100) |>
        dplyr::mutate(interval = 1 - interval * 2) |>
        dplyr::mutate(interval = round(interval, 10))

      prob_upper <- quant_bio_upper |>
        t() |>
        as.data.frame() |>
        dplyr::mutate(year = dd_mle[[scenario]]$data$first_year_catch : dd_mle[[scenario]]$data$last_year_catch) |>
        tidyr::pivot_longer(-year, names_to="interval", values_to = "prob_upper") |>
        dplyr::mutate(interval = as.numeric(stringr::str_sub(interval, end = -2))/100) |>
        dplyr::mutate(interval = 1-(1-interval)*2)  |>
        dplyr::mutate(interval = round(interval, 10))

      band_data <- prob_lower |>
        cbind(prob_upper = prob_upper$prob_upper) |>
        dplyr::mutate(scenario = scenario,
                      rownum = 0,
                      med = "band",
                      year = as.numeric(year),
                      value = 0) |>
        dplyr::select(rownum, scenario, year, value, med, interval, prob_lower, prob_upper)

      data <- data |> rbind(median_trajectory, median_annual_biomass, band_data)
    }
  }

  data$biomass_type <- biomass_type
  data$biomass_definition <- "vulnerable"

  rownames(data) <- NULL
  return(data)
}
