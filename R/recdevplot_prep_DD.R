# Copyright 2024 Fisheries Queensland

# This file is part of SSAND.
# SSAND is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
# SSAND is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
# You should have received a copy of the GNU General Public License along with SSAND. If not, see <https://www.gnu.org/licenses/>.

#' Prepare DDUST data for recdevplot()
#'
#' @param dd_mle A list of outputs from DDUST::makefullreport() with one list element per scenario. Will automatically reformat as a list if a single DDUST::makefullreport() output (i.e. one scenario) is entered
#' @param dd_mcmc A list of model fits from tmbstan::tmbstan() with one list element per scenario. Only needed if MCMC was used.
#' @param dd_sim A list of outputs from SSAND::simulate_DD() with one list element per scenario. Only required if MCMC was used. Will automatically reformat as a list if a single DDUST::makefullreport() output (i.e. one scenario) is entered
#' @param scenarios A vector of scenarios to plot (numeric). Shows all scenarios if left blank. Can be overridden in the plotting function.
#' @param intervals A vector of credible interval values to be displayed on banded MCMC plot (numeric). For example, "0.9" denotes the 90% credible interval.
#' @param CI_range Specify credible interval range (numeric). Only activated if mcmc_style==CI.
#'
#' @return A data frame with variables called year (int), value (int), ub (num), lb (num), median (num, if MCMC==TRUE), scenario (factor), method (chr).
#' @export
#'
#' @examples
#' data <- recdevplot_prep_DD(dd_mle)
#' recdevplot(data)
#'
#' \dontrun{
#' # MCMC model
#' library(DDUST)
#' dd_sim <- simulate_DDUST(dd_mle,dd_mcmc)
#' data <- recdevplot_prep_DD(dd_mle,dd_mcmc,dd_sim)
#' recdevplot(data)
#'
#' # Ensemble model
#' dd_mcmc_ens <- mcmc_ensemble_DD(dd_mcmc,dd_sim,scenarios=c(1,2))$dd_mcmc
#' dd_sim_ens <- mcmc_ensemble_DD(dd_mcmc,dd_sim,scenarios=c(1,2))$dd_sim
#' data <- recdevplot_prep_DD(dd_mle,dd_mcmc_ens,dd_sim_ens)
#' recdevplot(data)
#' }
recdevplot_prep_DD <- function(dd_mle,
                               dd_mcmc = NULL,
                               dd_sim = NULL,
                               scenarios = NULL,
                               intervals = c(0.2, 0.5, 0.7, 0.9, 0.95),
                               CI_range = 0.95){

  if (missing(dd_mcmc)) {MCMC = FALSE} else {MCMC = TRUE}

  if (check_scenarios(dd_mle,"DD","MLE")=="single scenario"){dd_mle <- list(dd_mle)}
  if (MCMC && check_scenarios(dd_mcmc,"DD","MCMC")=="single scenario"){dd_mcmc <- list(dd_mcmc)}
  if (MCMC && check_scenarios(dd_sim,"DD","SIM")=="single scenario"){dd_sim <- list(dd_sim)}

  if (MCMC && missing(scenarios)){scenarios <- 1:length(dd_mcmc)}
  if (!MCMC && missing(scenarios)){scenarios <- 1:length(dd_mle)}
  if (MCMC && check_scenarios(dd_mcmc,"DD","MCMC")=="ensemble") {dd_mle <- list(dd_mle[[1]])}

  if (!MCMC) {
    data_all <- data.frame()

    for (scenario in scenarios) {

      years_before_recdevs <- dd_mle[[scenario]]$data$first_year_rec_devs - dd_mle[[scenario]]$data$first_year_catch + 1
      estimated_recdevs <- dd_mle[[scenario]]$RecDev[years_before_recdevs:length(dd_mle[[scenario]]$RecDev)]
      estimated_recdevs_sd <- dd_mle[[scenario]]$RecDev_sd[years_before_recdevs:length(dd_mle[[scenario]]$RecDev_sd)]

      tmp <- data.frame(year = dd_mle[[scenario]]$data$first_year_rec_devs:dd_mle[[scenario]]$data$last_year_catch,
                        value = estimated_recdevs,
                        ub = estimated_recdevs + 1.96*estimated_recdevs_sd,
                        lb = estimated_recdevs - 1.96*estimated_recdevs_sd)
      tmp$scenario <- scenario
      tmp$method <- "MLE"

      data_all <- rbind(data_all, tmp)
    }
  }
  if (MCMC) {
    if (check_scenarios(dd_mcmc,"DD","MCMC")=="ensemble") {scenarios <- 1}

    data_all <- data.frame()

    for (scenario in scenarios) {

      startyr <- dd_mle[[scenario]]$data$first_year_catch
      endyr <- dd_mle[[scenario]]$data$last_year_catch

      sim <- dd_sim[[scenario]]

      ddtmpdata <- as.data.frame(t(sapply(1:length(sim), function(i){sim[[i]]$RecDev})) )
      names(ddtmpdata) <- startyr:endyr

      # Find median trajectory (median value of last year of Bratio)
      pos <- mcmc_median_position_DD(dd_mle,dd_mcmc,dd_sim)[scenario]

      median_trajectory <- ddtmpdata[pos,] |>
        tidyr::pivot_longer(cols=everything(), names_to = "year", values_to = "value") |>
        dplyr::mutate(year = as.numeric(year)) |>
        dplyr::mutate(rownum=0,scenario=scenario,med="trajectory",interval=NA,prob_lower=NA,prob_upper=NA) |>
        dplyr::select(rownum,scenario,year,value,med,interval,prob_lower,prob_upper)

      # Find median rec dev for each year
      median_recdevs <- data.frame(
        rownum = 0,
        scenario = scenario,
        year = median_trajectory$year,
        value = apply(ddtmpdata,2,quantile,probs = 0.5),
        med = "median_recdevs",
        interval = 0.5,
        prob_lower = NA,
        prob_upper = NA
      )

      # Box plot prep
      boxplotprep <- ddtmpdata |>
        tibble::rownames_to_column("rownum") |>
        tidyr::pivot_longer(cols=-rownum, names_to = "year", values_to = "value") |>
        dplyr::mutate(rownum=as.numeric(rownum),value, scenario = scenario, med="MCMC",interval=NA,prob_lower=NA,prob_upper=NA) |>
        dplyr::select(rownum,scenario,year,value,med,interval,prob_lower,prob_upper)

      # Banded plot prep
      intervals <- round(intervals,10)
      intervals_desc <- sort(intervals,decreasing = TRUE)

      quant_bio_lower <- apply(ddtmpdata,2,quantile, na.rm = TRUE, probs = (1-intervals)/2)
      quant_bio_upper <- apply(ddtmpdata,2,quantile, na.rm = TRUE, probs = 1-(1-intervals)/2)

      prob_lower <- quant_bio_lower |>
        t() |>
        as.data.frame() |>
        tibble::rownames_to_column("year") |>
        dplyr::filter(year %in% c(startyr : endyr)) |>
        tidyr::pivot_longer(-year, names_to="interval", values_to = "prob_lower") |>
        dplyr::mutate(interval = as.numeric(stringr::str_sub(interval, end = -2))/100) |>
        dplyr::mutate(interval = 1 - interval * 2) |>
        dplyr::mutate(interval = round(interval, 10))

      prob_upper <- quant_bio_upper |>
        t() |>
        as.data.frame() |>
        tibble::rownames_to_column("year") |>
        dplyr::filter(year %in% c(startyr : endyr)) |>
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

      # CI plot prep
      CI_data_quants <- apply(ddtmpdata,2,quantile,probs = c((1-CI_range)/2, 1-(1-CI_range)/2))

      CI_data <- data.frame(
        lb = CI_data_quants[1,],
        ub = CI_data_quants[2,]) |>
        tibble::rownames_to_column("year") |>
        dplyr::filter(year %in% c(startyr : endyr)) |>
        dplyr::mutate(scenario = scenario,
                      rownum = 0,
                      interval = CI_range,
                      med = "CI",
                      value=NA) |>
        dplyr::select(rownum,scenario,year,value,med,interval,prob_lower=lb,prob_upper=ub)


      data_all <- rbind(data_all, median_trajectory, median_recdevs, boxplotprep, band_data, CI_data)
      data_all <- data_all |> dplyr::mutate(year=as.numeric(year))
    }
  }
  data <- data_all

  # If ensemble model, force to only produce one scenario. All scenarios are duplicates without this filter.
  # Allows users to put in long list of dd_mle
  if (MCMC && check_scenarios(dd_mcmc,"DD","MCMC")=="ensemble") {
    data <- data |> dplyr::filter(scenario==1)
  }

  rownames(data) <- NULL
  return(data)
}
