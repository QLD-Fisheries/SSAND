# Copyright 2024 Fisheries Queensland

# This file is part of SSAND.
# SSAND is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
# SSAND is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
# You should have received a copy of the GNU General Public License along with SSAND. If not, see <https://www.gnu.org/licenses/>.

#' Prepare Stock Synthesis data for Recruitment deviation distribution plot
#'
#' @param ss_mle A list of outputs from r4ss::SS_output() with one element per scenario. Will automatically reformat as a list if a single r4ss::SS_output() output (i.e. one scenario) is entered.
#' @param ss_mcmc A list of outputs from r4ss::SSgetMCMC() with one element per scenario. Only needed if MCMC was used. Will automatically reformat as a list if a single r4ss::SSgetMCMC() output (i.e. one scenario) is entered.
#' @param scenarios A vector of scenarios to plot (numeric). Shows all scenarios if left blank. Can be overridden in the plotting function.
#' @param intervals A vector of credible interval values to be displayed on banded MCMC plot (numeric). For example, "0.9" denotes the 90% credible interval.
#' @param CI_range Specify credible interval range (numeric). Only activated if mcmc_style==CI.
#'
#' @return A data frame with variables called year (int), value (int), ub (num), lb (num), median (num, if MCMC), scenario (factor), method (chr).

recruitmentdistributiondeviationplot_prep_SS <- function(ss_mle,
                               ss_mcmc = NULL,
                               scenarios = NULL,
                               intervals = c(0.2, 0.5, 0.7, 0.9, 0.95),
                               CI_range = 0.95
){

  if (missing(ss_mcmc)) {MCMC = FALSE} else {MCMC = TRUE}
  if (check_scenarios(ss_mle,"SS","MLE")=="single scenario"){ss_mle <- list(ss_mle)}
  if (MCMC && check_scenarios(ss_mcmc,"SS","MCMC")=="single scenario"){ss_mcmc <- list(ss_mcmc)}

  if (MCMC && missing(scenarios)){scenarios <- 1:length(ss_mcmc)}
  if (!MCMC && missing(scenarios)){scenarios <- 1:length(ss_mle)}
  if (MCMC && check_scenarios(ss_mcmc,"SS","MCMC")=="ensemble") {ss_mle <- list(ss_mle[[1]])}

  if (!MCMC) {
    data_all <- data.frame()
    for (scenario in scenarios) {

      if (is.null(ss_mle[[scenario]]$RecrDistpars)) {next}

      tmp <- ss_mle[[scenario]]$RecrDistpars |>
        dplyr::filter(stringr::str_detect(Label, "DEVmult")) |>
        dplyr::mutate(year = stringr::str_extract(Label, "(?<=_)(\\d+)$")) |>
        dplyr::select(year, value=Value) |>
        dplyr::mutate(scenario = scenario,
                      method = "MLE") |>
        dplyr::select(year, value, scenario, method)

      rownames(tmp) <- NULL
      data_all <- rbind(data_all, tmp)
    }
  }

  # if (MCMC) {
  #   data_all <- data.frame()
  #   for(scenario in scenarios) {
  #
  #     # get rec devs
  #     tmpdata <- ss_mcmc[[scenario]] |>
  #       dplyr::select(dplyr::contains("RecrDev_"))
  #
  #     # If no rec devs calculated, skip to next scenario
  #     if (length(tmpdata)==0) {next}
  #
  #     # Find median trajectory (median value of last year of Bratio)
  #     pos <- mcmc_median_position_SS(ss_mle,ss_mcmc)[scenario]
  #
  #     median_trajectory <- tmpdata[pos,] |>
  #       tidyr::pivot_longer(cols=everything(), names_to = "year", values_to = "value") |>
  #       dplyr::mutate(year = as.numeric(substr(year,14,17))) |>
  #       dplyr::mutate(rownum=0,scenario=scenario,med="trajectory",interval=NA,prob_lower=NA,prob_upper=NA) |>
  #       dplyr::select(rownum,scenario,year,value,med,interval,prob_lower,prob_upper)
  #
  #     # Find median rec dev for each year
  #     median_recdevs <- data.frame(
  #       rownum = 0,
  #       scenario = scenario,
  #       year = median_trajectory$year,
  #       value = apply(tmpdata,2,quantile,probs = 0.5),
  #       med = "median_recdevs",
  #       interval = 0.5,
  #       prob_lower = NA,
  #       prob_upper = NA
  #     )
  #
  #     # Box plot prep
  #     boxplotprep <- tmpdata |>
  #       tibble::rownames_to_column("rownum") |>
  #       tidyr::pivot_longer(cols=-rownum, names_to = "Parameters", values_to = "Value") |>
  #       dplyr::mutate(year = as.numeric(substr(Parameters,14,17))) |>
  #       dplyr::mutate(rownum=as.numeric(rownum),value=Value, scenario = scenario, med="MCMC",interval=NA,prob_lower=NA,prob_upper=NA) |>
  #       dplyr::select(rownum,scenario,year,value,med,interval,prob_lower,prob_upper)
  #
  #     # Banded plot prep
  #     intervals <- round(intervals,10)
  #     intervals_desc <- sort(intervals,decreasing = TRUE)
  #
  #     quant_bio_lower <- apply(tmpdata,2,quantile, na.rm = TRUE, probs = (1-intervals)/2)
  #     quant_bio_upper <- apply(tmpdata,2,quantile, na.rm = TRUE, probs = 1-(1-intervals)/2)
  #
  #     prob_lower <- quant_bio_lower |>
  #       t() |>
  #       as.data.frame() |>
  #       tibble::rownames_to_column("year") |>
  #       dplyr::mutate(year = as.numeric(substr(year,14,17))) |>
  #       dplyr::filter(year %in% c(ss_mle[[scenario]]$startyr : ss_mle[[scenario]]$endyr)) |>
  #       tidyr::pivot_longer(-year, names_to="interval", values_to = "prob_lower") |>
  #       dplyr::mutate(interval = as.numeric(stringr::str_sub(interval, end = -2))/100) |>
  #       dplyr::mutate(interval = 1 - interval * 2) |>
  #       dplyr::mutate(interval = round(interval, 10))
  #
  #     prob_upper <- quant_bio_upper |>
  #       t() |>
  #       as.data.frame() |>
  #       tibble::rownames_to_column("year") |>
  #       dplyr::mutate(year = as.numeric(substr(year,14,17))) |>
  #       dplyr::filter(year %in% c(ss_mle[[scenario]]$startyr : ss_mle[[scenario]]$endyr)) |>
  #       tidyr::pivot_longer(-year, names_to="interval", values_to = "prob_upper") |>
  #       dplyr::mutate(interval = as.numeric(stringr::str_sub(interval, end = -2))/100) |>
  #       dplyr::mutate(interval = 1-(1-interval)*2)  |>
  #       dplyr::mutate(interval = round(interval, 10))
  #
  #     band_data <- prob_lower |>
  #       cbind(prob_upper = prob_upper$prob_upper) |>
  #       dplyr::mutate(scenario = scenario,
  #                     rownum = 0,
  #                     med = "band",
  #                     year = as.numeric(year),
  #                     value = 0) |>
  #       dplyr::select(rownum, scenario, year, value, med, interval, prob_lower, prob_upper)
  #
  #     # CI plot prep
  #     CI_data_quants <- apply(tmpdata,2,quantile,probs = c((1-CI_range)/2, 1-(1-CI_range)/2))
  #
  #     CI_data <- data.frame(
  #       lb = CI_data_quants[1,],
  #       ub = CI_data_quants[2,]) |>
  #       tibble::rownames_to_column("year") |>
  #       dplyr::mutate(year = as.numeric(substr(year,14,17))) |>
  #       dplyr::filter(year %in% c(ss_mle[[scenario]]$startyr : ss_mle[[scenario]]$endyr)) |>
  #       dplyr::mutate(scenario = scenario,
  #                     rownum = 0,
  #                     interval = CI_range,
  #                     med = "CI",
  #                     value=NA) |>
  #       dplyr::select(rownum,scenario,year,value,med,interval,prob_lower=lb,prob_upper=ub)
  #
  #
  #     data_all <- rbind(data_all, median_trajectory, median_recdevs, boxplotprep, band_data, CI_data)
  #   }
  # }
  data <-data_all
  rownames(data) <- NULL
  return(data)
}
