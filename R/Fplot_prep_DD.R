# Copyright 2024 Fisheries Queensland

# This file is part of SSAND.
# SSAND is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
# SSAND is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
# You should have received a copy of the GNU General Public License along with SSAND. If not, see <https://www.gnu.org/licenses/>.

#' Prepare DDUST data for fishing mortality plot
#'
#' @param dd_mle A list of outputs from DDUST::makefullreport() with one list element per scenario. Will automatically reformat as a list if a single DDUST::makefullreport() output (i.e. one scenario) is entered
#' @param dd_mcmc A list of model fits from tmbstan::tmbstan() with one list element per scenario. Only needed if MCMC was used.
#' @param dd_sim A list of outputs from SSAND::simulate_DD() with one list element per scenario. Only required if MCMC was used. Will automatically reformat as a list if a single DDUST::makefullreport() output (i.e. one scenario) is entered
#' @param scenarios A vector of scenarios to plot (numeric). Shows all scenarios if left blank. Can be overridden in the plotting function.
#' @param intervals A vector of credible interval values to be displayed on banded MCMC plot (numeric). For example, "0.9" denotes the 90% credible interval.
#' @param CI_range Specify credible interval range (numeric). Only activated if mcmc_style==CI.
#' @param F_relative_to_MSY Set to TRUE to report F relative to FMSY
#'
#' @return A data frame with variables date, fleet, obs, exp, ub, lb, scenario, biomass_definition
#' @export
#'
#' @examples
#' data <- Fplot_prep_DD(dd_mle)
#' Fplot(data)
#'
#' \dontrun{
#' library(DDUST)
#' dd_sim <- simulate_DDUST(dd_mle,dd_mcmc)
#' data <- Fplot_prep_DD(dd_mle,dd_mcmc,dd_sim)
#' Fplot(data)
#'
#' dd_mcmc_ens <- mcmc_ensemble_DD(dd_mcmc,dd_sim,scenarios=c(1))$dd_mcmc
#' dd_sim_ens <- mcmc_ensemble_DD(dd_mcmc,dd_sim,scenarios=c(1))$dd_sim
#' data <- Fplot_prep_DD(dd_mle,dd_mcmc_ens,dd_sim_ens)
#' Fplot(data)
#' }
Fplot_prep_DD <- function(dd_mle,
                          dd_mcmc = NULL,
                          dd_sim = NULL,
                          scenarios = NULL,
                          intervals = c(0.2, 0.5, 0.7, 0.9, 0.95),
                          CI_range = 0.95,
                          F_relative_to_MSY = FALSE
) {

  if (missing(dd_mcmc)) {MCMC = FALSE} else {MCMC = TRUE}
  if (check_scenarios(dd_mle,"DD","MLE")=="single scenario"){dd_mle <- list(dd_mle)}
  if (MCMC && check_scenarios(dd_mcmc,"DD","MCMC")=="single scenario"){dd_mcmc <- list(dd_mcmc)}
  if (MCMC && check_scenarios(dd_sim,"DD","SIM")=="single scenario"){dd_sim <- list(dd_sim)}

  if (MCMC && missing(scenarios)){scenarios <- 1:length(dd_mcmc)}
  if (!MCMC && missing(scenarios)){scenarios <- 1:length(dd_mle)}
  if (MCMC && check_scenarios(dd_mcmc,"DD","MCMC")=="ensemble") {dd_mle <- list(dd_mle[[1]])}

  if (!MCMC) {
    data_all <- data.frame()

    for (scenario in scenarios){

      temp <- data.frame(year = dd_mle[[scenario]]$data$first_year_catch:dd_mle[[scenario]]$data$last_year_catch,
                         value = dd_mle[[scenario]]$F_annual,
                         upper = dd_mle[[scenario]]$F_annual + 1.96 * dd_mle[[scenario]]$F_annual_sd,
                         lower = dd_mle[[scenario]]$F_annual - 1.96 * dd_mle[[scenario]]$F_annual_sd,
                         scenario = scenario)


      if (F_relative_to_MSY) {
        temp <- data.frame(year = dd_mle[[scenario]]$data$first_year_catch:dd_mle[[scenario]]$data$last_year_catch,
                           F_annual = dd_mle[[scenario]]$F_annual,
                           F_annual_sd = dd_mle[[scenario]]$F_annual_sd,
                           Fmsy = dd_mle[[scenario]]$Fmsy[[1]],
                           Fmsy_sd = dd_mle[[scenario]]$Fmsy_sd[[1]],
                           scenario = scenario) |>
          # Propagation of error for division:
          dplyr::mutate(value = F_annual/Fmsy,
                        sd = value * sqrt( (F_annual_sd/F_annual)^2 + (Fmsy_sd/Fmsy)^2 ) ,
                        upper = value + 1.96*sd,
                        lower = value - 1.96*sd) |>
          dplyr::select(year,value,upper,lower,scenario)
      }



      data_all <- rbind(data_all,temp)
    }
  }

  if (MCMC) {
    data_all <- data.frame()

    if (check_scenarios(dd_mcmc,"DD","MCMC")=="ensemble") {scenarios <- 1}

    for (scenario in scenarios) {

      startyr <- dd_mle[[scenario]]$data$first_year_catch
      endyr <- dd_mle[[scenario]]$data$last_year_catch

      sim <- dd_sim[[scenario]]

      ddtmpdata <- as.data.frame(t(sapply(1:nrow(dd_mcmc[[scenario]]),
                                          function(i){(matrix(sim[[i]]$F_annual))})))

      if (F_relative_to_MSY) {
        Fmsydata <- as.matrix(sapply(1:nrow(dd_mcmc[[scenario]]),
                                     function(i){matrix(sim[[i]]$Fmsy, nrow = 1)}))

        ddtmpdata <- ddtmpdata / Fmsydata

        rows_no_nan <- apply(ddtmpdata, 1, function(row) !any(is.nan(row)))
        ddtmpdata <- ddtmpdata[rows_no_nan, ]
      }

      names(ddtmpdata) <- startyr:endyr

      # Find median trajectory (median value of last year of Bratio)
      pos <- mcmc_median_position_DD(dd_mle,dd_mcmc,dd_sim)[scenario]

      median_trajectory <- ddtmpdata[pos,] |>
        tidyr::pivot_longer(cols=everything(), names_to = "year", values_to = "value") |>
        dplyr::mutate(year = as.numeric(year)) |>
        dplyr::mutate(rownum=0,scenario=scenario,med="trajectory",interval=NA,prob_lower=NA,prob_upper=NA) |>
        dplyr::select(rownum,scenario,year,value,med,interval,prob_lower,prob_upper)

      # Find median F for each year
      median_F <- data.frame(
        rownum = 0,
        scenario = scenario,
        year = median_trajectory$year,
        value = apply(ddtmpdata,2,quantile,probs = 0.5),
        med = "median_F",
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

      data_all <- rbind(data_all, median_trajectory, median_F, boxplotprep, band_data, CI_data)
      data_all <- data_all |> dplyr::mutate(year=as.numeric(year))
    }
  }
  data <- data_all |>
    dplyr::mutate(scenario = factor(scenario, levels = scenarios),
                  biomass_definition = "vulnerable")

  rownames(data) <- NULL
  return(data)

}
