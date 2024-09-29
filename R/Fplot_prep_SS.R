# Copyright 2024 Fisheries Queensland

# This file is part of SSAND.
# SSAND is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
# SSAND is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
# You should have received a copy of the GNU General Public License along with SSAND. If not, see <https://www.gnu.org/licenses/>.

#' Prepare Stock Synthesis data for fishing mortality plot
#'
#' @param ss_mle A list of outputs from r4ss::SS_output() with one element per scenario. Will automatically reformat as a list if a single r4ss::SS_output() output (i.e. one scenario) is entered.
#' @param ss_mcmc A list of outputs from r4ss::SSgetMCMC() with one element per scenario. Only needed if MCMC was used. Will automatically reformat as a list if a single r4ss::SSgetMCMC() output (i.e. one scenario) is entered.
#' @param scenarios A vector of scenarios to plot (numeric). Shows all scenarios if left blank. Can be overridden in the plotting function.
#' @param intervals A vector of credible interval values to be displayed on banded MCMC plot (numeric). For example, "0.9" denotes the 90% credible interval.
#' @param CI_range Specify credible interval range (numeric). Only activated if mcmc_style==CI.
#' @param biomass_definition The definition of biomass used. Options are "spawning" or "vulnerable" (character).
#' @param selectivity_fleet Fleet number used for the selectivity applied to calculate vulnerable biomass.
#'
#' @return A data frame with variables date, fleet, obs, exp, ub, lb, scenario
#' @export
#'
#' @examples
#' data <- Fplot_prep_SS(ss_mle,ss_mcmc)
#' Fplot(data)
#' # MCMC
#' data <- Fplot_prep_SS(ss_mle, ss_mcmc)
#' Fplot(data)
#'
#' # Ensemble model
#' ss_mcmc_ens <- mcmc_ensemble_SS(ss_mcmc)
#' data <- Fplot_prep_SS(ss_mle, ss_mcmc_ens)
#' Fplot(data)
Fplot_prep_SS <- function(ss_mle,
                          ss_mcmc = NULL,
                          scenarios = NULL,
                          intervals = c(0.2, 0.5, 0.7, 0.9, 0.95),
                          CI_range = 0.95,
                          biomass_definition = "spawning",
                          selectivity_fleet = NULL
){

  if (missing(ss_mcmc)) {MCMC = FALSE} else {MCMC = TRUE}
  if (check_scenarios(ss_mle,"SS","MLE")=="single scenario"){ss_mle <- list(ss_mle)}
  if (MCMC && check_scenarios(ss_mcmc,"SS","MCMC")=="single scenario"){ss_mcmc <- list(ss_mcmc)}

  if (MCMC && missing(scenarios)){scenarios <- 1:length(ss_mcmc)}
  if (!MCMC && missing(scenarios)){scenarios <- 1:length(ss_mle)}
  if (MCMC && check_scenarios(ss_mcmc,"SS","MCMC")=="ensemble") {ss_mle <- list(ss_mle[[1]])}

  if (missing(selectivity_fleet) & biomass_definition == 'vulnerable'){stop('Please provide the fleet for the selectivity applied to calculate vulnerable biomass.')}


  if (!MCMC) {
    data <- data.frame()
    if (biomass_definition == 'vulnerable'){
      for (scenario in scenarios) {
        VB <- biomassplot_prep_SS(ss_mle,
                                  scenarios = scenario,
                                  biomass_type = 'absolute',
                                  biomass_definition = 'vulnerable',
                                  selectivity_fleet = selectivity_fleet) |>
          dplyr::mutate(VB = value) |>
          dplyr::select(year, VB)
        catch <- catchplot_prep_SS(ss_mle, scenarios = scenario) |>
          dplyr::filter(fleet == selectivity_fleet) |>
          dplyr::mutate(year = as.numeric(format(as.Date(date, format="%Y/%m/%d"),"%Y")), catch = value) |>
          dplyr::select(year, catch)
        temp <- dplyr::left_join(VB, catch, by = 'year') |> # left_join against VB to remove years with no VB estimate
          dplyr::mutate(harvestrate = catch/VB, value = -log(1-harvestrate), upper = value, lower = value, scenario = scenario) |>
          dplyr::select(year, value, upper, lower, scenario)
        data <- rbind(data, temp)

      }
    } else {
      for (scenario in scenarios) {
        yrs <- ss_mle[[scenario]]$startyr:ss_mle[[scenario]]$endyr # (period with F estimates)
        temp <- ss_mle[[scenario]]$derived_quants |>
          dplyr::filter(Label %in% paste("F",yrs,sep="_")) |>
          dplyr::mutate(year=as.integer(substr(Label,3,6))) |>
          dplyr::mutate(upper=Value + 1.96 * StdDev, lower=Value - 1.96 * StdDev) |>
          dplyr::mutate(scenario = scenario) |>
          dplyr::select(year, value=Value, upper, lower, scenario)

        data <- rbind(data, temp)
      }
    }
  }

  if (MCMC) {
    data <- data.frame()
    for(scenario in scenarios) {
      # get F
      tmpdata18 <- ss_mcmc[[scenario]] |>
        dplyr::select(dplyr::contains("F_18"))
      tmpdata19 <- ss_mcmc[[scenario]] |>
        dplyr::select(dplyr::contains("F_19"))
      tmpdata20 <- ss_mcmc[[scenario]] |>
        dplyr::select(dplyr::contains("F_20"))

      tmpdata <- tmpdata18 |> cbind(tmpdata19,tmpdata20)

      # Find median trajectory (median value of last year of Bratio)
      pos <- mcmc_median_position_SS(ss_mle,ss_mcmc)[scenario]

      median_trajectory <- tmpdata[pos,] |>
        tidyr::pivot_longer(cols=everything(), names_to = "year", values_to = "value") |>
        dplyr::mutate(year = as.numeric(substr(year,3,6))) |>
        dplyr::mutate(rownum=0,scenario=scenario,med="trajectory",interval=NA,prob_lower=NA,prob_upper=NA) |>
        dplyr::select(rownum,scenario,year,value,med,interval,prob_lower,prob_upper)

      # Find median rec dev for each year
      median_F <- data.frame(
        rownum = 0,
        scenario = scenario,
        year = median_trajectory$year,
        value = apply(tmpdata,2,quantile,probs = 0.5),
        med = "median_F",
        interval = 0.5,
        prob_lower = NA,
        prob_upper = NA
      )

      # Box plot prep
      boxplotprep <- tmpdata |>
        tibble::rownames_to_column("rownum") |>
        tidyr::pivot_longer(cols=-rownum, names_to = "Parameters", values_to = "Value") |>
        dplyr::mutate(year = as.numeric(substr(Parameters,3,6))) |>
        dplyr::mutate(rownum=as.numeric(rownum),value=Value, scenario = scenario, med="MCMC",interval=NA,prob_lower=NA,prob_upper=NA) |>
        dplyr::select(rownum,scenario,year,value,med,interval,prob_lower,prob_upper)

      # Banded plot prep
      intervals <- round(intervals,10)
      intervals_desc <- sort(intervals,decreasing = TRUE)

      quant_bio_lower <- apply(tmpdata,2,quantile, na.rm = TRUE, probs = (1-intervals)/2)
      quant_bio_upper <- apply(tmpdata,2,quantile, na.rm = TRUE, probs = 1-(1-intervals)/2)

      prob_lower <- quant_bio_lower |>
        t() |>
        as.data.frame() |>
        tibble::rownames_to_column("year") |>
        dplyr::mutate(year = as.numeric(substr(year,3,6))) |>
        tidyr::pivot_longer(-year, names_to="interval", values_to = "prob_lower") |>
        dplyr::mutate(interval = as.numeric(stringr::str_sub(interval, end = -2))/100) |>
        dplyr::mutate(interval = 1 - interval * 2) |>
        dplyr::mutate(interval = round(interval, 10))

      prob_upper <- quant_bio_upper |>
        t() |>
        as.data.frame() |>
        tibble::rownames_to_column("year") |>
        dplyr::mutate(year = as.numeric(substr(year,3,6))) |>
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
      CI_data_quants <- apply(tmpdata,2,quantile,probs = c((1-CI_range)/2, 1-(1-CI_range)/2))

      CI_data <- data.frame(
        lb = CI_data_quants[1,],
        ub = CI_data_quants[2,]) |>
        tibble::rownames_to_column("year") |>
        dplyr::mutate(year = as.numeric(substr(year,3,6))) |>
        dplyr::mutate(scenario = scenario,
                      rownum = 0,
                      interval = CI_range,
                      med = "CI",
                      value=NA) |>
        dplyr::select(rownum,scenario,year,value,med,interval,prob_lower=lb,prob_upper=ub)


      data <- rbind(data, median_trajectory, median_F, boxplotprep, band_data, CI_data)
    }
  }
  rownames(data) <- NULL
  data$biomass_definition <- biomass_definition
  return(data)
}

