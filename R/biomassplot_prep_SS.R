# Copyright 2024 Fisheries Queensland

# This file is part of SSAND.
# SSAND is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
# SSAND is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
# You should have received a copy of the GNU General Public License along with SSAND. If not, see <https://www.gnu.org/licenses/>.

#' Prepare Stock Synthesis data for SSAND::biomassplot()
#' If using MCMC, the "interval", "prob_lower" and "prob_upper" column will return NA when med="MCMC".
#'
#' @param ss_mle A list of outputs from r4ss::SS_output() with one element per scenario. Will automatically reformat as a list if a single r4ss::SS_output() output (i.e. one scenario) is entered.
#' @param ss_mcmc A list of outputs from r4ss::SSgetMCMC() with one element per scenario. Only needed if MCMC was used. Will automatically reformat as a list if a single r4ss::SSgetMCMC() output (i.e. one scenario) is entered.
#' @param scenarios A vector of scenarios to plot (numeric). Shows all scenarios if left blank. Can be overridden in the plotting function.
#' @param period Optional: time period of stock assessment.
#' @param biomass_type The type of biomass used. Options are "relative" or "absolute" (character).
#' @param biomass_definition The definition of biomass used. Options are "spawning" or "vulnerable" (character).
#' @param selectivity_fleet Fleet number used for the selectivity applied to calculate vulnerable biomass.
#' @param intervals A vector of credible interval values to be displayed on banded MCMC plot (numeric). For example, "0.9" denotes the 90% credible interval.
#'
#' @return If MCMC is being used, returns a data frame with variables called rownum, scenario, year, value, med, interval, prob_lower, prob_upper, biomass_type, biomass_definition. If MLE, returns a data frame with variables called year, value, lower, upper, scenario, biomass_type, biomass_definition.
#'
#' @export
#'
#' @examples
#' data <- biomassplot_prep_DD(dd_mle)
#' biomassplot(data)
#'
#' # Vulnerable biomass
#' data <- biomassplot_prep_SS(ss_mle,
#'                             biomass_type = 'absolute',
#'                             biomass_definition = 'vulnerable',
#'                             intervals = c(0.2, 0.5, 0.7, 0.9, 0.95),
#'                             scenarios = 1)
#'
#' # MCMC
#' data <- biomassplot_prep_SS(ss_mle, ss_mcmc)
#' biomassplot(data)
#'
#' # Ensemble model
#' ss_mcmc_ens <- mcmc_ensemble_SS(ss_mcmc)
#' data <- biomassplot_prep_SS(ss_mle, ss_mcmc_ens)
#' biomassplot(data)
biomassplot_prep_SS <- function(ss_mle,
                                ss_mcmc = NULL,
                                scenarios = NULL,
                                period = NULL,
                                biomass_type = "relative",
                                biomass_definition = "spawning",
                                selectivity_fleet = 2,
                                intervals = c(0.2, 0.5, 0.7, 0.9, 0.95)) {

  if (missing(ss_mcmc)) {MCMC = FALSE} else {MCMC = TRUE}
  # if (MCMC & length(ss_mle)!=length(ss_mcmc)) {stop("Length of MLE and MCMC inputs are not the same, implying a mismatch in scenarios.")}

  if (check_scenarios(ss_mle,"SS","MLE")=="single scenario"){ss_mle <- list(ss_mle)}
  if (MCMC && check_scenarios(ss_mcmc,"SS","MCMC")=="single scenario"){ss_mcmc <- list(ss_mcmc)}

  if (MCMC && missing(scenarios)){scenarios <- 1:length(ss_mcmc)}
  if (!MCMC && missing(scenarios)){scenarios <- 1:length(ss_mle)}

  if (MCMC && check_scenarios(ss_mcmc,"SS","MCMC")=="ensemble") {ss_mle <- list(ss_mle[[1]])}

  if (MCMC & biomass_definition == 'vulnerable'){warning('Vulnerable biomass not available for MCMC, using spawning biomass instead.')}

  if (biomass_definition=="vulnerable") {warning("Note that this plot of vulnerable biomass does not account for retention and discards.")}

  if (!MCMC) {
    data <- data.frame()
    for (scenario in scenarios) {
      if (missing(period)) {period = (ss_mle[[scenario]]$startyr+1) : (ss_mle[[scenario]]$endyr+1)  }

      if (biomass_type == "relative") {
        if (biomass_definition == "spawning"){
          biomass <- ss_mle[[scenario]]$derived_quants |>
            dplyr::filter(Label %in% paste("Bratio",period,sep="_")) |>
            dplyr::mutate(year=as.numeric(substr(Label,8,11))) |>
            dplyr::mutate(upper=Value + 1.96 * StdDev, lower=Value - 1.96 * StdDev) |>
            dplyr::mutate(scenario = scenario) |>
            dplyr::select(year, value=Value, upper, lower, scenario)
        }

        if (biomass_definition == "vulnerable"){

          sel_at_age_female <- ss_mle[[scenario]]$ageselex |>
            dplyr::filter(Fleet == selectivity_fleet, Yr == tail(period,1), Factor == 'Asel2', Sex == 1) |>
            dplyr::select(!c(Factor, Fleet, Yr, Sex, Label, Seas, Morph))
          numbers_at_age_female <- ss_mle[[scenario]]$natage |>
            dplyr::filter(`Beg/Mid` == 'M', Yr %in% period, Sex == 1) |>
            as.data.frame() |>
            dplyr::select(!c(Area, Bio_Pattern, Sex, BirthSeas, Settlement,
                             Platoon, Morph, Yr, Seas, Time, `Beg/Mid`, Era))
          weight_at_age_female <- ss_mle[[scenario]]$ageselex |>
            dplyr::filter(Fleet == selectivity_fleet, Yr == tail(period,1), Factor == 'bodywt', Sex == 1) |>
            dplyr::select(!c(Factor, Fleet, Yr, Seas, Sex, Morph, Label))
          VB_female <- as.matrix(numbers_at_age_female) %*% diag(weight_at_age_female) %*% as.matrix(sel_at_age_female |> t())

          sel_at_age_male <- ss_mle[[scenario]]$ageselex |>
            dplyr::filter(Fleet == selectivity_fleet, Yr == tail(period,1), Factor == 'Asel2', Sex == 2) |>
            dplyr::select(!c(Factor, Fleet, Yr, Sex, Label, Seas, Morph))
          numbers_at_age_male <- ss_mle[[scenario]]$natage |>
            dplyr::filter(`Beg/Mid` == 'M', Yr %in% period, Sex == 2) |>
            as.data.frame() |>
            dplyr::select(!c(Area, Bio_Pattern, Sex, BirthSeas, Settlement,
                             Platoon, Morph, Yr, Seas, Time, `Beg/Mid`, Era))
          weight_at_age_male <- ss_mle[[scenario]]$ageselex |>
            dplyr::filter(Fleet == selectivity_fleet, Yr == tail(period,1), Factor == 'bodywt', Sex == 2) |>
            dplyr::select(!c(Factor, Fleet, Yr, Seas, Sex, Morph, Label))
          VB_male <- as.matrix(numbers_at_age_male) %*% diag(weight_at_age_male) %*% as.matrix(sel_at_age_male |> t())

          if (dim(VB_male)[1] == 0) {VB <- VB_female} else {VB <- VB_female + VB_male}

          biomass <- data.frame(year = period, value = VB/VB[1,1], scenario = scenario, upper = VB/VB[1,1], lower = VB/VB[1,1])

        }
      }

      if (biomass_type == "absolute") {
        if (biomass_definition == "spawning"){
          biomass <- ss_mle[[scenario]]$derived_quants |>
            dplyr::filter(Label %in% paste("SSB",period,sep="_")) |>
            dplyr::mutate(year=as.numeric(substr(Label,5,8))) |>
            dplyr::mutate(upper=Value + 1.96 * StdDev, lower=Value - 1.96 * StdDev) |>
            dplyr::mutate(scenario = scenario) |>
            dplyr::select(year, value=Value, upper, lower, scenario)
        }
        if (biomass_definition == "vulnerable"){

          sel_at_age_female <- ss_mle[[scenario]]$ageselex |>
            dplyr::filter(Fleet == selectivity_fleet, Yr == tail(period,1), Factor == 'Asel2', Sex == 1) |>
            dplyr::select(!c(Factor, Fleet, Yr, Sex, Label, Seas, Morph))
          numbers_at_age_female <- ss_mle[[scenario]]$natage |>
            dplyr::filter(`Beg/Mid` == 'M', Yr %in% period, Sex == 1) |>
            as.data.frame() |>
            dplyr::select(!c(Area, Bio_Pattern, Sex, BirthSeas, Settlement,
                             Platoon, Morph, Yr, Seas, Time, `Beg/Mid`, Era))
          weight_at_age_female <- ss_mle[[scenario]]$ageselex |>
            dplyr::filter(Fleet == selectivity_fleet, Yr == tail(period,1), Factor == 'bodywt', Sex == 1) |>
            dplyr::select(!c(Factor, Fleet, Yr, Seas, Sex, Morph, Label))
          VB_female <- as.matrix(numbers_at_age_female) %*% diag(weight_at_age_female) %*% as.matrix(sel_at_age_female |> t())

          sel_at_age_male <- ss_mle[[scenario]]$ageselex |>
            dplyr::filter(Fleet == selectivity_fleet, Yr == tail(period,1), Factor == 'Asel2', Sex == 2) |>
            dplyr::select(!c(Factor, Fleet, Yr, Sex, Label, Seas, Morph))
          numbers_at_age_male <- ss_mle[[scenario]]$natage |>
            dplyr::filter(`Beg/Mid` == 'M', Yr %in% period, Sex == 2) |>
            as.data.frame() |>
            dplyr::select(!c(Area, Bio_Pattern, Sex, BirthSeas, Settlement,
                             Platoon, Morph, Yr, Seas, Time, `Beg/Mid`, Era))
          weight_at_age_male <- ss_mle[[scenario]]$ageselex |>
            dplyr::filter(Fleet == selectivity_fleet, Yr == tail(period,1), Factor == 'bodywt', Sex == 2) |>
            dplyr::select(!c(Factor, Fleet, Yr, Seas, Sex, Morph, Label))
          VB_male <- as.matrix(numbers_at_age_male) %*% diag(weight_at_age_male) %*% as.matrix(sel_at_age_male |> t())

          if (dim(VB_male)[1] == 0) {VB <- VB_female} else {VB <- VB_female + VB_male}

          biomass <- data.frame(year = period, value = VB, scenario = scenario, upper = VB, lower = VB)
        }
      }

      data <- rbind(data, biomass)
    }
  }

  if (MCMC) {
    data <- data.frame()
    for (scenario in scenarios) {
      # if (missing(period)) {period = (ss_mle[[scenario]]$startyr+1) : (ss_mle[[scenario]]$endyr+1) }
      if (missing(period)) {period = (ss_mle[[1]]$startyr+1) : (ss_mle[[1]]$endyr+1) }

      if (biomass_type == "relative") {
        tmpdata <- ss_mcmc[[scenario]] |>
          dplyr::select(tidyselect::contains("Bratio_"))
      }

      if (biomass_type == "absolute") {
        tmpdata <- ss_mcmc[[scenario]] |>
          dplyr::select(tidyselect::contains("SSB_")) |>
          dplyr::select(-"SSB_Virgin",-"SSB_Initial",-"SSB_unfished",-"SSB_Btgt",-"SSB_SPR",-"SSB_MSY",-"B_MSY/SSB_unfished")
      }

      # Find median trajectory (median value of last year of Bratio)
      pos <- mcmc_median_position_SS(ss_mle,ss_mcmc)[scenario]

      # Generate data frame of all MCMC runs
      all_mcmc <- tmpdata |>
        dplyr::mutate(rownum = dplyr::row_number(), scenario = scenario) |>
        tidyr::pivot_longer(cols=-c(rownum, scenario), names_to = "year", values_to = "value") |>
        tidyr::separate(year, c("Label", "year"), sep="_") |>
        dplyr::mutate(year = as.numeric(year), value = as.numeric(value)) |>
        dplyr::mutate(med = "MCMC") |>
        dplyr::mutate(interval = NA, prob_lower=NA, prob_upper=NA) |>
        dplyr::select(rownum, scenario, year, value, med, interval, prob_lower, prob_upper)

      # Find trajectory that produces the median final biomass
      median_trajectory <- all_mcmc |>
        dplyr::mutate(med = ifelse(rownum==pos,"trajectory","MCMC"),
                      rownum = ifelse(rownum==pos,0,rownum))

      # Add median biomass
      median_annual_biomass <- all_mcmc |>
        # already grouped by scenario because of the loop
        dplyr::group_by(year) |>
        dplyr::summarise(value = quantile(value,probs=0.5),
                         .groups = 'drop') |>
        dplyr::mutate(rownum=0,
                      scenario = scenario,
                      med = "annual_biomass",
                      interval = as.numeric(NA),
                      prob_lower=as.numeric(NA),
                      prob_upper=as.numeric(NA)) |>
        dplyr::select(rownum, scenario, year, value, med, interval, prob_lower, prob_upper)


      # Banded plot prep
      intervals <- round(intervals,10)
      intervals_desc <- sort(intervals,decreasing = TRUE)

      quant_bio_lower <- apply(tmpdata,2,quantile, na.rm = TRUE, probs = (1-intervals)/2)
      quant_bio_upper <- apply(tmpdata,2,quantile, na.rm = TRUE, probs = 1-(1-intervals)/2)

      prob_lower <- quant_bio_lower |>
        t() |>
        as.data.frame() |>
        tibble::rownames_to_column("year") |>
        tidyr::separate(year, c("Label", "year"), sep="_") |>
        dplyr::mutate(year = as.numeric(year)) |>
        dplyr::select(-Label) |>
        dplyr::filter(year %in% period) |>
        tidyr::pivot_longer(-year, names_to="interval", values_to = "prob_lower") |>
        dplyr::mutate(interval = as.numeric(stringr::str_sub(interval, end = -2))/100) |>
        dplyr::mutate(interval = 1 - interval * 2) |>
        dplyr::mutate(interval = round(interval, 10))

      prob_upper <- quant_bio_upper |>
        t() |>
        as.data.frame() |>
        tibble::rownames_to_column("year") |>
        tidyr::separate(year, c("Label", "year"), sep="_") |>
        dplyr::mutate(year = as.numeric(year)) |>
        dplyr::select(-Label) |>
        dplyr::filter(year %in% period) |>
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

      # Filter to exclude projection
      scenario_data <- rbind(median_trajectory, median_annual_biomass, band_data) |>
        dplyr::filter(year %in% period)

      data <- data |> rbind(scenario_data)

    }
  }

  data$biomass_type <- biomass_type
  data$biomass_definition <- biomass_definition

  rownames(data) <- NULL
  return(data)
}
