# Copyright 2024 Fisheries Queensland

# This file is part of SSAND.
# SSAND is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
# SSAND is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
# You should have received a copy of the GNU General Public License along with SSAND. If not, see <https://www.gnu.org/licenses/>.

#' Calculate report values from Stock Synthesis output
#'
#' @param ss_mle A list of outputs from r4ss::SS_output() with one element per scenario. Will automatically reformat as a list if a single r4ss::SS_output() output (i.e. one scenario) is entered.
#' @param ss_mcmc Output from mcmc_ensemble_SS(). A list that combines all outputs from r4ss::SSgetMCMC(). Only needed if MCMC was used.
#' @param base_case The representative base case or reference scenario from ss_mle. Defaults to 1.
#' @param prefix Optional string to add to the beginning of variable names within data output. Useful if extracting values for multiple models.
#' @param suffix Optional string to add to the end of variable names within data output. Useful if extracting values for multiple models.
#' @param catch A data frame of catch, with columns year (num), value (num) and sector (chr)
#' @param end_year Optional adjustment for biomass end year. For example, if catch is entered until 2022, end_year is 2023.
#' @param credible_interval Optional adjustment for credible interval. Default is 0.95.
#' @param number_catch_years Number of recent years to average catch. Default is 5.
#' @param number_F_years Number of recent years to average fishing pressure. Default is 5.
#' @param biomass_minimum Method for calculating minimum biomass for MCMC results.
#' "any" takes the minimum value from any MCMC iteration for any year
#' "median" takes the minimum value from the annual median biomass for any year
#' @param round_F_summary Number of decimal places to round F data for summary table
#' @param round_FMSY Number of decimal places to round F_MSY data
#' @param biomass_type The type of biomass used. Options are "relative" or "absolute" (character).
#' @param biomass_definition The definition of biomass used. Options are "spawning" or "vulnerable" (character).
#' @param selectivity_fleet Fleet number used for the selectivity applied to calculate vulnerable biomass.
#'
#' @return A list containing named values that could be useful in reporting
#' @export
#'
#' @examples
#' catch <- rbind(data.frame(year = 1953:2022, sector = "Commercial",
#'                  value = c(602,674,561,472,436,480,370,404,390,277)),
#'                data.frame(year = 1953:2022, sector = "Recreational",
#'                  value = c(750,705,570,535,495,390,506,448,383,303)))
#'
#' ss_mcmc_ens <- mcmc_ensemble_SS(ss_mcmc)
#'
#' data <- report_values_SS(ss_mle,ss_mcmc_ens,catch=catch)
#' data <- report_values_SS(ss_mle,ss_mcmc_ens,prefix="SS_",suffix="_1")
#' data <- report_values_SS(ss_mle,suffix="_MLE")
report_values_SS <- function(ss_mle,
                             ss_mcmc = NULL,
                             base_case = 1,
                             catch = NULL,
                             number_catch_years = 5,
                             prefix = NULL,
                             suffix = NULL,
                             end_year = NULL,
                             credible_interval = 0.95,
                             number_F_years = 5,
                             biomass_minimum = "any",
                             round_F_summary = 2,
                             round_FMSY = 3,
                             biomass_type = "relative",
                             biomass_definition = "spawning",
                             selectivity_fleet = 2
) {

  if (missing(ss_mcmc)) {MCMC = FALSE} else {MCMC = TRUE}
  if (check_scenarios(ss_mle,"SS","MLE")=="list of scenarios") {ss_mle <- list(ss_mle[[base_case]])}
  if (missing(end_year)) {end_year <- ss_mle[[1]]$endyr+1}

  data <- list()

  if (!MCMC) {
    # Biomass ----
    biomass_df <- biomassplot_prep_SS(ss_mle,
                                      biomass_type = biomass_type,
                                      biomass_definition = biomass_definition,
                                      selectivity_fleet = selectivity_fleet)

    biomass_final_df <- biomass_df |>
      dplyr::filter(year==end_year)

    data$biomass_summary       <- round(biomass_final_df$value*100)
    data$biomass_summary_upper <- round(biomass_final_df$upper*100)
    data$biomass_summary_lower <- round(biomass_final_df$lower*100)

    biomass_min_value <- min(biomass_df$value)

    data$biomass_min_year <- biomass_df |>
      dplyr::filter(value == biomass_min_value) |>
      dplyr::select(year) |>
      dplyr::pull()

    data$biomass_min_value <- round(biomass_min_value*100)

    # Fishing mortality ----
    F_years <- (end_year-number_F_years):(end_year-1)

    Fdata <- Fplot_prep_SS(ss_mle) |>  dplyr::filter(year %in% F_years)

    data$F_summary_lower <- round(mean(Fdata$lower),round_F_summary)
    data$F_summary_upper <- round(mean(Fdata$upper),round_F_summary)
    data$F_summary_mean  <- round(mean(Fdata$value),round_F_summary)

    # Biomass at MSY (% of unfished) ----
    BMSY <- ss_mle[[1]]$derived_quants |>
      dplyr::filter(Label == "B_MSY/SSB_unfished")

    data$BMSY_med <- BMSY |> dplyr::select(Value) |> dplyr::pull() |> round(2)
    data$BMSY_low <- BMSY |> dplyr::mutate(Value=Value-1.96*StdDev) |> dplyr::select(Value) |> dplyr::pull() |> round(2)
    data$BMSY_upp <- BMSY |> dplyr::mutate(Value=Value+1.96*StdDev) |> dplyr::select(Value) |> dplyr::pull() |> round(2)

    # MSY (dead catch) (t/yr) ----
    MSY <- ss_mle[[1]]$derived_quants |>
      dplyr::filter(Label == "Dead_Catch_MSY")

    data$MSY_med <- MSY |> dplyr::mutate(Value=round(Value)) |> dplyr::select(Value) |> dplyr::pull()
    data$MSY_low <- MSY |> dplyr::mutate(Value=round(Value-1.96*StdDev)) |> dplyr::select(Value) |> dplyr::pull()
    data$MSY_upp <- MSY |> dplyr::mutate(Value=round(Value+1.96*StdDev)) |> dplyr::select(Value) |> dplyr::pull()

    # MSY (dead catch) (t/yr) ----
    FMSY <- ss_mle[[1]]$derived_quants |>
      dplyr::filter(Label == "annF_MSY")

    data$FMSY_med <- FMSY |> dplyr::mutate(Value=round(Value,round_FMSY)) |> dplyr::select(Value) |> dplyr::pull()
    data$FMSY_low <- FMSY |> dplyr::mutate(Value=round(Value-1.96*StdDev,round_FMSY)) |> dplyr::select(Value) |> dplyr::pull()
    data$FMSY_upp <- FMSY |> dplyr::mutate(Value=round(Value+1.96*StdDev,round_FMSY)) |> dplyr::select(Value) |> dplyr::pull()
  }

  if (MCMC) {
    credible_upper <- 1-(1-credible_interval)/2
    credible_lower <- (1-credible_interval)/2

    # Biomass ----
    biomass_final_df <- mcmc_finalbiomassposterior_prep_SS(ss_mle,
                                                           ss_mcmc,
                                                           credible_interval = credible_interval,
                                                           end_year = end_year)

    data$biomass_summary_lower <- round(biomass_final_df$quant_lower)
    data$biomass_summary_upper <- round(biomass_final_df$quant_upper)
    data$biomass_median <- round(biomass_final_df$median)
    data$biomass_risk20 <- biomass_final_df$risk[1]
    data$biomass_risk20_40 <- biomass_final_df$risk[2]
    data$biomass_risk40_60 <- biomass_final_df$risk[3]
    data$biomass_risk60 <- biomass_final_df$risk[4]

    if (biomass_minimum == "any") {
      biomass_minimum_df <- ss_mcmc[[1]] |>
        dplyr::select(starts_with("Bratio_")) |>
        dplyr::mutate(iteration=dplyr::row_number()) |>
        tidyr::pivot_longer(!iteration, names_to = "YearChar") |>
        dplyr::mutate(year = as.numeric(substr(YearChar, 8, 11))) |>
        dplyr::filter(year <= end_year)

      biomass_min_value <- min(biomass_minimum_df$value)

      data$biomass_min_year <- biomass_minimum_df |>
        dplyr::filter(value == biomass_min_value) |>
        dplyr::select(year) |>
        dplyr::pull()

      data$biomass_min_value <- round(biomass_min_value*100)
    }

    if (biomass_minimum == "median") {
      biomass_minimum_df <- ss_mcmc[[1]] |>
        dplyr::select(starts_with("Bratio_")) |>
        dplyr::mutate(iteration=dplyr::row_number()) |>
        tidyr::pivot_longer(!iteration, names_to = "YearChar") |>
        dplyr::mutate(year = as.numeric(substr(YearChar, 8, 11))) |>
        dplyr::filter(year <= end_year) |>
        dplyr::group_by(year) |>
        dplyr::summarise(value = median(value), .groups='drop')

      biomass_min_value <- min(biomass_minimum_df$value)

      data$biomass_min_year <- biomass_minimum_df |>
        dplyr::filter(value == biomass_min_value) |>
        dplyr::select(year) |>
        dplyr::pull()

      data$biomass_min_value <- round(biomass_min_value*100)
    }

    # Fishing mortality ----
    F_years <- (end_year-number_F_years):(end_year-1)


    f_summary_data <- ss_mcmc[[1]] |>
      dplyr::select(starts_with("F_")) |>
      dplyr::mutate(iteration=dplyr::row_number()) |>
      tidyr::pivot_longer(!iteration, names_to = "YearChar") |>
      dplyr::mutate(year = as.numeric(substr(YearChar, 3, 6))) |>
      dplyr::filter(year %in% F_years) |>
      dplyr::summarise(F = mean(value),.by = iteration)

    f_summary_lower <- f_summary_data |>
      dplyr::select(F) |>
      purrr::as_vector() |>
      quantile(credible_lower)
    data$F_summary_lower <- round(f_summary_lower[[1]],round_F_summary)

    f_summary_upper <- f_summary_data |>
      dplyr::select(F) |>
      purrr::as_vector() |>
      quantile(credible_upper)
    data$F_summary_upper <- round(f_summary_upper[[1]],round_F_summary)

    f_summary_med <- f_summary_data |>
      dplyr::select(F) |>
      purrr::as_vector() |>
      quantile(0.5)
    data$F_summary_med <- round(f_summary_med[[1]],round_F_summary)

    if (ss_mle[[1]]$F_report_basis == "(F)/(Fmsy);_with_F=Exploit(bio)") {
      f_summary_risk <- f_summary_data |>
        dplyr::mutate(above = `F` > 1.0) |>
        dplyr::select(above) |>
        purrr::as_vector() |>
        sum()

      f_summary_risk <- f_summary_risk / nrow(f_summary_data)
      data$F_summary_risk <- round(f_summary_risk*100)
    } else {
      warning("Skipping calculation of probability of F exceeding FMSY because F_report_basis was not set to 2 in the starter file.")
    }


    # Biomass at MSY (% of unfished) ----
    data$BMSY_med <- round(quantile(ss_mcmc[[1]] |> dplyr::select(`B_MSY/SSB_unfished`) |> dplyr::pull(), probs = 0.5)[[1]]*100,0)
    data$BMSY_low <- round(quantile(ss_mcmc[[1]] |> dplyr::select(`B_MSY/SSB_unfished`) |> dplyr::pull(), probs = credible_lower)[[1]]*100,0)
    data$BMSY_upp <- round(quantile(ss_mcmc[[1]] |> dplyr::select(`B_MSY/SSB_unfished`) |> dplyr::pull(), probs = credible_upper)[[1]]*100,0)

    # MSY (dead catch) (t/yr) ----
    data$MSY_med <- round(quantile(ss_mcmc[[1]] |> dplyr::select(Dead_Catch_MSY) |> dplyr::pull(), probs = 0.5)[[1]],0)
    data$MSY_low <- round(quantile(ss_mcmc[[1]] |> dplyr::select(Dead_Catch_MSY) |> dplyr::pull(), probs = credible_lower)[[1]],0)
    data$MSY_upp <- round(quantile(ss_mcmc[[1]] |> dplyr::select(Dead_Catch_MSY) |> dplyr::pull(), probs = credible_upper)[[1]],0)

    # FMSY (yr−1) ----
    data$FMSY_med <- round(quantile(ss_mcmc[[1]] |> dplyr::select(annF_MSY) |> dplyr::pull(), probs = 0.5)[[1]], round_FMSY)
    data$FMSY_low <- round(quantile(ss_mcmc[[1]] |> dplyr::select(annF_MSY) |> dplyr::pull(), probs = credible_lower)[[1]], round_FMSY)
    data$FMSY_upp <- round(quantile(ss_mcmc[[1]] |> dplyr::select(annF_MSY) |> dplyr::pull(), probs = credible_upper)[[1]], round_FMSY)
  }

  if (!missing(catch)) {
    # Catch ----
    catch_years <- (end_year-number_catch_years):(end_year-1)

    catch_5years <- catch |>
      dplyr::filter(year %in% catch_years) |>
      dplyr::summarise(value = round(sum(value)/number_catch_years)) |>
      dplyr::pull()

    catch_5years_value <- stats::setNames(catch_5years, paste0("catch_",number_catch_years,"year_average"))     # Create a named vector from the dataframe
    data <- c(data,as.list(catch_5years_value))     # Convert the named vector to a list


    # Average catch from the last X years in each sector
    catch_5years_sector <- catch |>
      dplyr::filter(year %in% catch_years) |>
      dplyr::group_by(sector) |>
      dplyr::summarise(value = round(sum(value)/number_catch_years))

    catch_5years_sector_values <- stats::setNames(catch_5years_sector$value, paste0("catch_",number_catch_years,"year_average_", catch_5years_sector$sector))
    data <- c(data,as.list(catch_5years_sector_values))

    # Percentage split of catch from the last X years in each sector
    catch_5years_sector_percent <- catch_5years_sector |>
      dplyr::mutate(sum = sum(value),
                    value = round(value/sum*100)) |>
      dplyr::select(-sum)

    catch_5years_sector_percent_values <- stats::setNames(catch_5years_sector_percent$value, paste0("catch_",number_catch_years,"year_average_percent_", catch_5years_sector_percent$sector))
    data <- c(data,as.list(catch_5years_sector_percent_values))
  }

  if (!missing(prefix)) {
    # Function to add prefix
    add_prefix <- function(name, pref) {paste0(pref, name)}
    # Add prefix to all names in the list
    data <- stats::setNames(data, sapply(names(data), add_prefix, pref = prefix))
  }

  if (!missing(suffix)) {
    # Function to add prefix
    add_suffix <- function(name, suff) {paste0(name, suff)}
    # Add prefix to all names in the list
    data <- stats::setNames(data, sapply(names(data), add_suffix, suff = suffix))
  }

  data$end_year <- end_year
  data$end_year_catch <- end_year - 1
  data$start_year_catch <- ss_mle[[1]]$startyr
  data$start_year_biomass <- ss_mle[[1]]$startyr + 1

  return(data)
}

