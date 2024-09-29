# Copyright 2024 Fisheries Queensland

# This file is part of SSAND.
# SSAND is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
# SSAND is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
# You should have received a copy of the GNU General Public License along with SSAND. If not, see <https://www.gnu.org/licenses/>.

#' Calculate report values from DDUST output
#'
#' @param dd_mle A list of outputs from DDUST::makefullreport() with one list element per scenario. Will automatically reformat as a list if a single DDUST::makefullreport() output (i.e. one scenario) is entered
#' @param dd_mcmc Output from mcmc_ensemble_DD()$dd_mcmc. A list that combines all outputs from tmbstan::tmbstan(). Only needed if MCMC was used.
#' @param dd_sim Output from mcmc_ensemble_DD()$dd_sim. A list that combines all outputs from DDUST::simulate_DD(). Only required if MCMC was used.
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
#' @param F_relative_to_MSY Set to TRUE to report F relative to FMSY
#'
#' @return A list containing named values that could be useful in reporting
#' @export
#'
#' @examples
#' catch <- rbind(data.frame(year = 1953:2022, sector = "Commercial",
#'                  value = c(602,674,561,472,436,480,370,404,390,277)),
#'                data.frame(year = 1953:2022, sector = "Recreational",
#'                  value = c(750,705,570,535,495,390,506,448,383,303)))
#' data <- report_values_DD(dd_mle,suffix="_MLE")
#' \dontrun{
#' library(DDUST)
#' dd_sim <- simulate_DDUST(dd_mle,dd_mcmc)
#' dd_mcmc_ens <- mcmc_ensemble_DD(dd_mcmc,dd_sim,scenarios=c(1,2))$dd_mcmc
#' dd_sim_ens <- mcmc_ensemble_DD(dd_mcmc,dd_sim,scenarios=c(1,2))$dd_sim
#'
#' data <- report_values_DD(dd_mle,dd_mcmc_ens,dd_sim_ens,catch=catch)
#'
#' data <- report_values_DD(dd_mle,dd_mcmc_ens,dd_sim_ens,
#'                              prefix="DD_",suffix="_1")
#' }
report_values_DD <- function(dd_mle,
                             dd_mcmc = NULL,
                             dd_sim = NULL,
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
                             F_relative_to_MSY = TRUE
) {

  if (missing(dd_mcmc)) {MCMC = FALSE} else {MCMC = TRUE}
  if (check_scenarios(dd_mle,"DD","MLE")=="list of scenarios") {dd_mle <- list(dd_mle[[base_case]])}
  if (MCMC && check_scenarios(dd_mcmc,"DD","MCMC")=="list of scenarios") {dd_mcmc <- list(dd_mcmc[[base_case]])}
  if (MCMC && check_scenarios(dd_sim,"DD","SIM")=="list of scenarios") {dd_sim <- list(dd_sim[[base_case]])}

  if (missing(end_year)) {end_year <- dd_mle[[1]]$data$last_year_catch}

  data <- list()

  if (!MCMC) {
    # Biomass ----
    biomass_df <- biomassplot_prep_DD(dd_mle)

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

    Fdata <- Fplot_prep_DD(dd_mle) |>  dplyr::filter(year %in% F_years)

    data$F_summary_lower <- round(mean(Fdata$lower),round_F_summary)
    data$F_summary_upper <- round(mean(Fdata$upper),round_F_summary)
    data$F_summary_mean  <- round(mean(Fdata$value),round_F_summary)

    # Biomass at MSY (% of unfished) ----
    data$BMSY_med <- dd_mle[[1]]$Bmsy[[1]] |> round(2)
    data$BMSY_low <- (dd_mle[[1]]$Bmsy[[1]] - 1.96*dd_mle[[1]]$Bmsy_sd[[1]]) |> round(2)
    data$BMSY_upp <- (dd_mle[[1]]$Bmsy[[1]] + 1.96*dd_mle[[1]]$Bmsy_sd[[1]]) |> round(2)

    # MSY (dead catch) (t/yr) ----
    data$MSY_med <-  dd_mle[[1]]$msy[[1]] |> round()
    data$MSY_low <- (dd_mle[[1]]$msy[[1]] - 1.96*dd_mle[[1]]$msy_sd[[1]]) |> round()
    data$MSY_upp <- (dd_mle[[1]]$msy[[1]] + 1.96*dd_mle[[1]]$msy_sd[[1]]) |> round()

    # MSY (dead catch) (t/yr) ----
    data$FMSY_med <-  dd_mle[[1]]$Fmsy[[1]] |> round(round_FMSY)
    data$FMSY_low <- (dd_mle[[1]]$Fmsy[[1]] - 1.96*dd_mle[[1]]$Fmsy_sd[[1]]) |> round(round_FMSY)
    data$FMSY_upp <- (dd_mle[[1]]$Fmsy[[1]] + 1.96*dd_mle[[1]]$Fmsy_sd[[1]]) |> round(round_FMSY)
  }

  if (MCMC) {
    credible_upper <- 1-(1-credible_interval)/2
    credible_lower <- (1-credible_interval)/2

    # Biomass ----
    biomass_final_df <- mcmc_finalbiomassposterior_prep_DD(dd_mle,
                                                           dd_mcmc,
                                                           dd_sim,
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

      biomass_minimum_df <- biomassplot_prep_DD(dd_mle,
                                                dd_mcmc,
                                                dd_sim,
                                                biomass_type = biomass_type) |>
        dplyr::filter(med=="MCMC") |>
        dplyr::filter(value == min(value)) |>
        unique() # in case of multiple iterations producing the same minimum

      data$biomass_min_year <- biomass_minimum_df$year
      data$biomass_min_value <- round(biomass_minimum_df$value*100)
    }

    if (biomass_minimum == "median") {
      biomass_minimum_df <- biomassplot_prep_DD(dd_mle,
                                                dd_mcmc,
                                                dd_sim,
                                                biomass_type = biomass_type) |>
        dplyr::filter(med=="MCMC") |>
        dplyr::group_by(year) |>
        dplyr::summarise(value = median(value), .groups='drop') |>
        dplyr::filter(value == min(value))

      data$biomass_min_year <- biomass_minimum_df$year
      data$biomass_min_value <- round(biomass_minimum_df$value*100)
    }

    # Fishing mortality ----
    F_years <- (end_year-number_F_years):(end_year-1)

    f_summary_data <- Fplot_prep_DD(dd_mle,
                                    dd_mcmc,
                                    dd_sim,
                                    CI_range = credible_interval,
                                    F_relative_to_MSY = TRUE) |>
      dplyr::filter(med=="MCMC", year %in% F_years) |>
      dplyr::summarise(F = mean(value), .by=rownum)

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


    f_summary_data_MSY <- Fplot_prep_DD(dd_mle,
                                    dd_mcmc,
                                    dd_sim,
                                    CI_range = credible_interval,
                                    F_relative_to_MSY = TRUE) |>
      dplyr::filter(med=="MCMC", year %in% F_years) |>
      dplyr::summarise(F = mean(value), .by=rownum)

    f_summary_risk <- f_summary_data_MSY |>
      dplyr::mutate(above = `F` > 1.0) |>
      dplyr::select(above) |>
      purrr::as_vector() |>
      sum()

    f_summary_risk <- f_summary_risk / nrow(f_summary_data)
    data$F_summary_risk <- round(f_summary_risk*100)

    # Biomass at MSY (% of unfished) ----
    Bmsy <- unlist(lapply(dd_sim[[1]], function(x) x[["Bmsy"]]))
    data$BMSY_med <- quantile(Bmsy, probs = 0.5,             na.rm=TRUE)[[1]] |> round(2)
    data$BMSY_low <- quantile(Bmsy, probs = credible_lower , na.rm=TRUE)[[1]] |> round(2)
    data$BMSY_upp <- quantile(Bmsy, probs = credible_upper , na.rm=TRUE)[[1]] |> round(2)

    # MSY (dead catch) (t/yr) ----
    msy <- unlist(lapply(dd_sim[[1]], function(x) x[["msy"]]))
    data$MSY_med <- quantile(msy, probs = 0.5,             na.rm=TRUE)[[1]] |> round()
    data$MSY_low <- quantile(msy, probs = credible_lower , na.rm=TRUE)[[1]] |> round()
    data$MSY_upp <- quantile(msy, probs = credible_upper , na.rm=TRUE)[[1]] |> round()


    # FMSY (yr−1) ----
    Fmsy <- unlist(lapply(dd_sim[[1]], function(x) x[["Fmsy"]]))
    data$FMSY_med <- quantile(Fmsy, probs = 0.5,             na.rm=TRUE)[[1]] |> round(round_FMSY)
    data$FMSY_low <- quantile(Fmsy, probs = credible_lower , na.rm=TRUE)[[1]] |> round(round_FMSY)
    data$FMSY_upp <- quantile(Fmsy, probs = credible_upper , na.rm=TRUE)[[1]] |> round(round_FMSY)
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
  data$end_year_catch <- end_year
  data$start_year_catch <- dd_mle[[1]]$data$first_year_catch
  data$start_year_biomass <- dd_mle[[1]]$data$first_year_catch

  return(data)
}

