# Copyright 2024 Fisheries Queensland

# This file is part of SSAND.
# SSAND is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
# SSAND is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
# You should have received a copy of the GNU General Public License along with SSAND. If not, see <https://www.gnu.org/licenses/>.

#' Prepare DDUST data for dataplot()
#'
#' @param dd_mle A list of outputs from DDUST::makefullreport() with one list element per scenario. Will automatically reformat as a list if a single DDUST::makefullreport() output (i.e. one scenario) is entered
#' @param cpue_circle_size Size of cpue circles (default 1)
#' @param absolute_biomass_circle_size Size of absolute biomass circles (default 1)
#' @param retained_catch_name Label for catch input (default "Retained catch")
#' @param abundance_indices_name Label for abundance index (default "Abundance indices")
#' @param financial_year Set to TRUE if the assessment was based on financial year (logical).
#' @param scenarios A vector of scenarios to plot (numeric). Shows all scenarios if left blank. Can be overridden in the plotting function.
#'
#' @return A data object with variables called yr (int/num), typename (factor), size (numeric), fleetnames (factor)
#' @export
#'
#' @examples
#' data <- dataplot_prep_DD(dd_mle)
#' dataplot(data)
dataplot_prep_DD <- function(dd_mle,
                             scenarios = 1,
                             cpue_circle_size = 1,
                             absolute_biomass_circle_size = 1,
                             retained_catch_name = "Retained catch",
                             abundance_indices_name = "Abundance indices",
                             financial_year = FALSE) {

  if (check_scenarios(dd_mle,"DD","MLE")=="single scenario"){dd_mle <- list(dd_mle)}
  # if (missing(scenarios)){scenarios <- 1:length(dd_mle)}

  data_all <- data.frame()
  for (scenario in scenarios) {
    harvest_data <- data.frame(year = sort(rep(dd_mle[[scenario]]$data$first_year_catch:dd_mle[[scenario]]$data$last_year_catch,12/dd_mle[[scenario]]$data$Number_months_per_timestep)),
                               month = seq(1,12,dd_mle[[scenario]]$data$Number_months_per_timestep),
                               value = dd_mle[[scenario]]$data$ctch/1000) |>
      dplyr::mutate(date = as.Date(paste0('01/',month,'/',year), format = '%d/%m/%Y')) |>
      dplyr::select(!c(year, month)) |>
      dplyr::mutate(year = as.numeric(format(date,"%Y"))) |>
      dplyr::group_by(year) |>
      dplyr::summarise(value = sum(value), .groups = 'drop') |>
      dplyr::mutate(date = as.Date(paste0('01/01/',year), format = '%d/%m/%Y')) |>
      dplyr::mutate(yr = year, typename = retained_catch_name, size = value/mean(value), fleet = 1) |>
      dplyr::select(yr, typename, size, fleet)

    cpue_data <- cpueplot_prep_DD(dd_mle) |>
      dplyr::mutate(year = as.numeric(format(date,"%Y"))) |>
      dplyr::group_by(year, fleet) |>
      dplyr::summarise(cpue = mean(obs, na.rm=TRUE), .groups = 'drop') |>
      dplyr::mutate(date = as.Date(paste0('01/01/',year), format = '%d/%m/%Y')) |>
      dplyr::filter(!is.na(cpue)) |>
      dplyr::mutate(yr=year, typename=abundance_indices_name, size=cpue_circle_size, fleet = 1) |>
      dplyr::select(yr, typename, size, fleet)

    data <- harvest_data |> rbind(cpue_data)

    if (sum(dd_mle[[scenario]]$data$absolute_biomass) > 0) {
      abs_biomass_data <- absolutebiomassplot_prep_DD(dd_mle) |>
        dplyr::mutate(year = as.numeric(format(date,"%Y"))) |>
        dplyr::group_by(year, fleet) |>
        dplyr::summarise(absolute_biomass = sum(obs, na.rm=TRUE), .groups = 'drop') |>
        dplyr::filter(absolute_biomass > 0) |>
        dplyr::mutate(date = as.Date(paste0('01/01/',year), format = '%d/%m/%Y')) |>
        dplyr::mutate(yr=year, typename="Absolute biomass", size=absolute_biomass_circle_size, fleet = 1) |>
        dplyr::select(yr, typename, size, fleet)

      data <- data |> rbind(abs_biomass_data)
    }

    if (financial_year==T) {
      data$yr <- data$yr + 1
    }

    data <- data |>
      dplyr::mutate(scenario = scenario,
                    fleet = as.factor(fleet)) |>
      dplyr::select(year = yr, typename, size, fleet, scenario)

    data_all <- rbind(data_all, data)
  }
  data <- data_all
  rownames(data) <- NULL
  return(data)
}
