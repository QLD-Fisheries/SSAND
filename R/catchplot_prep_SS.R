# Copyright 2024 Fisheries Queensland

# This file is part of SSAND.
# SSAND is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
# SSAND is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
# You should have received a copy of the GNU General Public License along with SSAND. If not, see <https://www.gnu.org/licenses/>.

#' Prepare Stock Synthesis data for catchplot()
#'
#' @param ss_mle A list of outputs from r4ss::SS_output() with one element per scenario. Will automatically reformat as a list if a single r4ss::SS_output() output (i.e. one scenario) is entered.
#' @param scenarios A vector of scenarios to plot (numeric). Shows all scenarios if left blank. Can be overridden in the plotting function.
#' @param partition Type of catch to plot. Options are "selected" (all catch including retained and discarded), "dead" (retained catch plus dead discarded) and "retained" (retained catch ignoring discards)
#'
#' @return A dataframe with date, value, sector, scenario
#' @export
#'
#' @examples
#' data <- catchplot_prep_SS(ss_mle)
#' catchplot(data, scenarios=1)
catchplot_prep_SS <- function(ss_mle,
                              scenarios = NULL,
                              partition = "retained"){

  if (check_scenarios(ss_mle,"SS","MLE")=="single scenario"){ss_mle <- list(ss_mle); warning("Assuming you are entering a single scenario, not a list of scenarios. Wrap ss_mle input inside a list() to avoid this warning.")}
  if (missing(scenarios)){scenarios <- 1:length(ss_mle)}

  if (!partition %in% c("retained","selected","dead")) {stop("Please enter a valid entry for 'partition'. Options are 'selected', 'dead' or 'retained'.")}
  if (partition=="selected") {partition = "sel"}
  if (partition=="retained") {partition = "retain"}
  # if (partition=="dead") {partition = "dead"}

  data <- data.frame()
  for (scenario in scenarios) {
    tmp <- ss_mle[[scenario]]$timeseries |>
      dplyr::filter(Yr >= ss_mle[[scenario]]$startyr & Yr <= ss_mle[[scenario]]$endyr) |>
      dplyr::mutate(date = as.Date(paste0('01/01/',Yr), format = '%d/%m/%Y'))  |>
      dplyr::select(date, contains(paste0(partition,"(B)"))) |>
      tidyr::pivot_longer(cols = !date, names_to = c("name", "fleet"), names_sep = "_",  values_to = "value") |>
      dplyr::mutate(fleet = as.factor(fleet)) |>
      dplyr::mutate(scenario = scenario, partition = partition) |>
      dplyr::select(date, value, fleet, scenario, partition)
    data <- rbind(data,tmp)
  }
  rownames(data) <- NULL
  return(data)
}
