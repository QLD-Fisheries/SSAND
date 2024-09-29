# Copyright 2024 Fisheries Queensland

# This file is part of SSAND.
# SSAND is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
# SSAND is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
# You should have received a copy of the GNU General Public License along with SSAND. If not, see <https://www.gnu.org/licenses/>.

#' Weight plot
#'
#' @param data Output from weightplot_prep(). A dataframe with xvar (int), weight (num), scenario (int), sex (chr)
#' @param xlab Label for x-axis (character). Default is "Year".
#' @param ylab Label for y-axis (character). Default is "Retained catch (t)".
#' @param colours A vector of colours used (character).
#' @param scenarios A vector of scenarios to plot (numeric). Shows all scenarios if left blank. Can be overridden in the plotting function.
#' @param scenario_labels A vector of customised scenario names (character). Default is "Scenario 1", "Scenario 2", etc.
#' @param scenario_order A vector to reorder how scenarios are displayed (character). Use the label names defined in "scenario_labels".
#' If "scenario_labels" is left blank, the labels will be "Scenario 1", "Scenario 2" etc.
#' Any scenarios not included in "scenario_order" will be tacked on in the order they appear in the input data.
#' @param scales Scales for ggplot2::facet_wrap(). Default is 'free', see ?ggplot2::facet_wrap for options.
#' @param ncol Number of columns for facet_wrap(). Default is 2.
#'
#' @return Weight plot
#' @export
#'
#' @examples
#' data <- weightplot_prep_SS(ss_mle)
#' weightplot(data)
weightplot <- function(data,
                       xlab = "Length (cm)",
                       ylab = "Mean weight (kg) in last year",
                       colours = NULL,
                       scenarios = NULL,
                       scenario_labels = NULL,
                       scenario_order = NULL,
                       scales = 'free',
                       ncol = 2) {

  # Data input warnings
  if (!"xvar" %in% names(data)) {warning("Input data is missing xvar column")}
  if (!"weight" %in% names(data)) {warning("Input data is missing weight column")}
  if (!"scenario" %in% names(data)) {warning("Input data is missing scenario column")}
  if (!"sex" %in% names(data)) {warning("Input data is missing sex column")}

  if (!missing(scenarios)){data <- data |> dplyr::filter(scenario %in% scenarios)}

  if (missing(scenario_labels)) {
    data <- data |> dplyr::mutate(scenario_labels = as.factor(paste0("Scenario ",scenario)))
  } else {
    scenario.lookup<- data.frame(scenario = unique(data$scenario), scenario_labels = scenario_labels)
    data <- data |>
      dplyr::left_join(scenario.lookup, by = "scenario") |>
      dplyr::mutate(scenario_labels = as.factor(scenario_labels))
  }

  if (!missing(scenario_order)) {
    # Add on any scenarios not included in the scenario_order list
    scenario_order = c(scenario_order, setdiff(scenario_labels, scenario_order))
    # Reorder scenarios
    data$scenario_labels <- factor(data$scenario_labels, levels = scenario_order)
  }

  if (missing(colours)) {colours <- "black"}


  p <- ggplot2::ggplot(data, ggplot2::aes(x=xvar, y=weight), colour=colours) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::xlab(xlab) +
    ggplot2::ylab(ylab) +
    ggplot2::theme_bw()

  if (length(unique(data$scenario))>1){
    p <- p +
      ggplot2::facet_wrap(~scenario_labels, scales = scales, ncol = ncol)
  }

  return(p)


}
