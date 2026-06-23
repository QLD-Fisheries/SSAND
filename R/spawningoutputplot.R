# Copyright 2024 Fisheries Queensland

# This file is part of SSAND.
# SSAND is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
# SSAND is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
# You should have received a copy of the GNU General Public License along with SSAND. If not, see <https://www.gnu.org/licenses/>.

#' Spawning output plot
#'
#' @param data Output from spawningoutputplot_prep(). A dataframe with xvar (int), maturityxfecundity (num), x (chr), scenario (int)
#' @param xaxis The variable you would like to appear on the x-axis. Options are "age" or "length"
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
#' @param text_size,legend_text_size,text_colour,legend_text_colour,legend_position,legend_box,legend_title_blank,panel_border,panel_border_colour,xangle Optional plotting theme overrides. Defaults are controlled by `theme_ssand()`
#'   and can be set globally via `set_ssand_style()`.
#'
#' @return Spawning output plot
#' @export
#'
#' @examples
#' data <- spawningoutputplot_prep_SS(ss_mle)
#' spawningoutputplot(data,xaxis="length")
spawningoutputplot <- function(data,
                               xaxis = "age", # length
                               xlab = "Age (years)",
                               ylab = "Spawning output",
                               colours = "black",
                               scenarios = NULL,
                               scenario_labels = NULL,
                               scenario_order = NULL,
                               scales = 'free',
                               ncol = 2,
                               xangle = NULL,
                               legend_position = NULL,
                               text_size = NULL,
                               legend_text_size = NULL,
                               text_colour = NULL,
                               legend_text_colour = NULL,
                               legend_box = NULL,
                               legend_title_blank = NULL,
                               panel_border = NULL,
                               panel_border_colour = NULL) {

  check_data_columns(data, c("xvar","maturityxfecundity","x","scenario"))

  data <- data |> dplyr::filter(x %in% xaxis)

  if (missing(xlab) & xaxis=="length") {xlab="Length (cm)"}

  data <- apply_scenarios(data, scenarios, scenario_labels, scenario_order)

  p <- ggplot2::ggplot(data, ggplot2::aes(x=xvar, y=maturityxfecundity)) +
    ggplot2::geom_line(colour=colours) +
    ggplot2::geom_point(colour=colours) +
    ggplot2::xlab(xlab) +
    ggplot2::ylab(ylab)

  p <- add_scenario_facets(p, data, scales = scales, ncol = ncol)

  # ___________________
  # Axes and theme
  # ___________________
  p <- add_ssand_theme(p,
                       text_size = text_size,
                       legend_text_size = legend_text_size,
                       text_colour = text_colour,
                       legend_text_colour = legend_text_colour,
                       legend_position = legend_position,
                       legend_box = legend_box,
                       legend_title_blank = legend_title_blank,
                       panel_border = panel_border,
                       panel_border_colour = panel_border_colour,
                       xangle = xangle)

  return(p)
}
