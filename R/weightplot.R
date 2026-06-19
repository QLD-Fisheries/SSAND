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
#' @param show_two_sex Default is FALSE. Set to TRUE to show two sexes on the plot (logical).
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
                       colours = "black",
                       scenarios = NULL,
                       scenario_labels = NULL,
                       scenario_order = NULL,
                       show_two_sex = FALSE,
                       scales = 'free',
                       ncol = 2) {

  # ___________________
  # Data validation
  # ___________________

  check_data_columns(data, c("xvar","weight","scenario","sex"))

  # ___________________
  # Custom to this plot
  # ___________________


  # ___________________
  # Basic plot set up
  # ___________________

  data <- apply_scenarios(data, scenarios, scenario_labels, scenario_order)

  # ___________________
  # Build MLE plot
  # ___________________

  p <- ggplot2::ggplot(data, ggplot2::aes(x=xvar, y=weight), colour=colours) +
    get_theme_ssand() +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::xlab(xlab) +
    ggplot2::ylab(ylab)

  if (show_two_sex){
    p <- p + (aes(colour = sex)) +
      # ggplot2::theme(legend.position = "top", legend.title = ggplot2::element_blank()) +
      ggplot2::scale_colour_manual(values = colours)
  }

  p <- add_scenario_facets(p, data, scales = scales, ncol = ncol)

  return(p)
}
