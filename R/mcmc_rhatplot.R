# Copyright 2024 Fisheries Queensland

# This file is part of SSAND.
# SSAND is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
# SSAND is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
# You should have received a copy of the GNU General Public License along with SSAND. If not, see <https://www.gnu.org/licenses/>.

#' Plot of Rhat values for each parameter
#'
#' @param data Output from mcmc_rhatplot_prep() with columns parameter (fac), Rhat (num), scenario (fac), group (fac), xmax (num)
#' @param ncol Number of columns for facet_wrap(). Default is 3.
#' @param parameter_labels y-axis parameter labels, e.g. expression(xi)
#' @param ylab Label for y-axis (character). Default is "Fishing mortality".
#' @param show_point Set to TRUE to change to point type plot. Default is FALSE, displaying bar type plot
#' @param scenarios A vector of scenario numbers to be shown on plot (numeric). This was already specified in prep file, but this is a manual override to save running the prep function again.
#' @param scenario_labels A vector of customised scenario names (character). Default is "Scenario 1", "Scenario 2", etc.
#' @param scenario_order A vector to reorder how scenarios are displayed (character). Use the label names defined in "scenario_labels".
#' If "scenario_labels" is left blank, the labels will be "Scenario 1", "Scenario 2" etc.
#' Any scenarios not included in "scenario_order" will be tacked on in the order they appear in the input data.
#' @param financial_year Set to TRUE if the assessment was based on financial year (logical). Adjusts the x-axis to show full financial year notation.
#' @param text_size,legend_text_size,text_colour,legend_text_colour,legend_position,legend_box,legend_title_blank,panel_border,panel_border_colour,xangle Optional plotting theme overrides. Defaults are controlled by `theme_ssand()`
#'   and can be set globally via `set_ssand_style()`.
#'
#' @return A plot of R-hat values for parameter estimates
#' @export
#'
#' @examples
#' data <- mcmc_rhatplot_prep_SS(ss_mle, ss_mcmc,
#'            parameters = extract_SS_parameters(ss_mle)[c(2:10),])
#' mcmc_rhatplot(data)
mcmc_rhatplot <- function(data,
                          ncol = 3,
                          parameter_labels = NULL,
                          ylab = "Parameter",
                          show_point = FALSE,
                          scenarios = NULL,
                          scenario_labels = NULL,
                          scenario_order = NULL,
                          xangle = NULL,
                          legend_position = NULL,
                          financial_year = FALSE,
                          text_size = NULL,
                          legend_text_size = NULL,
                          text_colour = NULL,
                          legend_text_colour = NULL,
                          legend_box = NULL,
                          legend_title_blank = NULL,
                          panel_border = NULL,
                          panel_border_colour = NULL
                          ) {

  # Data input warnings
  check_data_columns(data, c("parameter","Rhat","scenario","group","xmax"))

  data <- apply_scenarios(data, scenarios, scenario_labels, scenario_order)

  if(!show_point) {
    p <- ggplot2::ggplot(data) +
      ggplot2::geom_bar(ggplot2::aes(x=Rhat, y=parameter, fill=group), stat = 'identity') +
      ggplot2::coord_cartesian(xlim = c(0.95, NA)) +
      ggplot2::facet_wrap(~scenario, scales = 'free_x', ncol = ncol)

  } else {
    ymax = length(unique(data$parameter)) + 1
    p <- ggplot2::ggplot(data) +
      ggplot2::geom_rect(ggplot2::aes(xmin = 0.95, xmax = 1.05, ymin = 0, ymax = ymax, fill = "A"), alpha = 0.2) + # , ymin = 1, ymax =yylim[2]
      ggplot2::geom_rect(ggplot2::aes(xmin = 1.05, xmax = 1.1, ymin = 0, ymax = ymax, fill = "B"), alpha = 0.2) +
      ggplot2::geom_rect(ggplot2::aes(xmin = 1.1, xmax = xmax+0.005, ymin = 0, ymax = ymax, fill = "C"), alpha = 0.2) +
      ggplot2::geom_point(ggplot2::aes(x=Rhat, y=parameter)) +
      ggplot2::facet_wrap(~scenario, ncol = ncol, scales = "free")
  }

  p <- p +
    ggplot2::scale_fill_manual(labels = c(expression(hat(R) < 1.05),
                                          expression(hat(R) < 1.1),
                                          expression(hat(R) > 1.1)),
                               values = c("#b4d9eb", "#4facda", "#136993")) +
    ggplot2::xlab(expression(hat(R))) +
    ggplot2::ylab(ylab)

  if (!missing(parameter_labels)){
    p = p + ggplot2::scale_y_discrete(labels= parameter_labels)
  }

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
