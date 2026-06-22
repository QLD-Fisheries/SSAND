# Copyright 2024 Fisheries Queensland

# This file is part of SSAND.
# SSAND is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
# SSAND is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
# You should have received a copy of the GNU General Public License along with SSAND. If not, see <https://www.gnu.org/licenses/>.

#' Plot catchability pattern
#'
#' @param data Output from catchabilityplot_prep(). A dataframe with q (num), month (int), month_point (int), monthnames (factor), fleet (factor), scenario (factor)
#' @param xlab Label for x-axis (character). Default is "Month".
#' @param ylab Label for y-axis (character). Default is "Catchability coefficient (q)".
#' @param ncol Number of columns for facet_wrap(). Default is 2.
#' @param scales Scales for ggplot2::facet_wrap(). Default is 'free', see ?ggplot2::facet_wrap for options.
#' @param colours A vector of colours used for lines (character).
#' @param scenario_labels A vector of customised scenario names (character). Default is "Scenario 1", "Scenario 2", etc.
#' @param scenarios A vector of scenario numbers to be shown on plot (numeric). This was already specified in prep file, but this is a manual override to save running the prep function again.
#' @param scenario_order A vector to reorder how scenarios are displayed (character). Use the label names defined in "scenario_labels".
#' If "scenario_labels" is left blank, the labels will be "Scenario 1", "Scenario 2" etc.
#' Any scenarios not included in "scenario_order" will be tacked on in the order they appear in the input data.
#' @param text_size,legend_text_size,text_colour,legend_text_colour,legend_position,legend_box,legend_title_blank,panel_border,panel_border_colour,xangle Optional plotting theme overrides. Defaults are controlled by `theme_ssand()`
#'   and can be set globally via `set_ssand_style()`.
#'
#' @return A plot of catchability pattern
#' @export
#'
#' @examples
#' data <- catchabilityplot_prep_DD(dd_mle)
#' catchabilityplot(data)
#'
#' data <- catchabilityplot_prep_SS(ss_mle)
#' catchabilityplot(data)
catchabilityplot <- function(data,
                             xlab = 'Month',
                             ylab = 'Catchability coefficient (q)',
                             ncol = 2,
                             scales = 'free',
                             colours = fq_palette("alisecolours"),
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
                             panel_border_colour = NULL) {
  # ___________________
  # Data validation
  # ___________________
  check_data_columns(data, c("q","month","fleet","scenario","month_point","monthnames"))
  data$xvar <- data$month

  # ___________________
  # Basic plot set up
  # ___________________
  data <- apply_scenarios(data, scenarios, scenario_labels, scenario_order)
  p <- ggplot2::ggplot(data)

  # ___________________
  # Build MLE plot
  # ___________________
  p <- p +
    ggplot2::geom_line(ggplot2::aes(x=month,y=q,col=fleet)) +
    ggplot2::geom_point(ggplot2::aes(x=month_point,y=q,col=fleet)) +
    ggplot2::scale_x_discrete(limits=month.name) + ##########################
    ggplot2::scale_y_continuous(limits=ylim, breaks = ybreaks) +
    ggplot2::scale_colour_manual(name = "Fleet", values = colours)

  if (length(unique(data$scenario))>1){
    p <- p +
      ggplot2::facet_wrap(~scenario_labels, scales = scales, ncol = ncol)
  }

  # ___________________
  # Axes and theme
  # ___________________
  x_axis <- build_x_axis(x = data$xvar,
                         xlab = xlab,
                         xlim = xlim,
                         xbreaks = xbreaks,
                         xlabels = xlabels,
                         financial_year = financial_year,
                         expand_upper = as.numeric(show_final_biomass),
                         xangle = xangle,
                         is_date = FALSE)

  y_axis <- build_y_axis(y = data$q,
                         ylab = ylab,
                         ylim = ylim,
                         ybreaks = ybreaks,
                         ylabels = ylabels,
                         lower = 0,
                         upper = data$q) #


  p <- add_x_scale_continuous(p, x_axis)
  p <- add_y_scale_continuous(p, y_axis)

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
                       xangle = x_axis$angle)

  return(p)
}
