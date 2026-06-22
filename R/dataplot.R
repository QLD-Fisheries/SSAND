# Copyright 2024 Fisheries Queensland

# This file is part of SSAND.
# SSAND is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
# SSAND is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
# You should have received a copy of the GNU General Public License along with SSAND. If not, see <https://www.gnu.org/licenses/>.

#' Data plot
#'
#' @param data Output from dataplot_prep(). Columns are year (num), typename (fac), size (num), fleet (fac), scenario (int)
#' @param xlab Label for x-axis (character). Default is "Year".
#' @param ylab Label for y-axis (character). Default is NULL.
#' @param xbreaks A vector of breaks between x-axis labels, used in ggplot2::scale_x_continous() (numeric).
#' @param xlabels A vector of labels for the x-axis breaks.
#' @param xlim A vector of lower and upper x-axis limits (e.g. c(1950, 2020)) (numeric).
#' @param ylim A vector of lower and upper y-axis limits (e.g. c(0,1)) (numeric).
#' @param fleet_names A vector of customised fleet names for legend
#' @param colours A vector of colours used (character).
#' @param size_range A two-number vector specifying the minimum and maximum sizes for the circles
#' @param scenarios Scenario to be shown on plot (numeric). Default is 1. This was already specified in prep file, but this is a manual override to save running the prep function again.
#' @param scenario_labels A vector of customised scenario names (character). Default is "Scenario 1", "Scenario 2", etc.
#' @param scenario_order A vector to reorder how scenarios are displayed (character). Use the label names defined in "scenario_labels".
#' If "scenario_labels" is left blank, the labels will be "Scenario 1", "Scenario 2" etc.
#' Any scenarios not included in "scenario_order" will be tacked on in the order they appear in the input data.
#' @param scales Scales for ggplot2::facet_wrap(). Default is 'free', see ?ggplot2::facet_wrap for options.
#' @param ncol Number of columns for facet_wrap(). Default is .
#' @param financial_year Set to TRUE if the assessment was based on financial year (logical). Adjusts the x-axis to show full financial year notation.
#' @param hollow A logitcal to switch between two colouring styles. Default is TRUE.
#' @param text_size,legend_text_size,text_colour,legend_text_colour,legend_position,legend_box,legend_title_blank,panel_border,panel_border_colour,xangle Optional plotting theme overrides. Defaults are controlled by `theme_ssand()`
#'   and can be set globally via `set_ssand_style()`.
#'
#' @return A plot of data used in model
#' @export
#'
#' @examples
#' data <- dataplot_prep_SS(ss_mle)
#' dataplot(data)
#'
#' data <- dataplot_prep_DD(dd_mle)
#' dataplot(data)
dataplot <- function(data,
                     xlab = "Year",
                     ylab = NULL,
                     xbreaks = NULL,
                     xlabels = NULL,
                     xlim = NULL,
                     ylim = NULL,
                     colours = fq_palette("alisecolours"),
                     size_range=c(0.5,5),
                     scenarios = 1,
                     scenario_labels = NULL,
                     scenario_order = NULL,
                     scales = 'free',
                     ncol = 2,
                     fleet_names = NULL,
                     hollow = TRUE,
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
  check_data_columns(data, c("year","typename","size","fleet","scenario"))
  data$xvar <- data$year

  # ___________________
  # Custom to this plot
  # ___________________
  data <- apply_fleet_names(data, fleets=NULL, fleet_names)
  data <- apply_scenarios(data, scenarios, scenario_labels, scenario_order)

  # ___________________
  # Basic plot set up
  # ___________________
  p <- ggplot2::ggplot(data)

  # ___________________
  # Build plot
  # ___________________
  if (hollow) {
    p <- p +
      ggplot2::geom_point(ggplot2::aes(x=year, y=fleet_names, size=size,colour=fleet_names), alpha = 0.01) + # coloured
      ggplot2::geom_point(ggplot2::aes(x=year, y=fleet_names, size=size, colour=fleet_names),shape = 1) # outline
  } else {
    p <- p +
      ggplot2::geom_point(ggplot2::aes(x=year, y=fleet_names, size=size,colour=fleet_names)) + # coloured
      ggplot2::geom_point(ggplot2::aes(x=year, y=fleet_names, size=size), shape = 1,colour="#9D9D9D", alpha = 0.3)
  }

  p <- p +
    ggplot2::facet_wrap(~typename, ncol=1, scales='free_y') +
    ggplot2::scale_x_continuous(limits = xlim, breaks = xbreaks, labels = xlabels) +
    ggplot2::scale_y_discrete(limits=rev, position = "right") + # read y-axis top to bottom
    ggplot2::guides(size="none",colour="none") +
    ggplot2::xlab(xlab) +
    ggplot2::scale_color_manual(values=colours) +
    ggplot2::scale_size_continuous(range = size_range)

  # If only one fleet, remove fleet names
  if (length(unique(data$fleet))==1) {
    p <- p +
      ggplot2::theme(axis.text.y = ggplot2::element_blank())
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

  y_axis <- build_y_axis(y = data$value,
                         ylab = ylab,
                         ylim = ylim,
                         ybreaks = ybreaks,
                         ylabels = ylabels,
                         lower = 0,
                         upper = data$upper)

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

  p <- p +
    ggplot2::theme(strip.background = ggplot2::element_rect(color=ggplot2::alpha("white",0), fill="white", linewidth = 0, linetype="solid")) +
    ggplot2::theme(axis.text = ggplot2::element_text(face="bold"),
                   strip.text.x = ggplot2::element_text(face="bold"))

  return(p)
}
