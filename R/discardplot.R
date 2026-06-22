# Copyright 2024 Fisheries Queensland

# This file is part of SSAND.
# SSAND is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
# SSAND is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
# You should have received a copy of the GNU General Public License along with SSAND. If not, see <https://www.gnu.org/licenses/>.

#' Discard plot
#'
#' @param data A dataframe from discardplot_prep with year, fleet, obs, upper, lower, exp
#' @param show_fits Set to TRUE to show model fits.
#' @param fleets A numeric or vector of fleet numbers to plot
#' @param fleet_names A vector of customised fleet names for legend
#' @param scenarios A vector of scenario numbers to be shown on plot (numeric). This was already specified in prep file, but this is a manual override to save running the prep function again.
#' @param scenario_labels Customise facet labels for individual scenarios
#' @param scenario_order A vector to reorder how scenarios are displayed (character). Use the label names defined in "scenario_labels".
#' If "scenario_labels" is left blank, the labels will be "Scenario 1", "Scenario 2" etc.
#' Any scenarios not included in "scenario_order" will be tacked on in the order they appear in the input data.
#' @param xlab Label for x-axis (character). Default is "Year".
#' @param ylab Label for y-axis (character). Default is "Catch rate (kg/operation day)".
#' @param xbreaks A vector of breaks between x-axis labels, used in ggplot2::scale_x_continous() (numeric).
#' @param ybreaks A vector of breaks between y-axis labels, used in ggplot2::scale_y_continous() (numeric).
#' @param xlim A vector of lower and upper x-axis limits (e.g. c(1950, 2020)) (numeric).
#' @param ylim A vector of lower and upper y-axis limits (e.g. c(0,1)) (numeric).
#' @param xlabels A vector of labels for the x-axis breaks.
#' @param ylabels A vector of labels for the y-axis breaks.
#' @param financial_year Set to TRUE if the assessment was based on financial year (logical). Adjusts the x-axis to show full financial year notation.
#' @param point_size Size of points used in ggplot2::geom_point(). Default is 1.5.
#' @param show_dates_on_axis Set to TRUE to show full dates on x-axis as opposed to years.
#' @param colours A vector of colours used (character).
#' @param text_size,legend_text_size,text_colour,legend_text_colour,legend_position,legend_box,legend_title_blank,panel_border,panel_border_colour,xangle Optional plotting theme overrides. Defaults are controlled by `theme_ssand()`
#'   and can be set globally via `set_ssand_style()`.
#'
#' @return Discard plot
#' @export
#'
#' @examples
#' data <- discardplot_prep_SS(ss_mle)
#' discardplot(data)
#' discardplot(data,
#'             fleets=c(1,2),
#'             fleet_names = c("Commercial","Recreational"))
discardplot <- function(data,
                        show_fits = TRUE,
                        fleets = NULL,
                        fleet_names = NULL,
                        scenarios = NULL,
                        scenario_labels = NULL,
                        scenario_order = NULL,
                        xlab = "Year",
                        ylab = "Discarded catch (kg)",
                        xbreaks = NULL,
                        ybreaks = NULL,
                        xlim = NULL,
                        ylim = NULL,
                        xlabels = NULL,
                        ylabels = NULL,
                        point_size = 1.5,
                        show_dates_on_axis = FALSE,
                        colours = c("black",fq_palette("alisecolours")),
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
  check_data_columns(data, c("year","fleet","obs","upper","lower","exp"))
  data$xvar <- data$year
  data$upper <- data$prob_upper; data$lower <- data$prob_lower

  # ___________________
  # Filter data
  # ___________________
  data <- apply_fleet_names(data, fleets, fleet_names)
  data <- apply_scenarios(data, scenarios, scenario_labels, scenario_order)

  # ___________________
  # Basic plot set up
  # ___________________
  p <- ggplot2::ggplot(data)

  # ___________________
  # Build MLE plot
  # ___________________
  p <- p +
    ggplot2::geom_errorbar(ggplot2::aes(x=year, ymin=lower, ymax=upper), colour=colours[1], width=.2, position=ggplot2::position_dodge(.1)) +
    ggplot2::geom_point(ggplot2::aes(x=year, y=obs), colour=colours[1], shape=19, size=point_size, position=ggplot2::position_dodge(0))

  if (show_fits) {
    p <- p +
      ggplot2::geom_point(ggplot2::aes(x=year, y=exp), colour=colours[2],shape="-",size=8, position=ggplot2::position_dodge(.1))
  }

  if (length(unique(data$fleet))>1 && length(unique(data$scenario))>1) {
    p <- p +
      ggplot2::facet_grid(rows=ggplot2::vars(fleet_names), cols=ggplot2::vars(scenario_labels))
  }

  if (length(unique(data$fleet))==1 && length(unique(data$scenario))>1) {
    p <- p +
      ggplot2::facet_wrap(~scenario_labels)
  }

  if (length(unique(data$fleet))>1 && length(unique(data$scenario))==1) {
    p <- p +
      ggplot2::facet_wrap(~fleet_names)
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

  return(p)
}



