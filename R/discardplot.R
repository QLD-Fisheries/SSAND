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
#' @param xangle Set to 90 to rotate x-axis labels 90 degrees.
#' @param financial_year Set to TRUE if the assessment was based on financial year (logical). Adjusts the x-axis to show full financial year notation.
#' @param text_size Text size (num). Default is 12.
#' @param point_size Size of points used in ggplot2::geom_point(). Default is 1.5.
#' @param show_dates_on_axis Set to TRUE to show full dates on x-axis as opposed to years.
#' @param colours A vector of colours used (character).
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
                        xangle = NULL,
                        financial_year = FALSE,
                        text_size = 12,
                        point_size = 1.5,
                        show_dates_on_axis = FALSE,
                        colours = c("black",fq_palette("alisecolours"))) {

  # ___________________
  # Data validation
  # ___________________

  # Data input warnings
  check_data_columns(data, c("year","fleet","obs","upper","lower","exp"))

  data$xvar <- data$year
  data$upper <- data$prob_upper; data$lower <- data$prob_lower

  # ___________________
  # Custom to this plot
  # ___________________

  if (!missing(fleets)) {
    data <- data |> dplyr::filter(fleet %in% fleets)
  }

  if (missing(fleet_names)) {
    data <- data |> dplyr::mutate(fleet_names = as.factor(paste0("Fleet ",fleet)))
  } else {
    fleet.lookup <- data.frame(fleet = unique(data$fleet), fleet_names = fleet_names)
    data <- data |>
      dplyr::left_join(fleet.lookup, by = "fleet") |>
      dplyr::mutate(fleet_names = as.factor(fleet_names))
  }

  # ___________________
  # Basic plot set up
  # ___________________

  data <- apply_scenarios(data, scenarios, scenario_labels, scenario_order)

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


  p <- ggplot2::ggplot(data) + get_theme_ssand()
  p <- add_x_scale_continuous(p, x_axis)
  p <- add_y_scale_continuous(p, y_axis)

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

  return(p)
}



