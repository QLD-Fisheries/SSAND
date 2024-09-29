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
                        colours = NULL) {

  if (!missing(fleets)) {
    data <- data |> dplyr::filter(fleet %in% fleets)
  }

  # Data input warnings
  if (!"year" %in% names(data)) {warning("Input data is missing year column")}
  if (!"fleet" %in% names(data)) {warning("Input data is missing fleet column")}
  if (!"obs" %in% names(data)) {warning("Input data is missing obs column")}
  if (!"upper" %in% names(data)) {warning("Input data is missing upper column")}
  if (!"lower" %in% names(data)) {warning("Input data is missing lower column")}
  if (!"exp" %in% names(data)) {warning("Input data is missing exp column")}

  if (financial_year & xlab=="Year") {warning("Your x-axis implies calendar year, but you've indicated you're using financial year.")}

  if (missing(xlim)) {xlim <- c(min(data$year)-0.5,max(data$year)+0.5)}
  if (missing(ylim)) {ylim <- c(0,max(data$upper))}

  if (missing(xbreaks) & xlim[1]!=xlim[2]) {xbreaks <- unique(floor(pretty(xlim)))} # unique(floor()) ensures integers only
  if (missing(xbreaks) & xlim[1]==xlim[2]) {xbreaks <- xlim}

  if (missing(xlabels)) {xlabels <- xbreaks}

  if (financial_year & !show_dates_on_axis) {xlabels <- paste0(xbreaks-1,"\U2013",xbreaks)} else {xlabels <- xbreaks}
  if (missing(xangle)) {xangle <- ifelse(financial_year,90,0)}

  if (missing(colours)) {colours <- c("black",fq_palette("cols"))}

  if (!missing(scenarios)){data <- data |> dplyr::filter(scenario %in% scenarios)}

  if (missing(scenario_labels)) {
    data <- data |> dplyr::mutate(scenario_labels = as.factor(paste0("Scenario ",scenario)))
  } else {
    scenario.lookup<- data.frame(scenario = unique(data$scenario), scenario_labels = scenario_labels)
    data <- data |>
      dplyr::left_join(scenario.lookup, by = "scenario") |>
      dplyr::mutate(scenario_labels = as.factor(scenario_labels))
  }

  if (missing(fleet_names)) {
    data <- data |> dplyr::mutate(fleet_names = as.factor(paste0("Fleet ",fleet)))
  } else {
    fleet.lookup <- data.frame(fleet = unique(data$fleet), fleet_names = fleet_names)
    data <- data |>
      dplyr::left_join(fleet.lookup, by = "fleet") |>
      dplyr::mutate(fleet_names = as.factor(fleet_names))
  }

  if (!missing(scenario_order)) {
    # Add on any scenarios not included in the scenario_order list
    scenario_order = c(scenario_order, setdiff(scenario_labels, scenario_order))
    # Reorder scenarios
    data$scenario_labels <- factor(data$scenario_labels, levels = scenario_order)
  }

  p <- ggplot2::ggplot(data) +
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

  p <- p +
    ggplot2::xlab(xlab) +
    ggplot2::ylab(ylab) +
    ggplot2::scale_y_continuous(limits=ylim) +
    ggplot2::scale_x_continuous(limits = xlim, breaks = xbreaks, labels = xlabels) +
    ggplot2::theme_bw() +
    ggplot2::theme(text = ggplot2::element_text(size=text_size))

  return(p)
}



