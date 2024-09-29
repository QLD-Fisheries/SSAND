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
#' @param xangle Set to 90 to rotate x-axis labels 90 degrees.
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
                     xangle = NULL,
                     financial_year = FALSE,
                     colours = NULL,
                     size_range=c(0.5,5),
                     scenarios = 1,
                     scenario_labels = NULL,
                     scenario_order = NULL,
                     scales = 'free',
                     ncol = 2,
                     fleet_names = NULL) {

  # Data input warnings
  if (!"year" %in% names(data)) {warning("Input data is missing year column")}
  if (!"typename" %in% names(data)) {warning("Input data is missing typename column")}
  if (!"size" %in% names(data)) {warning("Input data is missing size column")}
  if (!"fleet" %in% names(data)) {warning("Input data is missing fleet column")}
  if (!"scenario" %in% names(data)) {warning("Input data is missing scenario column")}


  if (financial_year & xlab=="Year") {warning("Your x-axis implies calendar year, but you've indicated you're using financial year.")}

  if (missing(xlim)) {xlim <- c(min(data$year),max(data$year))}
  if (missing(xbreaks)) {xbreaks <- pretty(xlim)}
  if (financial_year) {xlabels <- paste0(xbreaks-1,"\U2013",xbreaks)} else {xlabels <- xbreaks}
  if (missing(xangle)) {xangle <- ifelse(financial_year,90,0)}

  if (missing(colours)) {colours = fq_palette("alisecolours")}

  if (!missing(scenarios)){data <- data |> dplyr::filter(scenario %in% scenarios)}

  if (missing(scenario_labels)) {
    data <- data |> dplyr::mutate(scenario_labels = as.factor(paste0("Scenario ",scenario)))
  } else {
    scenario.lookup<- data.frame(scenario = unique(data$scenario), scenario_labels = scenario_labels)
    data <- data |>
      dplyr::left_join(scenario.lookup, by = "scenario") |>
      dplyr::mutate(scenario_labels = as.factor(scenario_labels))
  }

  if (missing(fleet_names)) {fleet_names <- paste0('Fleet ',sort(unique(data$fleet)))}

  fleet_names.lookup <- data.frame(fleet = unique(data$fleet),
                                   fleet_names = fleet_names)
  data <- data |>
    dplyr::left_join(fleet_names.lookup, by="fleet")


  # if (!missing(scenario_order)) {
  #   # Add on any scenarios not included in the scenario_order list
  #   scenario_order = c(scenario_order, setdiff(scenario_labels, scenario_order))
  #   # Reorder scenarios
  #   data$scenario_labels <- factor(data$scenario_labels, levels = scenario_order)
  # }


  p <- ggplot2::ggplot(data) +
    ggplot2::theme_bw() +
    ggplot2::geom_point(ggplot2::aes(x=year, y=fleet_names, size=size,colour=fleet_names)) + # coloured
    ggplot2::geom_point(ggplot2::aes(x=year, y=fleet_names, size=size),shape = 1,colour="#9D9D9D", alpha = 0.3) + # outline
    ggplot2::facet_wrap(~typename, ncol=1, scales='free_y') +
    ggplot2::scale_x_continuous(limits = xlim, breaks = xbreaks, labels = xlabels) +
    ggplot2::scale_y_discrete(limits=rev, position = "right") + # read y-axis top to bottom
    ggplot2::theme(strip.background = ggplot2::element_rect(color=ggplot2::alpha("white",0), fill="white", linewidth = 0, linetype="solid")) +
    ggplot2::theme(axis.text = ggplot2::element_text(face="bold"),
                   strip.text.x = ggplot2::element_text(face="bold")) +
    ggplot2::guides(size="none",colour="none") +
    ggplot2::xlab(xlab) +
    ggplot2::scale_color_manual(values=colours) +
    ggplot2::theme(legend.text = ggplot2::element_text(size=10)) +
    ggplot2::theme(text = ggplot2::element_text(size=10)) +
    ggplot2::scale_size_continuous(range = size_range) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = xangle, vjust = 0.5, hjust=ifelse(xangle==90,0,0.5)))


  if (missing(ylab)) {
    p <- p +
      ggplot2::ylab(ggplot2::element_blank())
  } else {
    p <- p +
      ggplot2::ylab(ylab)
  }

  # If only one fleet, remove fleet names
  if (length(unique(data$fleet))==1) {
    p <- p +
      ggplot2::theme(axis.text.y = ggplot2::element_blank())
  }

  return(p)
}
