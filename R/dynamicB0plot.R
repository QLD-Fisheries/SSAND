# Copyright 2024 Fisheries Queensland

# This file is part of SSAND.
# SSAND is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
# SSAND is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
# You should have received a copy of the GNU General Public License along with SSAND. If not, see <https://www.gnu.org/licenses/>.

#' Dynamic B0 plot
#'
#' @param data Output from dynamicB0plot_prep(). A data frame with variables called year (int), era (chr), type (chr), value (num), scenario (int)
#' @param area Area to show (num). If left blank, plot shows entire fishery.
#' @param xlab Label for x-axis (character). Default is "Year".
#' @param ylab Label for y-axis (character). Default is "Spawning biomass".
#' @param text_size Text size (num). Default is 12.
#' @param colours A vector of colours used (character).
#' @param linetype A vector of linetypes used (character)
#' @param scales Scales for ggplot2::facet_wrap(). Default is 'free', see ?ggplot2::facet_wrap for options.
#' @param ncol Number of columns for facet_wrap(). Default is 2.
#' @param scenarios A vector of scenario numbers to be shown on plot (numeric). This was already specified in prep file, but this is a manual override to save running the prep function again.
#' @param scenario_labels A vector of customised scenario names (character). Default is "Scenario 1", "Scenario 2", etc.
#' @param scenario_order A vector to reorder how scenarios are displayed (character). Use the label names defined in "scenario_labels".
#' If "scenario_labels" is left blank, the labels will be "Scenario 1", "Scenario 2" etc.
#' Any scenarios not included in "scenario_order" will be tacked on in the order they appear in the input data.
#' @param financial_year Set to TRUE if the assessment was based on financial year (logical). Adds 1 to each year in the dataset.
#' @param legend_position Position of the legend ("none", "left", "right", "bottom", "top", or two-element numeric vector for x and y position). Default is "top".
#' @param line_width Width of lines. Default is 0.7.
#' @param financial_year Set to TRUE if the assessment was based on financial year (logical). Adjusts the x-axis to show full financial year notation.
#' @param xbreaks A vector of breaks between x-axis labels, used in ggplot2::scale_x_continous() (numeric).
#' @param ybreaks A vector of breaks between y-axis labels, used in ggplot2::scale_y_continous() (numeric).
#' @param xlabels A vector of labels for the x-axis breaks.
#' @param ylabels A vector of labels for the y-axis breaks.
#' @param xlim A vector of lower and upper x-axis limits (e.g. c(1950, 2020)) (numeric).
#' @param ylim A vector of lower and upper y-axis limits (e.g. c(0,1)) (numeric).
#' @param xangle Set to 90 to rotate x-axis labels 90 degrees.
#'
#' @return A dynamic B0 plot to show the effect of fishing
#' @export
#'
#' @examples
#' data <- dynamicB0plot_prep_SS(ss_mle)
#' dynamicB0plot(data)
dynamicB0plot <- function(data,
                          area = NULL,
                          xlab = "Year",
                          ylab = "Spawning biomass",
                          xbreaks = NULL,
                          ybreaks = NULL,
                          xlabels = NULL,
                          ylabels = NULL,
                          xlim = NULL,
                          ylim = NULL,
                          xangle = NULL,
                          financial_year = FALSE,
                          text_size = 12,
                          colours = c("black","grey40"),
                          linetype = c("solid", "dashed"),
                          scales = "free",
                          ncol = 2,
                          scenarios = NULL,
                          scenario_labels = NULL,
                          scenario_order = NULL,
                          legend_position = "top",
                          line_width = 0.7) {

  # Data input warnings
  if (!"year" %in% names(data)) {warning("Input data is missing year column")}
  if (!"era" %in% names(data)) {warning("Input data is missing era column")}
  if (!"type" %in% names(data)) {warning("Input data is missing type column")}
  if (!"value" %in% names(data)) {warning("Input data is missing value column")}
  if (!"scenario" %in% names(data)) {warning("Input data is missing scenario column")}

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

  if (financial_year & xlab=="Year") {warning("Your x-axis implies calendar year, but you've indicated you're using financial year.")}

  if (missing(xlim)) {xlim <- c(min(data$year),max(data$year))}
  if (missing(ylim)) {ylim <- c(min(data$value),max(data$value))}

  if (missing(xbreaks)) {xbreaks <- pretty(xlim)}
  if (missing(ybreaks)) {ybreaks <- pretty(ylim)}

  if (missing(xlabels)) {xlabels <- xbreaks}
  if (missing(ylabels)) {ylabels <- ybreaks}

  if (financial_year) {xlabels <- paste0(xbreaks-1,"\U2013",xbreaks)} else {xlabels <- xbreaks}
  if (missing(xangle)) {xangle <- ifelse(financial_year,90,0)}


  if (missing(area)) {
    data <- data |> dplyr::filter(type %in% c("SSB", "SSB no fishing"))
  } else {
    data <- data |>
      dplyr::filter(grepl(paste0("area",area), type)) |>
      dplyr::mutate(type = ifelse(type==paste0("SSB_area",area),"SSB",type)) |>
      dplyr::mutate(type = ifelse(type==paste0("SSB_nofishing_area",area),"SSB no fishing",type))
  }

  p <- ggplot2::ggplot() +
    ggplot2::geom_line(data=data, ggplot2::aes(x=year, y=value, colour=type, linetype=type), linewidth = line_width) +
    ggplot2::geom_point(data=data |> dplyr::filter(era=="VIRG" & type=="SSB"), ggplot2::aes(x=year, y=value, shape="Equilibrium")) +
    ggplot2::theme_bw() +
    ggplot2::xlab(xlab) +
    ggplot2::ylab(ylab) +
    ggplot2::theme(text = ggplot2::element_text(size=text_size)) +
    ggplot2::scale_linetype_manual(values = linetype) +
    ggplot2::scale_colour_manual(values = colours) +
    ggplot2::theme(legend.position = legend_position, legend.title=ggplot2::element_blank()) +
    ggplot2::scale_x_continuous(limits = xlim, breaks = xbreaks, labels = xlabels) +
    ggplot2::scale_y_continuous(limits = ylim, breaks = ybreaks, labels = ylabels) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = xangle, vjust = 0.5, hjust=ifelse(xangle==90,0,0.5)))


  if (length(unique(data$scenario))>1){
    p <- p +
      ggplot2::facet_wrap(~scenario_labels, scales = scales, ncol = ncol)
  }

  return(p)
}
