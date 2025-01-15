# Copyright 2024 Fisheries Queensland

# This file is part of SSAND.
# SSAND is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
# SSAND is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
# You should have received a copy of the GNU General Public License along with SSAND. If not, see <https://www.gnu.org/licenses/>.

#' Catch plot
#'
#' @param data Output from catchplot_prep(). A dataframe with date (date), value (int), fleet (fac), scenario (int)
#' @param xlab Label for x-axis (character). Default is "Year".
#' @param ylab Label for y-axis (character). Default is "Retained catch (t)" for DDUST models. Default is determined by catchplot_prep_SS partition setting for SS models.
#' @param fleet_names A vector of customised fleet names for legend (character).
#' @param xbreaks A vector of breaks between x-axis labels, used in ggplot2::scale_x_continous() (numeric).
#' @param ybreaks A vector of breaks between y-axis labels, used in ggplot2::scale_y_continous() (numeric).
#' @param xlabels A vector of labels for the x-axis breaks.
#' @param ylabels A vector of labels for the y-axis breaks.
#' @param xlim A vector of lower and upper x-axis limits (e.g. c(1950, 2020)) (numeric).
#' @param ylim A vector of lower and upper y-axis limits (e.g. c(0,1)) (numeric).
#' @param xangle Set to 90 to rotate x-axis labels 90 degrees.
#' @param colours A vector of colours used (character).
#' @param legend_position Position of the legend ("none", "left", "right", "bottom", "top", or two-element numeric vector for x and y position). Default is "top".
#' @param reverse Set to TRUE to reverse the default stacking order (logical). This is useful if you're rotating both the plot and legend.
#' @param strip_position Customise ggplot2::facet_wrap() strips. By default, the labels are displayed on the top of the plot. Using strip_position it is possible to place the labels on either of the four sides by setting strip.position = c("top", "bottom", "left", "right").
#' @param show_annual_aggregate Set to TRUE to aggregate annually (logical). Default is FALSE.
#' @param scenarios A vector of scenarios to plot (numeric). Shows all scenarios if left blank. Can be overridden in the plotting function.
#' @param scenario_labels A vector of customised scenario names (character). Default is "Scenario 1", "Scenario 2", etc.
#' @param scenario_order A vector to reorder how scenarios are displayed (character). Use the label names defined in "scenario_labels".
#' If "scenario_labels" is left blank, the labels will be "Scenario 1", "Scenario 2" etc.
#' Any scenarios not included in "scenario_order" will be tacked on in the order they appear in the input data.
#' @param scales Scales for ggplot2::facet_wrap(). Default is 'free', see ?ggplot2::facet_wrap for options.
#' @param ncol Number of columns for facet_wrap(). Default is 2.
#' @param financial_year Set to TRUE if the assessment was based on financial year (logical). Adjusts the x-axis to show full financial year notation.
#' @param show_dates_on_axis Set to TRUE to show full dates on x-axis as opposed to years.
#'
#' @return Catch plot
#' @export
#'
#' @examples
#' data <- catchplot_prep_SS(ss_mle)
#' catchplot(data, fleet_names = "Commercial")
#' catchplot(data, fleet_names = "Commercial", financial_year=TRUE)
#'
#' data <- catchplot_prep_DD(dd_mle)
#' catchplot(data)
catchplot <- function(data,
                      xlab = "Year",
                      ylab = NULL,
                      xbreaks = NULL,
                      ybreaks = NULL,
                      xlabels = NULL,
                      ylabels = NULL,
                      xlim = NULL,
                      ylim = NULL,
                      xangle = NULL,
                      financial_year = FALSE,
                      fleet_names = NULL,
                      colours = NULL,
                      legend_position = "top",
                      reverse = FALSE,
                      strip_position = NA,
                      show_annual_aggregate = FALSE,
                      show_dates_on_axis = FALSE,
                      scenarios = NULL,
                      scenario_labels = NULL,
                      scenario_order = NULL,
                      scales = 'free',
                      ncol = 2) { # function name with default values

  # Data input warnings
  if (!"date" %in% names(data)) {warning("Input data is missing date column")}
  if (!"value" %in% names(data)) {warning("Input data is missing value column")}
  if (!"fleet" %in% names(data)) {warning("Input data is missing fleet column")}
  if (!"scenario" %in% names(data)) {warning("Input data is missing scenario column")}

  if (missing(ylab)) {
    if ("partition" %in% names(data)) {
      if (data$partition[1]=="sel") {ylab = "Catch (retained and total discarded) (t)"}
      if (data$partition[1]=="retain") {ylab = "Retained catch (t)"}
      if (data$partition[1]=="dead") {ylab = "Dead catch (t)"}
    } else {
      ylab = "Retained catch (t)"
    }
  }

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

  if (!missing(fleet_names)) {
    fleet_names.lookup <- data.frame(fleet = unique(data$fleet),
                                     fleet_names = fleet_names)
    data <- data |>
      dplyr::left_join(fleet_names.lookup, by="fleet") |>
      dplyr::select(-fleet) |>
      dplyr::rename(fleet=fleet_names)
  }

  if (missing(colours)) {colours <- c("grey70",fq_palette("alisecolours")[1:9])}

  if (show_annual_aggregate){
    data <- data |>
      dplyr::mutate(year = as.numeric(format(date,"%Y"))) |>
      dplyr::group_by(year, fleet) |>
      dplyr::summarise(value = sum(value), .groups = 'drop') |>
      dplyr::mutate(date = as.Date(paste0('01/01/',year), format = '%d/%m/%Y'))
  }


  if (financial_year & xlab=="Year") {warning("Your x-axis implies calendar year, but you've indicated you're using financial year.")}

  # If xlim is entered as just years, convert to dates
  if (!missing(xlim)) {
    if (nchar(xlim[1])==4) {
      xlim <- c(as.Date(paste0(xlim[1], "-01-01"), format = "%Y-%m-%d"),
                as.Date(paste0(xlim[2], "-01-01"), format = "%Y-%m-%d"))
    }
  }

  # if (!show_dates_on_axis) {
  #   data <- data |> dplyr::mutate(date = lubridate::year(date))
  # }
  if (missing(xlim)) {xlim <- c(min(data$date),max(data$date)+1)}
  if (missing(ylim)) {ylim <- c(0,max(data$value)+1)}

  if (missing(xbreaks)) {xbreaks <- pretty(xlim)}
  if (missing(ybreaks)) {ybreaks <- pretty(ylim)}

  # if (missing(xlabels)) {xlabels <- xbreaks}
  if (missing(ylabels)) {ylabels <- ybreaks}

  if (missing(xlabels) & !financial_year & show_dates_on_axis) {xlabels <- xbreaks}
  if (missing(xlabels) & !financial_year & !show_dates_on_axis) {xlabels <- lubridate::year(xbreaks)}

  if (missing(xlabels) & financial_year & !show_dates_on_axis) {xlabels <- paste0(lubridate::year(xbreaks)-1,"\U2013",lubridate::year(xbreaks))}
  if (missing(xlabels) & financial_year & show_dates_on_axis) {xlabels <- xbreaks}

  if (missing(xangle)) {xangle <- ifelse(financial_year,90,0)}



  p <- ggplot2::ggplot(data) +
    ggplot2::geom_bar(data,
                      mapping = ggplot2::aes(x = date, y = value, fill = as.factor(fleet)),
                      position = ggplot2::position_stack(reverse = reverse),
                      stat = 'identity') +
    ggplot2::xlab(xlab) +
    ggplot2::ylab(ylab) +
    ggplot2::theme_bw() +
    ggplot2::scale_fill_manual(values = colours) +
    ggplot2::scale_colour_manual(values = "#3d4040") +
    ggplot2::theme(panel.background = ggplot2::element_rect(fill = NA, colour = "black"),
                   legend.title = ggplot2::element_blank(),
                   legend.position = legend_position) +
    ggplot2::theme(text = ggplot2::element_text(size=12),
                   legend.text = ggplot2::element_text(size=12)) +
    ggplot2::scale_x_continuous(limits = xlim, breaks = xbreaks, labels = xlabels) +
    ggplot2::scale_y_continuous(limits = ylim, breaks = ybreaks, labels = ylabels) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = xangle, vjust = 0.5, hjust=ifelse(xangle==90,0,0.5)))



  if (length(unique(data$scenario))>1){
    p <- p +
      ggplot2::facet_wrap(~scenario_labels, scales = scales, ncol = ncol)
  }

  return(p)
}
