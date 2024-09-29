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
#' @param text_size Text size (num). Default is 12.
#' @param colours A vector of colours used for lines (character).
#' @param scenario_labels A vector of customised scenario names (character). Default is "Scenario 1", "Scenario 2", etc.
#' @param scenarios A vector of scenario numbers to be shown on plot (numeric). This was already specified in prep file, but this is a manual override to save running the prep function again.
#' @param scenario_order A vector to reorder how scenarios are displayed (character). Use the label names defined in "scenario_labels".
#' If "scenario_labels" is left blank, the labels will be "Scenario 1", "Scenario 2" etc.
#' Any scenarios not included in "scenario_order" will be tacked on in the order they appear in the input data.
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
                             text_size = 12,
                             ncol = 2,
                             scales = 'free',
                             colours = fq_palette("alisecolours"),
                             scenarios = NULL,
                             scenario_labels = NULL,
                             scenario_order = NULL) {

  # Data input warnings
  if (!"q" %in% names(data)) {warning("Input data is missing q column")}
  if (!"month" %in% names(data)) {warning("Input data is missing month column")}
  if (!"fleet" %in% names(data)) {warning("Input data is missing fleet column")}
  if (!"scenario" %in% names(data)) {warning("Input data is missing scenario column")}
  if (!"month_point" %in% names(data)) {warning("Input data is missing month_point column")}
  if (!"monthnames" %in% names(data)) {warning("Input data is missing monthnames  column")}

  ylim <- c(0,max(data$q))
  ybreaks <- pretty(ylim)

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

  p <- ggplot2::ggplot(data)+
    ggplot2::geom_line(ggplot2::aes(x=month,y=q,col=fleet))+
    ggplot2::geom_point(ggplot2::aes(x=month_point,y=q,col=fleet))+
    ggplot2::scale_x_discrete(limits=month.name)+
    ggplot2::scale_y_continuous(limits=ylim, breaks = ybreaks)+
    ggplot2::xlab(xlab)+
    ggplot2::ylab(ylab)+
    ggplot2::theme_bw()+
    ggplot2::theme(legend.position = 'top',
                   axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust=1))+
    ggplot2::theme(text = ggplot2::element_text(size=text_size)) +
    ggplot2::scale_colour_manual(name = "Fleet", values = colours)

  if (length(unique(data$scenario))>1){
    p <- p +
      ggplot2::facet_wrap(~scenario_labels, scales = scales, ncol = ncol)
  }

  return(p)
}
