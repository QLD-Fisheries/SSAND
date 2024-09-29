# Copyright 2024 Fisheries Queensland

# This file is part of SSAND.
# SSAND is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
# SSAND is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
# You should have received a copy of the GNU General Public License along with SSAND. If not, see <https://www.gnu.org/licenses/>.

#' Plot of recruitment pattern
#'
#' Stock synthesis functionality not yet introduced
#'
#' @param data Output from recruitmentpatternplot_prep(). A dataframe with recruitment (num), months (num), monthnames (fac), scenario (fac)
#' @param xlab Label for x-axis (character). Default is "Month".
#' @param ylab Label for y-axis (character). Default is "Recruitment %".
#' @param scenarios A vector of scenario numbers to be shown on plot (numeric). This was already specified in prep file, but this is a manual override to save running the prep function again.
#' @param scenario_labels A vector of customised scenario names (character). Default is "Scenario 1", "Scenario 2", etc.
#' @param scenario_order A vector to reorder how scenarios are displayed (character). Use the label names defined in "scenario_labels".
#' If "scenario_labels" is left blank, the labels will be "Scenario 1", "Scenario 2" etc.
#' Any scenarios not included in "scenario_order" will be tacked on in the order they appear in the input data.
#' @param scales Scales for ggplot2::facet_wrap(). Default is 'free', see ?ggplot2::facet_wrap for options.
#' @param ncol Number of columns for facet_wrap(). Default is 2.
#' @param colours A vector of colours used (character).
#'
#' @return Plot of recruitment pattern
#' @export
#'
#' @examples
#' data <- recruitmentpatternplot_prep_DD(dd_mle)
#' recruitmentpatternplot(data)
recruitmentpatternplot <- function(data,
                                   xlab = 'Month',
                                   ylab = 'Recruitment %',
                                   scenarios = NULL,
                                   scenario_labels = NULL,
                                   scenario_order = NULL,
                                   ncol = 2,
                                   scales = 'free',
                                   colours= c("black","grey80","grey20","grey60","grey40")){
  # Data input warnings
  if (!"recruitment" %in% names(data)) {warning("Input data is missing recruitment column")}
  if (!"months" %in% names(data)) {warning("Input data is missing months column")}
  if (!"monthnames" %in% names(data)) {warning("Input data is missing monthnames column")}
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

  p <- ggplot2::ggplot() +
    ggplot2::geom_point(data = data, ggplot2::aes(x=months,y=recruitment,colour=area))

  if (length(unique(data$monthnames))>1) {
    p <- p +
      ggplot2::geom_line(data = data, ggplot2::aes(x=months,y=recruitment,colour=area))
  }

  p <- p +
    ggplot2::scale_x_discrete(limits=month.name)+
    ggplot2::xlab(xlab) +
    ggplot2::ylab(ylab)+
    ggplot2::theme(legend.position = 'top')+
    ggplot2::theme_bw()+
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle=90,vjust=0.5, hjust = 1)) +
    ggplot2::scale_colour_manual(name="Area",values=colours)

  if (length(unique(data$scenario))>1){
    p <- p +
      ggplot2::facet_wrap(~scenario_labels, scales = scales, ncol = ncol)
  }

  return(p)
}
