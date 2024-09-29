# Copyright 2024 Fisheries Queensland

# This file is part of SSAND.
# SSAND is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
# SSAND is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
# You should have received a copy of the GNU General Public License along with SSAND. If not, see <https://www.gnu.org/licenses/>.

#' Maturity plot
#'
#' @param data A data frame with variables value (num), maturity (num), sex (int), scenario (int), type (chr)
#' @param xlab Label for x-axis (character). Default is "Age".
#' @param ylab Label for y-axis (character). Default is "Carapace length (cm, beginning of year)".
#' @param text_size Text size (num). Default is 12.
#' @param show_two_sex Set to TRUE to activate a feature that is relevant for two-sex models (logical).
#' @param scenarios A vector of scenario numbers to be shown on plot (numeric). This was already specified in prep file, but this is a manual override to save running the prep function again.
#' @param scenario_labels A vector of customised scenario names (character). Default is "Scenario 1", "Scenario 2", etc.
#' @param scenario_order A vector to reorder how scenarios are displayed (character). Use the label names defined in "scenario_labels".
#' If "scenario_labels" is left blank, the labels will be "Scenario 1", "Scenario 2" etc.
#' Any scenarios not included in "scenario_order" will be tacked on in the order they appear in the input data.
#' @param colours A vector of colours used (character).
#' @param scales Scales for ggplot2::facet_wrap(). Default is 'free', see ?ggplot2::facet_wrap for options.
#' @param ncol Number of columns for facet_wrap(). Default is 2.
#' @param maturity_type Either "length" or "age", to define the x-axis.
#'
#' @return Maturity plot
#' @export
#'
#' @examples
#' data <- maturityplot_prep_SS(ss_mle)
#' maturityplot(data)
#'
#' data <- maturityplot_prep_DD(x_max=10,x_mat=2)
#' maturityplot(data)
maturityplot <- function(data,
                         maturity_type = "length",
                         xlab = NULL,
                         ylab = "Maturity",
                         text_size = 12,
                         show_two_sex=NULL,
                         scenarios = NULL,
                         scenario_labels = NULL,
                         scenario_order = NULL,
                         colours = NULL,
                         scales = 'free',
                         ncol = 2) {

  # Data input warnings
  if (!"value" %in% names(data)) {warning("Input data is missing value column")}
  if (!"maturity" %in% names(data)) {warning("Input data is missing maturity column")}
  if (!"sex" %in% names(data)) {warning("Input data is missing sex column")}
  if (!"scenario" %in% names(data)) {warning("Input data is missing scenario column")}
  if (!"type" %in% names(data)) {warning("Input data is missing type column")}


  data <- data |>
    dplyr::filter(type %in% maturity_type) |>
    dplyr::mutate(sex = dplyr::recode(sex, "1" = "Female" ,  "2" = "Male"))

  if (missing(xlab)) {xlab <- ifelse(maturity_type=="length","Length (cm)", "Age (years)")}

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


  if (missing(colours)) {colours = c("grey70",fq_palette("alisecolours"))}

  p <- ggplot2::ggplot(data) +
    ggplot2::geom_line(ggplot2::aes(x=value, y=maturity, colour=sex)) +
    ggplot2::geom_point(ggplot2::aes(x=value, y=maturity, colour=sex)) +
    ggplot2::theme_bw() +
    ggplot2::xlab(xlab) +
    ggplot2::ylab(ylab) +
    ggplot2::ylim(c(0,1)) +
    ggplot2::theme(text = ggplot2::element_text(size=text_size)) +
    ggplot2::theme(legend.position = "top", legend.title = ggplot2::element_blank()) +
    ggplot2::scale_colour_manual(values = colours)


  if (length(unique(data$scenario))>1){
    p <- p +
      ggplot2::facet_wrap(~scenario_labels, scales = scales, ncol = ncol)
  }

  return(p)
}
