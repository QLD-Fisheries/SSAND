# Copyright 2024 Fisheries Queensland

# This file is part of SSAND.
# SSAND is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
# SSAND is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
# You should have received a copy of the GNU General Public License along with SSAND. If not, see <https://www.gnu.org/licenses/>.

#' Plot of Rhat values for each parameter
#'
#' @param data Output from mcmc_rhatplot_prep() with columns parameter (fac), Rhat (num), scenario (fac), group (fac), xmax (num)
#' @param ncol Number of columns for facet_wrap(). Default is 3.
#' @param parameter_labels y-axis parameter labels, e.g. expression(xi)
#' @param ylab Label for y-axis (character). Default is "Fishing mortality".
#' @param show_point Set to TRUE to change to point type plot. Default is FALSE, displaying bar type plot
#' @param scenarios A vector of scenario numbers to be shown on plot (numeric). This was already specified in prep file, but this is a manual override to save running the prep function again.
#' @param scenario_labels A vector of customised scenario names (character). Default is "Scenario 1", "Scenario 2", etc.
#' @param scenario_order A vector to reorder how scenarios are displayed (character). Use the label names defined in "scenario_labels".
#' If "scenario_labels" is left blank, the labels will be "Scenario 1", "Scenario 2" etc.
#' Any scenarios not included in "scenario_order" will be tacked on in the order they appear in the input data.
#'
#' @return A plot of R-hat values for parameter estimates
#' @export
#'
#' @examples
#' data <- mcmc_rhatplot_prep_SS(ss_mle, ss_mcmc,
#'            parameters = extract_SS_parameters(ss_mle)[c(2:10),])
#' mcmc_rhatplot(data)
mcmc_rhatplot <- function(data,
                          ncol = 3,
                          parameter_labels = NULL,
                          ylab = "Parameter",
                          show_point = FALSE,
                          scenarios = NULL,
                          scenario_labels = NULL,
                          scenario_order = NULL
                          ) {

  # Data input warnings
  if (!"parameter" %in% names(data)) {warning("Input data is missing parameter column")}
  if (!"Rhat" %in% names(data)) {warning("Input data is missing Rhat column")}
  if (!"scenario" %in% names(data)) {warning("Input data is missing scenario column")}
  if (!"group" %in% names(data)) {warning("Input data is missing group column")}
  if (!"xmax" %in% names(data)) {warning("Input data is missing xmax column")}

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

  if(!show_point) {
    p <- ggplot2::ggplot(data) +
      ggplot2::geom_bar(ggplot2::aes(x=Rhat, y=parameter, fill=group), stat = 'identity') +
      ggplot2::coord_cartesian(xlim = c(0.95, NA)) +
      ggplot2::facet_wrap(~scenario, scales = 'free_x', ncol = ncol)

  } else {
    ymax = length(unique(data$parameter)) + 1
    p <- ggplot2::ggplot(data) +
      ggplot2::geom_rect(ggplot2::aes(xmin = 0.95, xmax = 1.05, ymin = 0, ymax = ymax, fill = "A"), alpha = 0.2) + # , ymin = 1, ymax =yylim[2]
      ggplot2::geom_rect(ggplot2::aes(xmin = 1.05, xmax = 1.1, ymin = 0, ymax = ymax, fill = "B"), alpha = 0.2) +
      ggplot2::geom_rect(ggplot2::aes(xmin = 1.1, xmax = xmax+0.005, ymin = 0, ymax = ymax, fill = "C"), alpha = 0.2) +
      ggplot2::geom_point(ggplot2::aes(x=Rhat, y=parameter)) +
      ggplot2::facet_wrap(~scenario, ncol = ncol, scales = "free")
  }

  p <- p +
    ggplot2::scale_fill_manual(labels = c(expression(hat(R) < 1.05),
                                          expression(hat(R) < 1.1),
                                          expression(hat(R) > 1.1)),
                               values = c("#b4d9eb", "#4facda", "#136993")) +
    ggplot2::xlab(expression(hat(R))) +
    ggplot2::ylab(ylab) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = 'top') +
    ggplot2::theme(legend.title = ggplot2::element_blank())

  if (!missing(parameter_labels)){
    p = p + ggplot2::scale_y_discrete(labels= parameter_labels)
  }
  return(p)
}
