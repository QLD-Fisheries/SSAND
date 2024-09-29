# Copyright 2024 Fisheries Queensland

# This file is part of SSAND.
# SSAND is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
# SSAND is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
# You should have received a copy of the GNU General Public License along with SSAND. If not, see <https://www.gnu.org/licenses/>.

#' Yield curve plot for MCMC runs
#'
#' @param data Output from yieldplot_prep() with columns Final_bio (num), yield (num), data (chr), scenario (fac)
#' @param xlab Label for x-axis (character). Default is "Biomass (relative)".
#' @param ylab Label for y-axis (character). Default is "Equilibrium dead catch (t)".
#' @param text_size Text size (num). Default is 12.
#' @param show_current_line Set to TRUE to include a line on the plot showing where the current fishing pressure sits (logical).
#' @param show_msy_line Set to TRUE to include a line that shows MSY (logical).
#' @param scenarios A vector of scenario numbers to be shown on plot (numeric). This was already specified in prep file, but this is a manual override to save running the prep function again.
#' @param scenario_labels A vector of customised scenario names (character). Default is "Scenario 1", "Scenario 2", etc.
#' @param scenario_order A vector to reorder how scenarios are displayed (character). Use the label names defined in "scenario_labels".
#' If "scenario_labels" is left blank, the labels will be "Scenario 1", "Scenario 2" etc.
#' Any scenarios not included in "scenario_order" will be tacked on in the order they appear in the input data.
#' @param scales Scales for ggplot2::facet_wrap(). Default is 'free', see ?ggplot2::facet_wrap for options.
#' @param ncol Number of columns for facet_wrap(). Default is 2.
#'
#' @return Plot of spawning biomass vs equilibrium catch
#' @export
#'
#' @examples
#' data <- yieldplot_prep_SS(ss_mle)
#' yieldplot(data, show_msy_line=TRUE)
yieldplot <- function(data,
                      xlab = "Biomass (relative)",
                      ylab = "Equilibrium dead catch (t)",
                      text_size = 12,
                      show_current_line = TRUE,
                      show_msy_line = FALSE,
                      scenarios = NULL,
                      scenario_labels = NULL,
                      scenario_order = NULL,
                      scales = 'free',
                      ncol = 2) {

  # Data input warnings
  if (!"Final_bio" %in% names(data)) {warning("Input data is missing Final_bio column")}
  if (!"yield" %in% names(data)) {warning("Input data is missing yield column")}
  if (!"data" %in% names(data)) {warning("Input data is missing data column")}
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


  if (show_current_line & show_msy_line){

    if(!"msy" %in% data$data){
      cat("MSY line not provided in plot because 'msy_run' not provided in 'mcmc_yieldplot_prep' \n")
    }

    final_data = data |>
      dplyr::filter(data=="final")

    msy_data = data |>
      dplyr::filter(data=="msy")

    p <- ggplot2::ggplot(data) +
      ggplot2::geom_line(ggplot2::aes(x=Final_bio, y=yield/1000)) +
      ggplot2::geom_segment(data = final_data, ggplot2::aes(x = Final_bio, xend = Final_bio,
                                                            y = 0, yend = yield/1000, linetype = "A")) +
      ggplot2::geom_segment(data = msy_data, ggplot2::aes(x = Final_bio, xend = Final_bio,
                                                          y = 0, yend = yield/1000, linetype = "B")) +
      ggplot2::scale_linetype_manual(values = c("dashed", "twodash"), labels = c("Current", "MSY"))

  }else if(show_current_line){
    final_data = data |>
      dplyr::filter(data=="final")

    p <- ggplot2::ggplot(data) +
      ggplot2::geom_line(ggplot2::aes(x=Final_bio, y=yield/1000)) +
      ggplot2::geom_segment(data = final_data, ggplot2::aes(x = Final_bio, xend = Final_bio,
                                                            y = 0, yend = yield/1000, linetype = "B")) +
      ggplot2::scale_linetype_manual(values = "dashed", labels = 'Current')

  }else if (show_msy_line){
    if(!"msy" %in% data$data){
      cat("MSY line not provided in plot because 'msy_run' not provided in 'mcmc_yieldplot_prep' \n")
    }

    msy_data = data |>
      filter(data=="msy")

    p <- ggplot2::ggplot(data) +
      ggplot2::geom_line(ggplot2::aes(x=Final_bio, y=yield/1000)) +
      ggplot2::geom_segment(data = msy_data, ggplot2::aes(x = Final_bio, xend = Final_bio,
                                                          y = 0, yend = yield/1000, linetype = "A")) +
      ggplot2::scale_linetype_manual(values = "twodash", labels = "MSY")

  }else{
    p <- ggplot2::ggplot(data) +
      ggplot2::geom_line(ggplot2::aes(x=Final_bio, y=yield/1000))
  }
  p <- p +
    ggplot2::theme_bw() +
    ggplot2::xlab(xlab) +
    ggplot2::ylab(ylab) +
    ggplot2::scale_x_continuous(breaks = c(0,0.2, 0.4, 0.6, 0.8, 1.0), limits=c(0,1.01)) +
    ggplot2::scale_y_continuous(limits=c(0,NA)) +
    ggplot2::theme(legend.title = ggplot2::element_blank(),
                   legend.position = "top",
                   legend.background = ggplot2::element_blank(),
                   legend.text = ggplot2::element_text(size=text_size)) +
    ggplot2::theme(text = ggplot2::element_text(size=text_size))

  if(length(unique(data$scenario)) != 1){
    p <- p +
      ggplot2::facet_wrap(~scenario_labels, scales = scales, ncol=ncol)
  }
  return(p)

}
