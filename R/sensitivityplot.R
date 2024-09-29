# Copyright 2024 Fisheries Queensland

# This file is part of SSAND.
# SSAND is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
# SSAND is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
# You should have received a copy of the GNU General Public License along with SSAND. If not, see <https://www.gnu.org/licenses/>.

#' Sensitivity plot
#'
#' @param data Output from sensitivityplot_prep(). Data frame with scenario (int), name (chr), value (num), ub (num), lb (num), fixed (log).
#' @param parameters A vector of parameters to include on plot (character). This was already specified in prep file, but this is a manual override to save running the prep function again.
#' @param colours A vector of colours used (character).
#' @param scenarios A vector of scenario numbers to be shown on plot (numeric). This was already specified in prep file, but this is a manual override to save running the prep function again.
#' @param scenario_labels A vector of customised scenario names (character). Default is "Scenario 1", "Scenario 2", etc.
#' @param scenario_order A vector to reorder how scenarios are displayed (character). Use the label names defined in "scenario_labels".
#' If "scenario_labels" is left blank, the labels will be "Scenario 1", "Scenario 2" etc.
#' Any scenarios not included in "scenario_order" will be tacked on in the order they appear in the input data.
#' @param parameter_labels A vector of customised facet labels for different parameters (character).
#' @param ncol Number of columns for facet_wrap(). Default is 2.
#' @param scales Scales for ggplot2::facet_wrap(). Default is 'free', see ?ggplot2::facet_wrap for options.
#' @param show_x_labels Set to TRUE to include scenario names on the x-axis (logical).
#' @param xangle Set to 90 to rotate x-axis labels 90 degrees.
#' @param xlab Label for x-axis (character). Default is "Scenario".
#' @param ylab Label for y-axis (character). Default is "".
#'
#' @return This function produces comparison plot of parameter estimates across all scenarios.
#' @export
#'
#' @examples
#' parm <- extract_SS_parameters(ss_mle)[c(3:6),]
#' data <- sensitivityplot_prep_SS(ss_mle,
#'                                 ss_mcmc,
#'                                 show_MSY = TRUE,
#'                                 show_LL = TRUE,
#'                                 show_B_ratio = TRUE,
#'                                 parameters = parm)
#' sensitivityplot(data)
#'
#' data <- sensitivityplot_prep_DD(dd_mle)
#' sensitivityplot(data)
sensitivityplot <- function(data,
                            parameters = NULL,
                            colours = NULL,
                            parameter_labels = NULL,
                            scenarios = NULL,
                            scenario_labels = NULL,
                            scenario_order = NULL,
                            ncol = 2,
                            scales = 'free',
                            show_x_labels = FALSE,
                            xangle = NULL,
                            xlab = "Scenario",
                            ylab = "") {

  # Data input warnings
  if (!"scenario" %in% names(data)) {warning("Input data is missing scenario column")}
  if (!"name" %in% names(data)) {warning("Input data is missing name column")}
  if (!"value" %in% names(data)) {warning("Input data is missing value column")}
  if (!"ub" %in% names(data)) {warning("Input data is missing ub column")}
  if (!"lb" %in% names(data)) {warning("Input data is missing lb column")}
  if (!"fixed" %in% names(data)) {warning("Input data is missing fixed column")}

  if (!missing(scenarios)){data <- data |> dplyr::filter(scenario %in% scenarios)}

  if (missing(xangle)) {
    xangle <- 0
    vjust <- 0.5
    hjust <- 0.5
  } else{
    vjust <- 1
    hjust <- 1
  }

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

  # Filter if specific list of parameters is given
  if (!missing(parameters)) {
    data <- data |> dplyr::filter(name %in% parameters)
  }

  # Tidy up any DDUST variable names
  data <- data |>
    dplyr::mutate(name = dplyr::case_when(name=='Rinit' ~ 'R[init]',
                                          name=='k' ~ 'kappa',
                                          name=='q1' ~ 'q[1]',
                                          name=='q2' ~ 'q[2]',
                                          name=='lsigmaR_sq' ~ 'log(sigma[R]^2)',
                                          name=='lsigmaI_sq' ~ 'log(sigma[I]^2)',
                                          TRUE ~ name))

  # set ylims for each facet plot
  ylimits <- dplyr::summarise(dplyr::group_by(data, name), ymin = min(lb)*0.9,
                              ymax = max(ub)*1.1, .groups='drop')

  # Set min Bratio to 0
  ylimits <- ylimits |> dplyr::mutate(ymin = ifelse(substr(name,1,2)=="B[",0,ymin))

  data <- dplyr::left_join(data, ylimits, by='name')

  data$scenario_labels <- as.factor(data$scenario_labels)
  data$name <- as.factor(data$name)

  if (!missing(parameter_labels)) {
    levels(data$name) <- parameter_labels
  }

  # Plot
  p <- ggplot2::ggplot(data, ggplot2::aes(x=scenario_labels, y = value)) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin=lb, ymax=ub, colour=as.factor(scenario_labels)), width = .2) +
    ggplot2::geom_point(ggplot2::aes(x=scenario_labels, y=value, colour=as.factor(scenario_labels), shape = fixed),size=3) +
    # ggplot2::facet_wrap(~name, ncol=ncol, scales=scales, labeller = ggplot2::label_parsed(parameter_labels))
    ggplot2::facet_wrap(~name, ncol=ncol, scales=scales, labeller = ggplot2::label_parsed)

  if (missing(colours)) {
    p <- p +
      ggplot2::scale_colour_manual(values=fq_palette("cols"))
  } else {
    p <- p +
      ggplot2::scale_colour_manual(values=colours)
  }



  p <- p +
    ggplot2::scale_shape_manual(values=c(19,15), labels=c("Estimated","Fixed")) +
    ggplot2::geom_blank(ggplot2::aes(y = ymin)) +
    ggplot2::geom_blank(ggplot2::aes(y = ymax)) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position="top", legend.title = ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_text(angle = xangle, hjust = hjust)) +
    ggplot2::xlab(xlab) +
    ggplot2::ylab(ylab)

  if (!show_x_labels) {
    p <- p +
      ggplot2::theme(axis.text.x=ggplot2::element_blank())
  }

  return(p)

}

