# Copyright 2024 Fisheries Queensland

# This file is part of SSAND.
# SSAND is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
# SSAND is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
# You should have received a copy of the GNU General Public License along with SSAND. If not, see <https://www.gnu.org/licenses/>.

#' Growth plot
#'
#' @param data A data frame with variables age (int), value (num), lower (num), upper (num), sex (int), scenario (int)
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
#' @param variation_on_variation Set to TRUE to illustrate MCMC variation on the CV or SD values. Default (FALSE) sets the variation in growth to the median MCMC value.
#'
#' @return Growth plot
#' @export
#'
#' @examples
#' data <- growthplot_prep_SS(ss_mle)
#' growthplot(data)
#'
#' data <- growthplot_prep_DD(dd_mle)
#' growthplot(data)
growthplot <- function(data,
                       xlab = "Age",
                       ylab = "Carapace length (cm, beginning of year)",
                       text_size = 12,
                       show_two_sex=NULL,
                       scenarios = NULL,
                       scenario_labels = NULL,
                       scenario_order = NULL,
                       colours = NULL,
                       scales = 'free',
                       ncol = 2,
                       variation_on_variation = FALSE) {

  if ("CV_lower"%in%names(data)) {MCMC<-TRUE} else {MCMC<-FALSE}

  # Data input warnings
  if (!"age" %in% names(data)) {warning("Input data is missing age column")}
  if (!"value" %in% names(data)) {warning("Input data is missing value column")}
  if (!"lower" %in% names(data)) {warning("Input data is missing lower column")}
  if (!"upper" %in% names(data)) {warning("Input data is missing upper column")}
  if (!"sex" %in% names(data)) {warning("Input data is missing sex column")}
  if (!"scenario" %in% names(data)) {warning("Input data is missing scenario column")}
  if (MCMC) {
    if (!"CV_lower" %in% names(data)) {warning("Input data is missing CV_lower column")}
    if (!"CV_middle" %in% names(data)) {warning("Input data is missing CV_middle column")}
    if (!"CV_upper" %in% names(data)) {warning("Input data is missing CV_upper column")}
    if (!"growthCVtype" %in% names(data)) {warning("Input data is missing growthCVtype column")}
  }

  if (missing(show_two_sex)) {
    tmp1 <- 1 %in% data$sex
    tmp2 <- 2 %in% data$sex
    show_two_sex <- tmp1 & tmp2
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


  if (missing(colours)) {
    if (show_two_sex) {
      colours = c("#F4BB48","#248BB7","#6BA357") # yellow, blue, green
    } else {
      colours = "#9D9D9D" # grey colour for single sex plot
    }
  }

  if(show_two_sex) {
    dataF <- data[data$sex == 1, ]
    dataM <- data[data$sex == 2, ]
    p <- ggplot2::ggplot() +
      ggplot2::theme_bw() +
      ggplot2::xlab(xlab) +
      ggplot2::ylab(ylab) +
      ggplot2::geom_line(data = dataF, ggplot2::aes(x=age, y=value, colour = "A", linetype = "A"), linewidth=1.05) +
      ggplot2::geom_line(data = dataF,ggplot2::aes(x=age, y=lower, colour = "A"), linetype="dotted") +
      ggplot2::geom_line(data = dataF,ggplot2::aes(x=age, y=upper, colour = "A"), linetype="dotted") +
      ggplot2::geom_ribbon(data = dataF,ggplot2::aes(x=age, ymin=lower, ymax=upper), fill=colours[1], alpha=0.2) +
      ggplot2::theme(text = ggplot2::element_text(size=text_size)) +
      ggplot2::geom_line(data = dataM, ggplot2::aes(x=age, y=value, colour = "B", linetype = "B"),  linewidth=1.05) +
      ggplot2::geom_line(data = dataM, ggplot2::aes(x=age, y=lower, colour = "B"), linetype="dotted") +
      ggplot2::geom_line(data = dataM, ggplot2::aes(x=age, y=upper, colour = "B"), linetype="dotted") +
      ggplot2::geom_ribbon(data = dataM, ggplot2::aes(x=age, ymin=lower, ymax=upper), fill=colours[2], alpha=0.2) +
      ggplot2::scale_colour_manual(c("", ""),values=colours, labels = c("Female", "Male")) +
      ggplot2::scale_linetype_manual(c("", ""),values=c("solid","dashed"), labels = c("Female", "Male")) +
      ggplot2::theme(legend.position = "top")
  } else {
    p <- ggplot2::ggplot(data) +
      ggplot2::theme_bw() +
      ggplot2::xlab(xlab) +
      ggplot2::ylab(ylab) +
      ggplot2::geom_line(ggplot2::aes(x=age, y=value), colour= colours, linewidth=1.05) +
      ggplot2::theme(text = ggplot2::element_text(size=text_size))

    if (prod(!is.na(data$lower))){
      p <- p +
        ggplot2::geom_line(ggplot2::aes(x=age, y=lower), colour=colours, linetype="dotted") +
        ggplot2::geom_line(ggplot2::aes(x=age, y=upper), colour=colours, linetype="dotted") +
        ggplot2::geom_ribbon(ggplot2::aes(x=age, ymin=lower, ymax=upper), fill=colours, alpha=0.2)
    }
  }

  if (length(unique(data$scenario))>1){
    p <- p +
      ggplot2::facet_wrap(~scenario_labels, scales = scales, ncol = ncol)
  }


  if (MCMC) {
    if (!variation_on_variation) {
      p <- ggplot2::ggplot(data) +
        ggplot2::geom_ribbon(ggplot2::aes(x = age, ymin = value-1.96*CV_middle, ymax= value+1.96*CV_middle, fill = "Variation in growth using median standard deviation")) +
        ggplot2::geom_ribbon(ggplot2::aes(x = age, ymin = lower, ymax= upper, fill = "95% credible interval")) +
        ggplot2::geom_line(ggplot2::aes(x = age, y = value, linetype = "Median growth"), colour = "grey30") +
        ggplot2::scale_fill_manual("", values=c("grey65","grey85")) +
        ggplot2::scale_linetype_manual("", values=c("solid","dashed")) +
        ggplot2::theme_bw() +
        ggplot2::xlab(xlab) +
        ggplot2::ylab(ylab) +
        ggplot2::theme(legend.position="top") +
        ggplot2::coord_cartesian(ylim = c(-0, NA), xlim = c(0,NA)) # alternative to ylim that doesn't cut off ribbons
    } else {
      p <- ggplot2::ggplot(data) +
        ggplot2::geom_ribbon(ggplot2::aes(x = age, ymin = value-1.96*CV_lower, ymax = value-1.96*CV_upper, fill = "95% credible interval for variation in growth")) +
        ggplot2::geom_ribbon(ggplot2::aes(x = age, ymin = value+1.96*CV_lower, ymax = value+1.96*CV_upper, fill = "95% credible interval for variation in growth")) +
        ggplot2::geom_line(ggplot2::aes(x = age, y = value-1.96*CV_middle, linetype="Variation in growth")) +
        ggplot2::geom_line(ggplot2::aes(x = age, y = value+1.96*CV_middle, linetype="Variation in growth")) +
        ggplot2::geom_ribbon(ggplot2::aes(x = age, ymin = lower, ymax= upper, fill = "95% credible interval for growth")) +
        ggplot2::geom_line(ggplot2::aes(x = age, y = value, linetype = "Growth"), colour = "grey30") +
        ggplot2::scale_fill_manual("", values=c("grey65","grey85")) +
        ggplot2::scale_linetype_manual("", values=c("solid","dashed")) +
        ggplot2::theme_bw() +
        ggplot2::xlab(xlab) +
        ggplot2::ylab(ylab) +
        ggplot2::theme(legend.position="top") +
        ggplot2::coord_cartesian(ylim = c(-0, NA), xlim = c(0,NA)) # alternative to ylim that doesn't cut off ribbons
    }
  }
  return(p)
}
