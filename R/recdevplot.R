# Copyright 2024 Fisheries Queensland

# This file is part of SSAND.
# SSAND is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
# SSAND is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
# You should have received a copy of the GNU General Public License along with SSAND. If not, see <https://www.gnu.org/licenses/>.

#' Recruitment deviation plot
#'
#' @param data Output from recdevplot_prep().
#' For MLE, a dataframe with variables called year (int), value (int), ub (num), lb (num), median (num, if MCMC==TRUE), scenario (factor), method (chr).
#' For MCMC, a dataframe with variables called rownum (num), scenario (int), year (num), value (num), med (chr), interval (num), prob_lower (num), prob_upper (num)"
#' @param xlab Label for x-axis (character). Default is "Year".
#' @param ylab Label for y-axis (character). Default is "Log recruitment deviation".
#' @param point_size Size of points used in ggplot2::geom_line(). Default is 1.5.
#' @param text_size Text size (num). Default is 12.
#' @param show_median Logical, TRUE to show median line
#' @param colours A vector of colours used (character).
#' @param scales Scales for ggplot2::facet_wrap(). Default is 'free', see ?ggplot2::facet_wrap for options.
#' @param ncol Number of columns for facet_wrap(). Default is 3.
#' @param scenarios A vector of scenario numbers to be shown on plot (numeric). This was already specified in prep file, but this is a manual override to save running the prep function again.
#' @param scenario_labels A vector of customised scenario names (character). Default is "Scenario 1", "Scenario 2", etc.
#' @param scenario_order A vector to reorder how scenarios are displayed (character). Use the label names defined in "scenario_labels".
#' If "scenario_labels" is left blank, the labels will be "Scenario 1", "Scenario 2" etc.
#' Any scenarios not included in "scenario_order" will be tacked on in the order they appear in the input data.
#' @param financial_year Set to TRUE if the assessment was based on financial year (logical). Adjusts the x-axis to show full financial year notation.
#' @param xbreaks A vector of breaks between x-axis labels, used in ggplot2::scale_x_continous() (numeric).
#' @param ybreaks A vector of breaks between y-axis labels, used in ggplot2::scale_y_continous() (numeric).
#' @param xlabels A vector of labels for the x-axis breaks.
#' @param xlim A vector of lower and upper x-axis limits (e.g. c(1950, 2020)) (numeric).
#' @param ylim A vector of lower and upper y-axis limits (e.g. c(0,1)) (numeric).
#' @param ylabels A vector of labels for the y-axis breaks.
#' @param xangle Set to 90 to rotate x-axis labels 90 degrees.
#' @param line_type A vector of linetypes (e.g. "solid", "dashed") for median lines.
#' @param mcmc_style The type of MCMC plot to be displayed (character). Options are "banded", "hairy", "boxplot", "CI" and "joy", the default is "banded". Only one option can be selected.
#' @param aggregate_scenarios Set to TRUE to calculate credible intervals across all scenarios (logical). Only activated if mcmc_style==CI.
#' @param alpha Transparency for range (numeric) used in ggplot2::geom_density_ridges(). Default is 0.7.
#' @param line_width Width of median lines (numeric). Default is 1.
#' @param hair_width Width of fine MCMC hairs (numeric). Default is 0.5.
#' @param legend_box Display option for legend (character). Choose "vertical" to stack legend types vertically, or "horizontal" to keep legends in one row.
#' @param legend_position Position of the legend ("none", "left", "right", "bottom", "top", or two-element numeric vector for x and y position). Default is "top".
#' @param band_colour Colour of bands (character). Only used when mcmc_style=="banded". Input one colour, bands will be distinguished using an alpha.
#'
#' @return Plot grid of recruitment deviations
#' @export
#'
#' @examples
#' data <- recdevplot_prep_SS(ss_mle, ss_mcmc)
#' recdevplot(data)
#' recdevplot(data, mcmc_style = "banded")
#'
#' data <- recdevplot_prep_DD(dd_mle)
#' recdevplot(data)
recdevplot <- function(data,
                       scales='free',
                       ncol = 3,
                       xlab = "Year",
                       ylab = "Log recruitment deviation",
                       xbreaks = NULL,
                       ybreaks = NULL,
                       xlabels = NULL,
                       ylabels = NULL,
                       xlim = NULL,
                       ylim = NULL,
                       xangle = NULL,
                       point_size = 1.5,
                       text_size = 12,
                       # colours = c("#CF3B16"),
                       colours = c("black","darkred"),
                       line_type = c("solid","dashed"),
                       scenarios = NULL,
                       scenario_labels = NULL,
                       scenario_order = NULL,
                       financial_year = FALSE,
                       mcmc_style = "boxplot",
                       show_median = c("median_recdevs","trajectory"),
                       aggregate_scenarios = FALSE,
                       alpha = 0.7,
                       line_width = 0.7,
                       hair_width = 0.5,
                       legend_box = "horizontal",
                       legend_position= "top",
                       band_colour = "black"
                       ){

  MCMC <- "med" %in% names(data)

  # Data input warnings
  if (!MCMC & !"year" %in% names(data)) {warning("Input data is missing year column")}
  if (!MCMC & !"value" %in% names(data)) {warning("Input data is missing value column")}
  if (!MCMC & !"ub" %in% names(data)) {warning("Input data is missing ub column")}
  if (!MCMC & !"lb" %in% names(data)) {warning("Input data is missing lb column")}
  if (!MCMC & !"scenario" %in% names(data)) {warning("Input data is missing scenario column")}
  if (!MCMC & !"method" %in% names(data)) {warning("Input data is missing method column")}


  if (MCMC & !"rownum" %in% names(data)) {warning("Input data is missing rownum column")}
  if (MCMC & !"scenario" %in% names(data)) {warning("Input data is missing scenario column")}
  if (MCMC & !"year" %in% names(data)) {warning("Input data is missing year column")}
  if (MCMC & !"value" %in% names(data)) {warning("Input data is missing value column")}
  if (MCMC & !"med" %in% names(data)) {warning("Input data is missing med column")}
  if (MCMC & !"interval" %in% names(data)) {warning("Input data is missing interval column")}
  if (MCMC & !"prob_lower" %in% names(data)) {warning("Input data is missing prob_lower column")}
  if (MCMC & !"prob_upper" %in% names(data)) {warning("Input data is missing prob_upper column")}

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
  if (missing(ylim) & !MCMC) {ylim <- c(min(data$lb,na.rm=T),max(data$ub,na.rm=T))}
  if (missing(ylim) & MCMC) {ylim <- c(min(data$prob_lower,na.rm=T),max(data$prob_upper,na.rm=T))}

  if (missing(xbreaks)) {xbreaks <- pretty(xlim)}
  if (missing(ybreaks)) {ybreaks <- pretty(ylim)}

  if (missing(xlabels)) {xlabels <- xbreaks}
  if (missing(ylabels)) {ylabels <- ybreaks}

  if (financial_year) {xlabels <- paste0(xbreaks-1,"\U2013",xbreaks)} else {xlabels <- xbreaks}
  if (missing(xangle)) {xangle <- ifelse(financial_year,90,0)}

  if (!MCMC) {
    p <- ggplot2::ggplot(data) +
      ggplot2::geom_point(ggplot2::aes(x=year,y=value), size=point_size)+
      ggplot2::geom_errorbar(ggplot2::aes(x=year,ymin=ub, ymax=lb), width=.5,
                             position=ggplot2::position_dodge(0)) +
      ggplot2::geom_line(ggplot2::aes(x=year, y=value), linetype= "dotted", linewidth=0.7) +
      ggplot2::geom_hline(yintercept=0, colour="grey") +
      ggplot2::theme_bw() +
      ggplot2::xlab(xlab) +
      ggplot2::ylab(ylab) +
      ggplot2::theme(text = ggplot2::element_text(size=text_size))+
      ggplot2::theme(panel.grid.major = ggplot2::element_line(colour = "white"),
                     panel.grid.minor = ggplot2::element_line(colour = "white")) +
      ggplot2::scale_x_continuous(limits = xlim, breaks = xbreaks, labels = xlabels) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = xangle, vjust = 0.5, hjust=ifelse(xangle==90,0,0.5)))


    if (length(unique(data$scenario))>1){
      p <- p +
        ggplot2::facet_wrap(~scenario_labels, scales = scales, ncol = ncol)
    }

    return(p)
  }

  if (MCMC) {

    # MCMC warnings
    if (MCMC & !any(show_median %in% c('median_recdevs', 'trajectory', 'none'))){warning('Please check show_median options using ?recdevplot')}
    if (MCMC & any(show_median %in% c('Parameters'))){warning('Median parameters feature not available yet.')}
    if (MCMC & length(mcmc_style)>1) {warning("You can only select one mcmc_type at a time.")}

    # Box plot
    if (mcmc_style == "boxplot") {
      databox <- data |>
        dplyr::filter(rownum > 0)

      # Expand limits of x-axis to include box
      xlim[1] <- xlim[1]-0.5
      xlim[2] <- xlim[2]+0.5

      p <- ggplot2::ggplot(data) +
        ggplot2::geom_boxplot(data = databox, ggplot2::aes(x=year, y=value, group=year))
    }

    # Banded plot
    if (mcmc_style == "banded") {
      tmp <- unique(data$interval)[!is.na(unique(data$interval))]
      alpha_scale <- seq(round(1/length(tmp),2),1,round(1/length(tmp),2))^2 + 0.1
      alpha_scale <- alpha_scale/max(alpha_scale)

      p <- ggplot2::ggplot(data) +
        ggplot2::geom_ribbon(data = data |> dplyr::filter(!is.na(interval)),
                             ggplot2::aes(x=year, ymin=prob_lower, ymax=prob_upper, group=interval, alpha=as.factor(-interval)),
                             fill=band_colour) +
        ggplot2::scale_alpha_manual(values = alpha_scale,
                                    labels = rev(unique(data$interval)[!is.na(unique(data$interval))]),
                                    name = "Credible interval")
    }

    # Hairy plot
    if (mcmc_style == "hairy") {
      p <- ggplot2::ggplot(data) +
        ggplot2::geom_line(data = data |> dplyr::filter(med == "MCMC"),
                           ggplot2::aes(x=year,y=value, group=rownum), colour = 'grey20', linewidth=hair_width, alpha = 1)
    }


    # Credible interval
    if (mcmc_style == "CI") {
        dataCI <- data |>  dplyr::filter(med=="CI") |> dplyr::mutate(med=paste0(interval*100,"% credible interval"))
      p <- ggplot2::ggplot(data) +
        ggplot2::geom_ribbon(data = dataCI, ggplot2::aes(x=year, ymax=prob_upper, ymin = prob_lower, fill = med), alpha = alpha) +
        ggplot2::scale_fill_manual(name="", values="grey60")
    }

    # Add median lines
    if (!"none" %in% show_median) {
      data_med <- data |>
        dplyr::filter(med %in% show_median) |>
        dplyr::mutate(med = dplyr::recode(med,
                                          "median_recdevs" = "Median recruitment deviations",
                                          "trajectory" = "Median trajectory",
                                          "parameters" = "Median parameters"))

      p <- p +
        ggplot2::geom_line(data=data_med, ggplot2::aes(x=year,y=value, colour=med, linetype=med), linewidth=line_width) +
        ggplot2::scale_color_manual(values = colours, name = ggplot2::element_blank()) +
        ggplot2::scale_linetype_manual(values= line_type, name=ggplot2::element_blank())
    }

    p <- p +
      ggplot2::scale_x_continuous(limits = xlim, breaks = xbreaks, labels = xlabels) +
      ggplot2::scale_y_continuous(limits = ylim, breaks = ybreaks, labels = ylabels) +
      ggplot2::theme_bw() +
      ggplot2::xlab(xlab) +
      ggplot2::ylab(ylab) +
      ggplot2::theme(legend.position=legend_position) +
      ggplot2::theme(legend.text = ggplot2::element_text(size=text_size)) +
      ggplot2::theme(text = ggplot2::element_text(size=text_size)) +
      ggplot2::theme(legend.box=legend_box) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = xangle, vjust = 0.5, hjust=ifelse(xangle==90,0,0.5)))

    # Facet wrap
    if (length(unique(data$scenario))>1) {
      suppressMessages({
        p <- p +
          ggplot2::scale_x_continuous(limits = xlim, breaks = xbreaks, labels = xlabels) +
          ggplot2::scale_y_continuous(limits = ylim, breaks = ybreaks, labels = ylabels) +
          ggplot2::facet_wrap(~scenario_labels, ncol = ncol, scales = scales)

      })
    }

    return(p)
  }
}
