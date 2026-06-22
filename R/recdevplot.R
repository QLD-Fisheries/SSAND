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
#' @param line_type A vector of linetypes (e.g. "solid", "dashed") for median lines.
#' @param mcmc_style The type of MCMC plot to be displayed (character). Options are "banded", "hairy", "boxplot" and "CI", the default is "banded". Only one option can be selected.
#' @param aggregate_scenarios Set to TRUE to calculate credible intervals across all scenarios (logical). Only activated if mcmc_style==CI.
#' @param CI_range Specify credible interval range (numeric). Only activated if mcmc_style==CI.
#' @param alpha Transparency for range (numeric) used in ggplot2::geom_density_ridges(). Default is 0.7.
#' @param line_width Width of median lines (numeric). Default is 1.
#' @param hair_width Width of fine MCMC hairs (numeric). Default is 0.5.
#' @param sample Number of samples to plot from each MCMC chain to ease burden of rendering dense plots (numeric).
#' @param band_colour Colour of bands (character). Only used when mcmc_style=="banded". Input one colour, bands will be distinguished using an alpha.
#' @param band_labels Labels for bands. Default is NULL and interval is used.
#' @param boxplot_outliers Set to FALSE to remove outlier points from boxplot. Default is TRUE.
#' @param text_size,legend_text_size,text_colour,legend_text_colour,legend_position,legend_box,legend_title_blank,panel_border,panel_border_colour,xangle Optional plotting theme overrides. Defaults are controlled by `theme_ssand()`
#'   and can be set globally via `set_ssand_style()`.
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
                       point_size = 1.5,
                       colours = c("black","darkred"),
                       line_type = c("solid","dashed"),
                       sample = NULL,
                       scenarios = NULL,
                       scenario_labels = NULL,
                       scenario_order = NULL,
                       mcmc_style = "boxplot",
                       show_median = c("median_recdevs","trajectory"),
                       aggregate_scenarios = FALSE,
                       alpha = NULL,
                       line_width = 0.7,
                       hair_width = 0.5,
                       band_colour = "black",
                       band_labels = NULL,
                       boxplot_outliers = TRUE,
                       CI_range = 0.95,
                       xangle = NULL,
                       legend_position = NULL,
                       financial_year = FALSE,
                       text_size = NULL,
                       legend_text_size = NULL,
                       text_colour = NULL,
                       legend_text_colour = NULL,
                       legend_box = NULL,
                       legend_title_blank = NULL,
                       panel_border = NULL,
                       panel_border_colour = NULL) {

  # ___________________
  # Data validation
  # ___________________
  # Identify MCMC or MLE
  MCMC <- "med" %in% names(data)

  # Data input warnings
  if (!MCMC) check_data_columns(data, c("year","value","ub","lb","scenario","method"))
  if (MCMC)  check_data_columns(data, c("rownum","scenario","year","value","med","interval","prob_lower","prob_upper"))

  # MCMC warnings
  show_median <- simplify_show_median(show_median, c("median_recdevs","trajectory","none"))

  check_mcmc_style(mcmc_style)
  if (!MCMC) {data$upper <- data$ub; data$lower <- data$lb}
  if (MCMC)  {data$upper <- data$prob_upper; data$lower <- data$prob_lower}
  if (MCMC) data$med[startsWith(data$med, "median_")] <- "annual"
  if (MCMC) data <- sample_mcmc_runs(data, sample)
  data$xvar <- data$year

  # Determine axis settings if missing
  facet_wrap <- length(unique(data$scenario))>1 & !aggregate_scenarios

  if (is.null(alpha) & mcmc_style !="banded") {alpha=0.7}

  # ___________________
  # Basic plot set up
  # ___________________
  data <- apply_scenarios(data, scenarios, scenario_labels, scenario_order)

  p <- ggplot2::ggplot(data)

  # ___________________
  # Build MLE plot
  # ___________________
  if (!MCMC) {
    p <- p +
      ggplot2::geom_point(ggplot2::aes(x=year,y=value), size=point_size)+
      ggplot2::geom_errorbar(ggplot2::aes(x=year,ymin=upper, ymax=lower), width=.5,
                             position=ggplot2::position_dodge(0)) +
      ggplot2::geom_line(ggplot2::aes(x=year, y=value), linetype= "dotted", linewidth=0.7) +
      ggplot2::geom_hline(yintercept=0, colour="grey")
  }

  # ___________________
  # Build MCMC plot
  # ___________________
  if (MCMC) {
    if (mcmc_style == "boxplot") p <- mcmc_boxplot(p, data, xlim, boxplot_outliers)
    if (mcmc_style == "banded")  p <- mcmc_banded(p, data, alpha, band_labels, band_colour)
    if (mcmc_style == "hairy")   p <- mcmc_hairy(p, data, hair_width)
    if (mcmc_style == "CI")      p <- mcmc_CI(p, data, aggregate_scenarios, CI_range, alpha)
    # if (mcmc_style == "joy")     p <- mcmc_joy(p, data, CI_range, ridge_colour, rel_min_height, alpha, ridge_scale, show_CI,
    #                                            ybreaks, ylin,ylab, xlab, legend_position, text_size,xbreaks,legend_box,facet_wrap,
    #                                            show_median,xlabels,ylabels)

    # Add median lines
    p <- show_median_lines("recruitment deviations",p,data,show_median,line_width,colours)
  }

  # ___________________
  # Final layers
  # ___________________
  if (facet_wrap)  p <- add_scenario_facets(p, data, scales = scales, ncol = ncol)

  # ___________________
  # Axes and theme
  # ___________________
  x_axis <- build_x_axis(x = data$xvar,
                         xlab = xlab,
                         xlim = xlim,
                         xbreaks = xbreaks,
                         xlabels = xlabels,
                         financial_year = financial_year,
                         show_dates_on_axis = FALSE,
                         expand_upper = 0,
                         xangle = xangle,
                         is_date = FALSE)

  y_axis <- build_y_axis(y = data$value,
                         ylab = ylab,
                         ylim = ylim,
                         ybreaks = ybreaks,
                         ylabels = ylabels,
                         lower = data$lower,
                         upper = data$upper)

  p <- add_x_scale_continuous(p, x_axis)
  p <- add_y_scale_continuous(p, y_axis)

  p <- add_ssand_theme(p,
                       text_size = text_size,
                       legend_text_size = legend_text_size,
                       text_colour = text_colour,
                       legend_text_colour = legend_text_colour,
                       legend_position = legend_position,
                       legend_box = legend_box,
                       legend_title_blank = legend_title_blank,
                       panel_border = panel_border,
                       panel_border_colour = panel_border_colour,
                       xangle = x_axis$angle)

  return(p)
}
