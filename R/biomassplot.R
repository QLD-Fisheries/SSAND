# Copyright 2024 Fisheries Queensland

# This file is part of SSAND.
# SSAND is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
# SSAND is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
# You should have received a copy of the GNU General Public License along with SSAND. If not, see <https://www.gnu.org/licenses/>.

#' Biomass plot
#'
#' MLE biomass plot displays 95% confidence interval
#' Multiple options are available for MCMC plots.
#' The variable "mcmc_type" can be set to "hairy", "boxplot", "banded", "joy" or "CI" to display different plot types.
#' On top of this, one or more median types can be overlaid using "show_median".
#' The types of median to be shown are "annual_biomass" or "trajectory".
#'
#' @param data If MCMC is being used, a data frame with variables called rownum, scenario, year, value, med, interval, prob_lower, prob_upper, biomass_type, biomass_definition. If MLE, a data frame with variables called year, value, lower, upper, scenario, biomass_type, biomass_definition.
#' @param xlab Label for x-axis (character). Default is "Year".
#' @param ylab Label for y-axis (character). Default is "Spawning biomass (relative)".
#' @param xbreaks A vector of breaks between x-axis labels, used in ggplot2::scale_x_continous() (numeric).
#' @param ybreaks A vector of breaks between y-axis labels, used in ggplot2::scale_y_continous() (numeric).
#' @param xlabels A vector of labels for the x-axis breaks.
#' @param ylabels A vector of labels for the y-axis breaks.
#' @param xlim A vector of lower and upper x-axis limits (e.g. c(1950, 2020)) (numeric).
#' @param ylim A vector of lower and upper y-axis limits (e.g. c(0,1)) (numeric).
#' @param xangle Set to 90 to rotate x-axis labels 90 degrees.
#' @param colours A vector of colours used for median types (character).
#' @param legend_position Position of the legend ("none", "left", "right", "bottom", "top", or two-element numeric vector for x and y position). Default is "top".
#' @param annotation_position Horizontal position of annotation
#' @param financial_year Set to TRUE if the assessment was based on financial year (logical). Adjusts the x-axis to show full financial year notation.
#' @param text_size text_size, default 12
#' @param show_target_line Set to TRUE to show target reference point line (logical).
#' @param target_value target reference point, default is 0.6. Colour for line is second element of colours
#' @param show_limit_line show limit reference point
#' @param limit_value limit reference point, default is 0.2. Colour for line is third element of colours
#' @param scenarios A vector of scenario numbers to be shown on plot (numeric). This was already specified in prep file, but this is a manual override to save running the prep function again.
#' @param scenario_labels A vector of customised scenario names (character). Default is "Scenario 1", "Scenario 2", etc.
#' @param scenario_order A vector to reorder how scenarios are displayed (character). Use the label names defined in "scenario_labels".
#' If "scenario_labels" is left blank, the labels will be "Scenario 1", "Scenario 2" etc.
#' Any scenarios not included in "scenario_order" will be tacked on in the order they appear in the input data.
#' @param scales Scales for ggplot2::facet_wrap(). Default is 'fixed', see ?ggplot2::facet_wrap for options.
#' @param ncol Number of columns for facet_wrap(). Default is 2.
#' @param sample Number of samples to plot from each MCMC chain to ease burden of rendering dense plots (numeric).
#' @param alpha Transparency for range (numeric) used in ggplot2::geom_density_ridges(). Default is 0.7.
#' @param show_median Type of median shown. Default "annual_biomass" shows the median of each year,
#' "trajectory" shows median trajectory based on biomass in final year
#' @param mcmc_style The type of MCMC plot to be displayed (character). Options are "banded", "hairy", "boxplot", "CI" and "joy", the default is "banded". Only one option can be selected.
#' @param aggregate_scenarios Set to TRUE to calculate credible intervals across all scenarios (logical). Only activated if mcmc_style==CI.
#' @param CI_range Specify credible interval range (numeric). Only activated if mcmc_style==CI.
#' @param line_width Width of median lines (numeric). Default is 1.
#' @param hair_width Width of fine MCMC hairs (numeric). Default is 0.5.
#' @param legend_box Display option for legend (character). Choose "vertical" to stack legend types vertically, or "horizontal" to keep legends in one row.
#' @param show_CI Set to TRUE to show CI range on joy plot (logical).
#' @param rel_min_height Only used when mcmc_style=="joy". Lines with heights below this cutoff will be removed (passed to geom_density_ridges). Default is 0.01.
#' @param ridge_scale Only used when mcmc_style=="joy". Scale the height of the ridgelines relative to the spacing between them (passed to geom_density_ridges). Default is 4.5.
#' @param ridge_colour Two-element vector for the fill and outline of ridges (character). Only used when mcmc_style=="joy".
#' @param shapes Vector of shapes to denote different median types. Only used when mcmc_style=="joy".
#' @param band_colour Colour of bands (character). Only used when mcmc_style=="banded". Input one colour, bands will be distinguished using an alpha.
#' @param band_labels Labels for bands. Default is NULL and interval is used.
#' @param show_final_biomass Set to TRUE to show final biomass value at the end of the time series.
#' @param boxplot_outliers Set to FALSE to remove outlier points from boxplot. Default is TRUE.
#'
#' @return Biomass plot
#' @export
#'
#' @examples
#' data <- biomassplot_prep_DD(dd_mle)
#' biomassplot(data)
#'
#' data <- biomassplot_prep_SS(ss_mle, ss_mcmc)
#' biomassplot(data, mcmc_style = "banded", show_median = c("annual_biomass","trajectory"))
#' biomassplot(data, mcmc_style = "boxplot", show_median = c("annual_biomass","trajectory"))
#' biomassplot(data, mcmc_style = "hairy", show_median = c("annual_biomass","trajectory"))
#' biomassplot(data, mcmc_style = "CI", show_median = c("annual_biomass","trajectory"), CI_range = 0.9)
#' biomassplot(data, mcmc_style = "joy", show_median = c("none"))
biomassplot <- function(data,
                        xlab = "Year",
                        ylab = NULL,
                        xbreaks = NULL,
                        ybreaks = NULL,
                        xlabels = NULL,
                        ylabels = NULL,
                        xlim = NULL,
                        ylim = NULL,
                        xangle = NULL,
                        colours = NULL,
                        legend_position= "top",
                        annotation_position = min(data$year)+1,
                        financial_year = FALSE,
                        text_size = 12,
                        show_target_line = TRUE,
                        target_value = 0.6,
                        show_limit_line = TRUE,
                        limit_value = 0.2,
                        scenarios = NULL,
                        scenario_labels = NULL,
                        scenario_order = NULL,
                        scales = 'fixed',
                        ncol = 2,
                        sample = NULL,
                        alpha = NULL,
                        show_median = c("trajectory","annual_biomass"),
                        mcmc_style = "banded", # hairy, boxplot, banded, CI, joy
                        aggregate_scenarios = FALSE,
                        CI_range = 0.95,
                        show_CI = TRUE,
                        line_width = 1,
                        hair_width = 0.5,
                        legend_box = "horizontal",
                        rel_min_height = 0.01,
                        ridge_scale = 4.5,
                        ridge_colour = c("grey30","black"),
                        shapes = c(16,18,17),
                        band_colour = "black",
                        band_labels = NULL,
                        show_final_biomass = FALSE,
                        boxplot_outliers = TRUE) {

  # Identify MCMC or MLE
  MCMC <- "med" %in% names(data)

  # Data input warnings
  if (!MCMC) check_data_columns(data, c("year","value","upper","lower","scenario","biomass_type"))
  if (MCMC)  check_data_columns(data, c("rownum","scenario","year","value","interval","prob_lower","prob_upper","biomass_type"))

  # MCMC warnings
  show_median <- simplify_show_median(show_median, c("annual_biomass","trajectory","none"))

  check_mcmc_style(mcmc_style)
  if (MCMC) {data$upper <- data$prob_upper; data$lower <- data$prob_lower}
  if (MCMC) data$med[startsWith(data$med, "annual_")] <- "annual"
  if (MCMC) data <- sample_mcmc_runs(data, sample)
  data$xvar <- data$year
  if (is.null(alpha) & mcmc_style !="banded") {alpha=0.7}

  # ___________________
  # Custom to this plot
  biomass_type <- data$biomass_type[1]
  biomass_definition_label <- ifelse(data$biomass_definition == 'spawning', 'Spawning', 'Vulnerable')

  if (is.null(ylab)) {
    ylab <- ifelse(biomass_type=="relative",
                   paste0(biomass_definition_label," biomass (relative)"),
                   paste0(biomass_definition_label," biomass"))
  }

  # Determine axis settings if missing
  facet_wrap <- length(unique(data$scenario))>1 & !aggregate_scenarios

  # Determine aesthetics if missing
  if (is.null(colours)) {
    if (!MCMC) colours <- "black"
    if (MCMC) colours <- c("#7CC8FC", "#FFC000", "#773158", "#01917C")
  }

  if (biomass_type == "absolute") {
    data <- data |>
      dplyr::group_by(scenario_labels) |>
      dplyr::mutate(target_value = ifelse(dplyr::row_number()==1, dplyr::first(value)*target_value, NA),
                    limit_value  = ifelse(dplyr::row_number()==1, dplyr::first(value)*limit_value, NA)) |>
      dplyr::ungroup() |>
      dplyr::mutate(annotation_position = annotation_position)
  }

  # ___________________
  data <- apply_scenarios(data,
                          scenarios = scenarios,
                          scenario_labels = scenario_labels,
                          scenario_order = scenario_order)

  x_axis <- build_x_axis(x = data$xvar,
                         xlab = xlab,
                         xlim = xlim,
                         xbreaks = xbreaks,
                         xlabels = xlabels,
                         financial_year = financial_year,
                         expand_upper = as.numeric(show_final_biomass),
                         xangle = xangle,
                         is_date = FALSE)

  y_axis <- build_y_axis(y = data$value,
                         ylab = ylab,
                         ylim = ylim,
                         ybreaks = ybreaks,
                         ylabels = ylabels,
                         lower = 0,
                         upper = data$upper)

  # ___________________
  # Initiate plot
  p <- ggplot2::ggplot(data) + get_theme_ssand()
  p <- add_x_scale_continuous(p, x_axis)
  p <- add_y_scale_continuous(p, y_axis)

  # Build MLE plot
  if (!MCMC) {
    p <- p +
      ggplot2::geom_line(ggplot2::aes(x=year,y=value, colour="A")) +
      ggplot2::scale_colour_manual(c("","",""),values=colours,labels = c("Estimate")) +
      ggplot2::scale_fill_manual("",values="grey12")

    if (show_CI) {
      p <- p +
        ggplot2::geom_ribbon(ggplot2::aes(x=year,ymin=lower,ymax=upper,fill="95% confidence interval"), alpha = 0.2)
    }
  }

  # Build MCMC plot
  if (MCMC) {
    if (mcmc_style == "boxplot") p <- mcmc_boxplot(p, data, xlim, boxplot_outliers)
    if (mcmc_style == "banded")  p <- mcmc_banded(p, data, alpha, band_labels, band_colour)
    if (mcmc_style == "hairy")   p <- mcmc_hairy(p, data, hair_width)
    if (mcmc_style == "CI")      p <- mcmc_CI(p, data, aggregate_scenarios, CI_range, alpha)
    if (mcmc_style == "joy")     p <- mcmc_joy(p, data, CI_range, ridge_colour, rel_min_height, alpha, ridge_scale, show_CI,
                                               ybreaks, ylin,ylab, xlab, legend_position, text_size,xbreaks,legend_box,facet_wrap,
                                               show_median,xlabels,ylabels)
    # Add median lines
    p <- show_median_lines("biomass",p,data,show_median,line_width,colours)
  }

  # Customisable features
  if (show_final_biomass) p <- show_final_biomass(p, data, MCMC, colour_categories,scenario_labels)
  if (show_target_line)   p <- add_reference_line(p, data[1,], target_value, "#127B06", annotation_position, "Target reference point")
  if (show_limit_line)    p <- add_reference_line(p, data[1,], limit_value, "#AD3D25", annotation_position, "Limit reference point")
  if (facet_wrap)         p <- add_scenario_facets(p, data, scales = scales, ncol = ncol)

  return(p)
}
