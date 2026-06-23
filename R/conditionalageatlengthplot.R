# Copyright 2024 Fisheries Queensland

# This file is part of SSAND.
# SSAND is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
# SSAND is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
# You should have received a copy of the GNU General Public License along with SSAND. If not, see <https://www.gnu.org/licenses/>.

#' Conditional age-at-length plot
#'
#' @param data Output from ageplot_prep(). A data frame with year (int), bin (int), fleet (int), lbin_low (int), pearson (num), obs (num), sex (int), scenario (int)
#' @param show_fits Set to TRUE to show model fits.
#' @param xlab Label for x-axis (character). Default is "Year".
#' @param ylab Label for y-axis (character). Default is "Spawning biomass (relative)".
#' @param xbreaks A vector of breaks between x-axis labels, used in ggplot2::scale_x_continous() (numeric).
#' @param ybreaks A vector of breaks between y-axis labels, used in ggplot2::scale_y_continous() (numeric).
#' @param xlabels A vector of labels for the x-axis breaks.
#' @param ylabels A vector of labels for the y-axis breaks.
#' @param xlim A vector of lower and upper x-axis limits (e.g. c(1950, 2020)) (numeric).
#' @param ylim A vector of lower and upper y-axis limits (e.g. c(0,1)) (numeric).
#' @param size_breaks Breaks of size scale for bubbles
#' @param size_range Range of size scale for bubbles
#' @param fleet Specify which fleet to plot (numeric). By default, fleet 1 will be shown.
#' @param ylim A vector of lower and upper y-axis limits (e.g. c(0,1)) (numeric).
#' @param xlim A vector of lower and upper x-axis limits (e.g. c(1950, 2020)) (numeric).
#' @param scenarios A vector of scenario numbers to be shown on plot (numeric). This was already specified in prep file, but this is a manual override to save running the prep function again.
#' @param scenario_labels A vector of customised scenario names (character). Default is "Scenario 1", "Scenario 2", etc.
#' @param scenario_order A vector to reorder how scenarios are displayed (character). Use the label names defined in "scenario_labels".
#' If "scenario_labels" is left blank, the labels will be "Scenario 1", "Scenario 2" etc.
#' Any scenarios not included in "scenario_order" will be tacked on in the order they appear in the input data.
#' @param ncol Number of columns for facet_wrap(). Default is 3.
#' @param colours A vector of colours used (character).
#' @param financial_year Set to TRUE if the assessment was based on financial year (logical). Adjusts the x-axis to show full financial year notation.
#' @param text_size,legend_text_size,text_colour,legend_text_colour,legend_position,legend_box,legend_title_blank,panel_border,panel_border_colour,xangle Optional plotting theme overrides. Defaults are controlled by `theme_ssand()`
#'   and can be set globally via `set_ssand_style()`.
#'
#' @return Conditional age-at-length plot
#' @export
#'
#' @examples
#' data <- conditionalageatlengthplot_prep_SS(ss_mle,sex_code=1)
#' conditionalageatlengthplot(data, show_fits=FALSE)
#' conditionalageatlengthplot(data)
conditionalageatlengthplot <- function(data,
                                       show_fits = TRUE,
                                       xlab = "Age (years)",
                                       ylab = "Length (cm)",
                                       xbreaks = NULL,
                                       ybreaks = NULL,
                                       xlabels = NULL,
                                       ylabels = NULL,
                                       xlim = NULL,
                                       ylim = NULL,
                                       size_breaks = c(0.01,0.25,0.5),
                                       size_range = c(0.01, 6),
                                       fleet = 1,
                                       # ylim = c(NA,NA),
                                       # xlim = c(NA,NA),
                                       ncol = 3,
                                       scenarios = NULL,
                                       scenario_labels = NULL,
                                       scenario_order = NULL,
                                       colours = fq_palette("alisecolours")[c(2,10)],
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
                                       panel_border_colour = NULL){

  # ___________________
  # Data validation
  # ___________________
  check_data_columns(data, c("year","bin","fleet","lbin_low","pearson","obs","sex","scenario"))
  data$xvar <- data$bin

  # ___________________
  # Basic plot set up
  # ___________________
  data <- apply_scenarios(data, scenarios, scenario_labels, scenario_order)

  fleet_filter <- fleet
  data <- data |> dplyr::filter(fleet == fleet_filter)

  p <- ggplot2::ggplot(data) +
    ggplot2::facet_wrap(~year, dir = "v" , ncol = ncol) +
    ggplot2::scale_size_continuous(breaks = size_breaks, range = size_range) +
    ggplot2::scale_fill_manual(values = colours)

  # ___________________
  # Build MLE plot
  # ___________________
  if (!show_fits) {
    p <- p +
      ggplot2::geom_point(ggplot2::aes(x=bin, y=lbin_low, size=ifelse(obs==0, NA, abs(obs))),
                          shape=1)
  }

  if (show_fits) {
    temp_data <- data |>
      dplyr::mutate(posneg = ifelse(pearson>0,"Positive","Negative"))

    p <- p +
      ggplot2:: geom_point(data = temp_data,
                           ggplot2::aes(x = bin, y = lbin_low, size = ifelse(pearson==0, NA, abs(pearson)), fill = posneg),
                           alpha = 0.5,
                           shape = 21)
  }

  # ___________________
  # Axes and theme
  # ___________________
  x_axis <- build_x_axis(x = data$xvar,
                         xlab = xlab,
                         xlim = xlim,
                         xbreaks = xbreaks,
                         xlabels = xlabels,
                         financial_year = financial_year,
                         expand_upper = 0,
                         xangle = xangle,
                         is_date = FALSE)

  y_axis <- build_y_axis(y = data$value,
                         ylab = ylab,
                         ylim = ylim,
                         ybreaks = ybreaks,
                         ylabels = ylabels,
                         lower = 0,
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

