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
#' @param legend_position Position of the legend ("none", "left", "right", "bottom", "top", or two-element numeric vector for x and y position). Default is "top".
#' @param size_breaks Breaks of size scale for bubbles
#' @param size_range Range of size scale for bubbles
#' @param fleet Specify which fleet to plot (numeric). By default, fleet 1 will be shown.
#' @param ylim A vector of lower and upper y-axis limits (e.g. c(0,1)) (numeric).
#' @param xlim A vector of lower and upper x-axis limits (e.g. c(1950, 2020)) (numeric).
#' @param text_size Text size (num). Default is 12.
#' @param ncol Number of columns for facet_wrap(). Default is 3.
#' @param colours A vector of colours used (character).
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
                                       legend_position = "top",
                                       size_breaks = c(0.01,0.25,0.5),
                                       size_range = c(0.01, 6),
                                       fleet = 1,
                                       ylim = c(NA,NA),
                                       xlim = c(NA,NA),
                                       text_size = 12,
                                       ncol = 3,
                                       colours = fq_palette("alisecolours")[c(2,10)]){

  # ___________________
  # Data validation
  # ___________________

  # Data input warnings
  check_data_columns(data, c("year","bin","fleet","lbin_low","pearson","obs","sex","scenario"))
  data$xvar <- data$bin

  # ___________________
  # Custom to this plot
  # ___________________
  fleet_filter <- fleet
  data <- data |> dplyr::filter(fleet == fleet_filter)

  # ___________________
  # Basic plot set up
  # ___________________

  data <- apply_scenarios(data, scenarios, scenario_labels, scenario_order)

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


  p <- ggplot2::ggplot(data) + get_theme_ssand()
  p <- add_x_scale_continuous(p, x_axis)
  p <- add_y_scale_continuous(p, y_axis)

  p <- p +
    ggplot2::scale_size_continuous(breaks = size_breaks, range = size_range) +
    ggplot2::facet_wrap(~year, dir = "v" , ncol = ncol) +
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
  return(p)
}

