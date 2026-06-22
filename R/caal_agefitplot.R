# Copyright 2024 Fisheries Queensland

# This file is part of SSAND.
# SSAND is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
# SSAND is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
# You should have received a copy of the GNU General Public License along with SSAND. If not, see <https://www.gnu.org/licenses/>.

#' Plot to show age fits from conditional-age-at-length data
#'
#' @param data A data frame with year (int), bin (int), obs (num), exp (num), scenario (int), sex (int), fleet (int)
#' @param fleet A numeric of fleet number to plot
#' @param scenario A single scenario number to plot (numeric). Default is 1.
#' @param colours A vector of colours used (character). First element is bar fill, then line colour, then point colour.
#' @param line_width Width of lines.
#' @param ncol Number of columns for facet_wrap(). Default is 3.
#' @param scales Scales for ggplot2::facet_wrap(). Default is 'free', see ?ggplot2::facet_wrap for options.
#' @param point_size Size of points used in ggplot2::geom_line(). Default is 1.5.
#' @param xlab Label for x-axis (character). Default is "Age (years)".
#' @param ylab Label for y-axis (character). Default is "Sample size".
#' @param show_fits Set to TRUE to show model fits. Set to FALSE to show model 'inputs'.
#' When TRUE, the input data are transformed into proportions rather than absolute values.
#' @param financial_year Set to TRUE if the assessment was based on financial year (logical). Adjusts the x-axis to show full financial year notation.
#' @param text_size,legend_text_size,text_colour,legend_text_colour,legend_position,legend_box,legend_title_blank,panel_border,panel_border_colour,xangle Optional plotting theme overrides. Defaults are controlled by `theme_ssand()`
#'   and can be set globally via `set_ssand_style()`.
#'
#' @return A plot of age fits from conditional-age-at-length data
#' @export
#'
#' @examples
#' data <- caal_agefitplot_prep_SS(ss_mle)
#' caal_agefitplot(data, scenario=2,show_fits=FALSE)
#' caal_agefitplot(data, scenario=2)
caal_agefitplot <- function(data,
                            scenario = 1,
                            fleet = 1,
                            colours = c("grey70","black","black"),
                            line_width = 1,
                            ncol = 3,
                            scales = 'free',
                            point_size = 1.5,
                            xlab = "Age (years)",
                            ylab = "Proportion",
                            show_fits = TRUE,
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
  check_data_columns(data, c("year","bin","obs","exp","scenario","sex","fleet"))

  data$xvar <- data$bin

  # ___________________
  # Custom to this plot
  # ___________________
  fleet_val <- fleet
  scenario_val <- scenario

  data <- data |>
    dplyr::filter(scenario == scenario_val) |>
    dplyr::filter(fleet == fleet_val)

  if (length(colours) == 1) { colours = c(colours, 'black', 'black')}
  if (length(scenario)>1) {warning("Please enter a single scenario to display on plot.")}

  data <- data |>
    dplyr::group_by(year) |>
    dplyr::mutate(sum = sum(obs),
                  obs = obs/sum,
                  sum1 = sum(exp),
                  exp = exp/sum1) |>
    dplyr::ungroup()

  if (!show_fits & missing(ylab)) {ylab = "Proportion"}

  # ___________________
  # Basic plot set up
  # ___________________
  p <- ggplot2::ggplot(data)

  # ___________________
  # Build MLE plot
  # ___________________
  p <- p +
    ggplot2::geom_bar(ggplot2::aes(x=bin,y=obs), fill=colours[1], stat='identity')

  if (show_fits) {
    p <- p +
      ggplot2::geom_line(ggplot2::aes(x=bin,y=exp), colour=colours[2], linewidth=line_width) +
      ggplot2::geom_point(ggplot2::aes(x=bin,y=exp), colour=colours[3], size=point_size)
  }

  # ___________________
  # Final layers
  # ___________________
  p <- add_scenario_facets(p, data, scales = scales, ncol = ncol, dir="v")

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
