# Copyright 2024 Fisheries Queensland

# This file is part of SSAND.
# SSAND is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
# SSAND is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
# You should have received a copy of the GNU General Public License along with SSAND. If not, see <https://www.gnu.org/licenses/>.

#' Andre plot
#'
#' @param data Output of andreplot_prep_SS. A data frame with columns "year" (num), "length" (num), "obs" (num), "pred" (num), "low" (num), "upp" (num), "label" (chr), "scenario" (int), "CI" (num)
#' @param scenario The scenario to plot. Default is 1.
#' @param fleet The fleet to plot. Default is 1.
#' @param years A vector of years to plot
#' @param colours A vector of colours to plot. First is the colour of points, then the colour of the ribbon, then the colour of the expected line.
#' @param xlab Label for x-axis (character). Default is "Length (cm)".
#' @param ylab A vector of labels for y-axis (character). Two entries, for each column of the plot. Default is c("Age","Standard deviation (age)").
#' @param legend_ratio A vector specifying the relative heights of the legend and the rest of the plot. Default is c(1,10)
#' @param financial_year Set to TRUE if the assessment was based on financial year (logical). Adjusts the x-axis to show full financial year notation.
#' @param text_size,legend_text_size,text_colour,legend_text_colour,legend_position,legend_box,legend_title_blank,panel_border,panel_border_colour,xangle Optional plotting theme overrides. Defaults are controlled by `theme_ssand()`
#'   and can be set globally via `set_ssand_style()`.
#'
#' @return Andre plot with an option to filter by fleet
#' @export
#'
#' @examples
#' data <- andreplot_prep_SS(ss_mle, sex_code=1)
#' andreplot(data, fleet = 1)
andreplot <- function(data,
                      scenario = 1,
                      fleet = 1,
                      years = NULL,
                      xlab = "Length (cm)",
                      ylab = c("Age","Standard deviation (age)"),
                      colours = c("black","grey80",fq_palette("DAF")),
                      legend_ratio = c(1,10),
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
  check_data_columns(data, c("year","length","obs","pred","low","upp","label","scenario","CI"))
  data$xvar <- data$length

  # ___________________
  # Custom to this plot
  # ___________________
  if (!missing(years)) {data <- data |> dplyr::filter(year %in% years)}
  scenario_var <- scenario
  fleet_var <- fleet

  data <- data |>
    dplyr::mutate(fill_var = paste0(round(CI*100),"% confidence interval")) |>
    dplyr::filter(scenario == scenario_var) |>
    dplyr::filter(fleet == fleet_var)

  data_age <- data |> dplyr::filter(label=="Age")
  data_sd  <- data |> dplyr::filter(label=="Standard deviation (age)")

  # ___________________
  # Build MLE plots
  # ___________________
  p_age <- ggplot2::ggplot(data_age, ggplot2::aes(x = length, y = obs)) +
    get_theme_ssand() +
    ggplot2::geom_ribbon(data = data_age |>
                           dplyr::filter(!is.na(low) & !is.na(upp)),
                         ggplot2::aes(ymin = low, ymax = upp, fill = fill_var)) +
    ggplot2::geom_point(ggplot2::aes(shape = "Observed"), colour=colours[1]) +
    ggplot2::geom_line(ggplot2::aes(y = pred, color = "Expected")) +
    ggplot2::xlab(xlab) +
    ggplot2::ylab(ylab[1]) +
    ggplot2::facet_wrap(~year, ncol=1) +
    ggplot2::scale_fill_manual(name = "", values=colours[2]) +
    ggplot2::scale_shape_manual(name = "", values=16) +
    ggplot2::scale_colour_manual(name = "", values=colours[3]) +
    ggplot2::theme(legend.position = "top")

  p_sd <- ggplot2::ggplot(data_sd, ggplot2::aes(x = length, y = obs)) +
    get_theme_ssand() +
    ggplot2::geom_ribbon(data = data_sd |> dplyr::filter(!is.na(low) & !is.na(upp)),
                         ggplot2::aes(ymin = low, ymax = upp),
                         fill = colours[2]) +
    ggplot2::geom_point(colour = colours[1]) +
    ggplot2::geom_line(ggplot2::aes(y = pred), color = colours[3]) +
    ggplot2::xlab(xlab) +
    ggplot2::ylab(ylab[2]) +
    ggplot2::facet_wrap(~year, ncol=1)


  p_age <- add_ssand_theme(p_age,
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


  p_sd <- add_ssand_theme(p_sd,
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

  # Combine two plots into one
  plots <- gridExtra::arrangeGrob(p_age + ggplot2::theme(legend.position="none"), p_sd, nrow=1)

  # Extract legend from p_age
  legend <- ggplot2::ggplot_gtable(ggplot2::ggplot_build(p_age))$grobs[[which(sapply(ggplot2::ggplot_gtable(ggplot2::ggplot_build(p_age))$grobs, function(x) x$name) == "guide-box")]]

  # Combine plots and legend
  if (legend_position == "top") {
    gridExtra::grid.arrange(legend, plots,
                            nrow=2,
                            heights = legend_ratio)
  } else {
    gridExtra::grid.arrange(plots,legend,
                            nrow=2,
                            heights=legend_ratio)
  }
}
