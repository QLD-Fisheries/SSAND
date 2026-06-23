# Copyright 2024 Fisheries Queensland

# This file is part of SSAND.
# SSAND is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
# SSAND is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
# You should have received a copy of the GNU General Public License along with SSAND. If not, see <https://www.gnu.org/licenses/>.

#' A plot that illustrates the impact of catchability rescaling how the model perceives catch rates
#'
#' @param data Output from cpueqplot_prep()
#' @param xlab Label for x-axis (character). Default is "Year".
#' @param ylab Label for y-axis (character). Default is "Catch rate (kg/fisher day)".
#' @param colours A vector of colours used for scenarios (character).
#' @param financial_year Set to TRUE if the assessment was based on financial year (logical). Adjusts the x-axis to show full financial year notation.
#' @param text_size,legend_text_size,text_colour,legend_text_colour,legend_position,legend_box,legend_title_blank,panel_border,panel_border_colour,xangle Optional plotting theme overrides. Defaults are controlled by `theme_ssand()`
#'   and can be set globally via `set_ssand_style()`.
#'
#' @return A plot that illustrates the impact of catchability rescaling how the model perceives catch rates
#' @export
#'
cpueqplot <- function(data,
                      xlab = "Year",
                      ylab = "Catch rate (kg/fisher day)",
                      colours = c("#FFC000","#9E480E", "#70AD47"),
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

  p <- ggplot2::ggplot(data) +
    ggplot2::geom_line(ggplot2::aes(x=year,y=cpueadjust, colour=scenario)) +
    ggplot2::scale_colour_manual(values = colours) +
    ggplot2::xlab(xlab) +
    ggplot2::ylab(ylab)

  # ___________________
  # Axes and theme
  # ___________________
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
                       xangle = xangle)
  return(p)
}

