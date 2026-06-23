# Copyright 2024 Fisheries Queensland

# This file is part of SSAND.
# SSAND is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
# SSAND is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
# You should have received a copy of the GNU General Public License along with SSAND. If not, see <https://www.gnu.org/licenses/>.

#' Piner plot
#' A fleet-based likelihood plot
#'
#' @param data Output from pinerplot_prep(). A dataframe with columns x_vector, fleet and likelihood
#' @param xlab Label for x-axis (character). # Default is `expression(log(italic(R)[0]))`.
#' @param ylab Label for y-axis (character). Default is "Change in -log-likelihood".
#' @param xlim A vector of lower and upper x-axis limits (e.g. c(1950, 2020)) (numeric).
#' @param ylim A vector of lower and upper y-axis limits (e.g. c(0,1)) (numeric).
#' @param colours A vector of colours used (character).
#' @param shapes A vector of shapes used (character).
#' @param fleet_names A vector of customised fleet names for legend (character).
#' @param text_size,legend_text_size,text_colour,legend_text_colour,legend_position,legend_box,legend_title_blank,panel_border,panel_border_colour,xangle Optional plotting theme overrides. Defaults are controlled by `theme_ssand()`
#'   and can be set globally via `set_ssand_style()`.
#'
#' @return Piner plot, a fleet based likelihood plot
#' @export
#'
#' @examples
#' \dontrun{
#' r4ss::profile(dir = '.', # directory of 4 SS files
#'               oldctlfile = "control.ctl",
#'               newctlfile = "control.ctl",
#'               string = "steep",
#'               profilevec = c(0.4,0.5,0.6),
#'               exe = "C:/stocksynthesis/ss_3.30.22.exe")
#'
#' profile_input <- r4ss::SSsummarize(
#'   r4ss::SSgetoutput(dirvec = ".",
#'                     keyvec = 1:3, # 1:length(profilevec)
#'                     getcovar = FALSE,
#'                     getcomp = FALSE))
#'
#' pinerplot_component_options(profile_input)
#' data <- pinerplot_prep_SS(profile_input, component="Length_like")
#' pinerplot(data)
#' }
pinerplot <- function(data,
                      xlab = NULL,
                      ylab = "Change in -log-likelihood",
                      xlim = NULL,
                      ylim = NULL,
                      colours = SSAND::fq_palette("alisecolours"),
                      shapes = c(16,17,18,15,1,2,5,0,19,20,3,4,7,8,9,10,11,12,13,14),
                      fleet_names = NULL,
                      xangle = NULL,
                      legend_position = NULL,
                      text_size = NULL,
                      legend_text_size = NULL,
                      text_colour = NULL,
                      legend_text_colour = NULL,
                      legend_box = NULL,
                      legend_title_blank = NULL,
                      panel_border = NULL,
                      panel_border_colour = NULL) {

  # Data input warnings
  check_data_columns(data, c("x_vector","fleet","likelihood"))

  if (missing(xlab)) xlab = expression(log(italic(R)[0]))

  if (missing(xlim)) {
    xlim <- c(
      dplyr::first(sort(unique(data$x_vector))),
      dplyr::last(sort(unique(data$x_vector)))
    )
  }

  if (missing(ylim)) ylim <- c(0,max(data$likelihood))

  p <- ggplot2::ggplot(data) +
    ggplot2::geom_line(ggplot2::aes(x=x_vector,
                                    y=likelihood,
                                    colour=fleet)) +
    ggplot2::geom_point(ggplot2::aes(x=x_vector,
                                     y=likelihood,
                                     colour=fleet,
                                     shape=fleet)) +
    ggplot2::scale_x_continuous(breaks=sort(unique(data$x_vector)),
                                name=xlab,
                                limits=xlim) +
    ggplot2::scale_y_continuous(name=ylab,
                                limits=ylim) +
    ggplot2::scale_colour_manual(name="Fleet",
                                 values=colours) +
    ggplot2::scale_shape_manual(name="Fleet",
                                values=shapes)

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
