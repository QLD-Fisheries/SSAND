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
#' @param text_size Text size (num). Default is 12.
#' @param colours A vector of colours used for scenarios (character).
#'
#' @return A plot that illustrates the impact of catchability rescaling how the model perceives catch rates
#' @export
#'
cpueqplot <- function(data,
                      xlab = "Year",
                      ylab = "Catch rate (kg/fisher day)",
                      text_size = 12,
                      colours = c("#FFC000","#9E480E", "#70AD47")) {

  p <- ggplot2::ggplot(data) +
    ggplot2::geom_line(ggplot2::aes(x=year,y=cpueadjust, colour=scenario)) +
    ggplot2::scale_colour_manual(values = colours) +
    ggplot2::theme_bw() +
    ggplot2::xlab(xlab) +
    ggplot2::ylab(ylab) +
    ggplot2::theme(text = ggplot2::element_text(size=text_size)) +
    ggplot2::theme(legend.title = ggplot2::element_blank(), legend.position = "top", legend.box="vertical")

  return(p)
}

