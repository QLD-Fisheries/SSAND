# Copyright 2024 Fisheries Queensland

# This file is part of SSAND.
# SSAND is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
# SSAND is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
# You should have received a copy of the GNU General Public License along with SSAND. If not, see <https://www.gnu.org/licenses/>.

#' SSANDify plots
#' A quick wrapper to give other ggplots the 'look and feel' of SSAND. Mostly just a short cut for theme_bw() and default SSAND colours
#'
#' @param p A ggplot object
#' @param legend_title Optional legend title. Default is blank
#' @param x Optional x-axis label
#' @param y Optional y-axis label
#'
#' @return A SSAND-looking plot
#' @export
#'
#' @examples
#' p <- ggplot2::ggplot(ggplot2::mpg) + ggplot2::geom_point(ggplot2::aes(displ, hwy, colour = class))
#' ssandify(p)
#' ssandify(p, x = "This", y = "That")
ssandify <- function(p, legend_title = "", x = NULL, y = NULL) {
  p <- p +
    ggplot2::theme_bw() +
    ggplot2::scale_fill_manual(name = legend_title, values = rep(fq_palette("alisecolours"),10)) +
    ggplot2::scale_colour_manual(name = legend_title, values = rep(fq_palette("alisecolours"),10))

  if (!missing(x)) p <- p + ggplot2::xlab(x)
  if (!missing(y)) p <- p + ggplot2::ylab(y)
  return(p)
}
