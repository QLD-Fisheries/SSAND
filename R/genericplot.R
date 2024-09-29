# Copyright 2024 Fisheries Queensland

# This file is part of SSAND.
# SSAND is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
# SSAND is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
# You should have received a copy of the GNU General Public License along with SSAND. If not, see <https://www.gnu.org/licenses/>.

#' Generic plot
#'
#' @param data Output from catchplot_prep(). A dataframe with date, value, fleet, scenario
#' @param xlab Label for x-axis (character). Default is "Year".
#' @param ylab Label for y-axis (character). Default is "Retained catch (t)".
#' @param xbreaks A vector of breaks between x-axis labels, used in ggplot2::scale_x_continous() (numeric).
#' @param ybreaks A vector of breaks between y-axis labels, used in ggplot2::scale_y_continous() (numeric).
#' @param xlabels A vector of labels for the x-axis breaks.
#' @param ylabels A vector of labels for the y-axis breaks.
#' @param xlim A vector of lower and upper x-axis limits (e.g. c(1950, 2020)) (numeric).
#' @param ylim A vector of lower and upper y-axis limits (e.g. c(0,1)) (numeric).
#' @param xangle Set to 90 to rotate x-axis labels 90 degrees.
#' @param colours A vector of colours used (character).
#' @param legend_position Position of the legend ("none", "left", "right", "bottom", "top", or two-element numeric vector for x and y position). Default is "top".
#' @param scales Scales for ggplot2::facet_wrap(). Default is 'free', see ?ggplot2::facet_wrap for options.
#' @param type Specify type of plot to generate (character). Options are "bar" or "line".
#' @param show_points If type=="line", set show_points to TRUE to show points on top of the line plot.
#' @param ncol Number of columns for facet_wrap(). Default is 2.
#' @param xvar A vector to be plotted on the x-axis. See default values for example.
#' @param yvar A vector to be plotted on the y-axis. See default values for example.
#' @param fill_var A vector to be plotted on the x-axis. See default values for example.
#' @param colour_var A vector to guide the colour mapping. See default values for example.
#' @param shape_var A vector to guide the shape mapping. See default values for example.
#' @param legend_title Text to display as legend title (character).
#'
#' @return Generic plot
#' @export
#'
#' @examples
#' data <- data.frame()
#' for (scenario in 1:6) {
#'   for (fleet in 1:2) {
#'     tmp <- data.frame(year = 1980:2020,
#'                       value = runif(41),
#'                       fleet = fleet,
#'                       scenario = scenario)
#'     data <- rbind(data, tmp)
#'   }
#' }
#' genericplot(data)
#' genericplot(data, type = "line", show_points = TRUE)

genericplot <- function(data,
                        type = "bar",
                        xvar = data$year,
                        yvar = data$value,
                        fill_var = data$fleet,
                        colour_var = data$fleet,
                        shape_var = NULL,
                        show_points = FALSE,
                        legend_title = "Legend",
                        xlab = "x axis",
                        ylab = "y axis",
                        xbreaks = NULL,
                        ybreaks = NULL,
                        xlabels = NULL,
                        ylabels = NULL,
                        xlim = NULL,
                        ylim = NULL,
                        xangle = NULL,
                        colours = NULL,
                        legend_position = "top",
                        scales = 'free',
                        ncol = 2) { # function name with default values


  if (missing(colours)) {
    colours <- fq_palette("alisecolours")[1:9]
  }

  if (missing(xlim)) {
    xlim <- c(min(xvar),max(xvar))
  }
  if (type == "line" & missing(ylim)) {
    ylim <- c(min(yvar),max(yvar))
  }

  if(type == "bar" & missing(ylim)) {
    ylim[1] <- 0
    ymax <- data.frame(x = xvar, y = yvar) |>
      dplyr::group_by(x) |>
      dplyr::mutate(sum = sum(y)) |>
      dplyr::ungroup()

    ylim[2] <- max(ymax$sum)
  }

  if (missing(xbreaks)) {
    xbreaks <- pretty(xlim)
  }
  if (missing(ybreaks)) {
    ybreaks <- pretty(ylim)
  }

  if (missing(xlabels)) {
    xlabels <- xbreaks
  }
  if (missing(ylabels)) {
    ylabels <- ybreaks
  }

  if (missing(xangle)) {
    xangle <- 0
  }

  if (type == "bar") {
    p <- ggplot2::ggplot(data) +
      ggplot2::geom_bar(ggplot2::aes(x = xvar, y = yvar, fill = as.factor(fill_var)),
                        stat = 'identity')
  }

  if (type == "line") {
    p <- ggplot2::ggplot(data) +
      ggplot2::geom_line(ggplot2::aes(x = xvar, y = yvar, colour = as.factor(colour_var)))
  }

  if (show_points) {
    if (missing(shape_var)) {
      p <- p +
        ggplot2::geom_point(ggplot2::aes(x = xvar, y = yvar, colour = as.factor(colour_var)))

    } else {
      p <- p +
        ggplot2::geom_point(ggplot2::aes(x = xvar, y = yvar, colour = as.factor(colour_var), shape = as.factor(shape_var)))
    }
  }

  p <- p +
    ggplot2::xlab(xlab) +
    ggplot2::ylab(ylab) +
    ggplot2::theme_bw() +
    ggplot2::scale_fill_manual(name = legend_title, values = colours) +
    ggplot2::scale_colour_manual(name = legend_title, values = colours) +
    ggplot2::scale_shape_manual(name = legend_title) +
    # ggplot2::theme(panel.background = ggplot2::element_rect(fill = NA, colour = "black"),
    #                legend.title = ggplot2::element_blank(),
    #                legend.position = legend_position) +
    # ggplot2::theme(text = ggplot2::element_text(size=12),
    #                legend.text = ggplot2::element_text(size=12)) +
    ggplot2::scale_x_continuous(limits = xlim, breaks = xbreaks, labels = xlabels) +
    ggplot2::scale_y_continuous(limits = ylim, breaks = ybreaks, labels = ylabels) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = xangle, vjust = 0.5, hjust=ifelse(xangle==90,0,0.5)))


  return(p)
}
