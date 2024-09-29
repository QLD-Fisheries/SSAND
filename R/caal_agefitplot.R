# Copyright 2024 Fisheries Queensland

# This file is part of SSAND.
# SSAND is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
# SSAND is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
# You should have received a copy of the GNU General Public License along with SSAND. If not, see <https://www.gnu.org/licenses/>.

#' Plot to show age fits from conditional-age-at-length data
#'
#' @param data A data frame with year (int), bin (int), obs (num), exp (num), scenario (int), sex (int)
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
#'
#' @return A plot of age fits from conditional-age-at-length data
#' @export
#'
#' @examples
#' data <- caal_agefitplot_prep_SS(ss_mle)
#' caal_agefitplot(data, scenario=1,show_fits=FALSE)
#' caal_agefitplot(data, scenario=1)
caal_agefitplot <- function(data,
                            scenario = 1,
                            colours = c("grey70","black","black"),
                            line_width = 1,
                            ncol = 3,
                            scales = 'free',
                            point_size = 1.5,
                            xlab = "Age (years)",
                            ylab = "Sample size",
                            show_fits = TRUE) {

  if (!show_fits) {
    data <- data |>
      dplyr::group_by(year) |>
      dplyr::mutate(sum = sum(obs),
             obs = obs/sum) |>
      dplyr::ungroup()
  }

  if (!show_fits & missing(ylab)) {ylab = "Proportion"}

  if (length(scenario)>1) {warning("Please enter a single scenario to display on plot.")}

  p <- ggplot2::ggplot(data) +
    ggplot2::geom_bar(ggplot2::aes(x=bin,y=obs), fill=colours[1], stat='identity')

  if (show_fits) {
    p <- p +
      ggplot2::geom_line(ggplot2::aes(x=bin,y=exp), colour=colours[2], linewidth=line_width) +
      ggplot2::geom_point(ggplot2::aes(x=bin,y=exp), colour=colours[3], size=point_size)
  }
  p <- p +
    ggplot2::facet_wrap(~year, scales=scales, ncol=ncol, dir="v") +
    ggplot2::theme_bw() +
    ggplot2::scale_x_continuous(name=xlab)+
    ggplot2::scale_y_continuous(name=ylab)

  return(p)
}


