# Copyright 2024 Fisheries Queensland

# This file is part of SSAND.
# SSAND is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
# SSAND is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
# You should have received a copy of the GNU General Public License along with SSAND. If not, see <https://www.gnu.org/licenses/>.

#' Length composition plot with an option to add model fits
#'
#' @param data Output from lengthplot_prep(). A data frame with year (int), fleet (int), bin (int), sex (chr), obs (num), exp (num), scenario (int)
#' @param show_fits Set to TRUE to show model fits.
#' @param fleet Specify which fleet to plot (numeric). By default, fleet 1 will be shown.
#' @param partition Specify which partition to plot (numeric). By default, partition 0 will be shown.
#' @param scenario A scenario numbers to be shown on plot (numeric). This was already specified in prep file, but this is a manual override to save running the prep function again.
#' @param point_size Size of points used in ggplot2::geom_line(). Default is 1.
#' @param colours A vector of colours used for sex types (character).
#' @param legend_position Position of the legend ("none", "left", "right", "bottom", "top", or two-element numeric vector for x and y position). Default is "top".
#' @param xlab Label for x-axis (character). Default is "Year".
#' @param ylab Label for y-axis (character). Default is "Spawning biomass (relative)".+
#' @param xbreaks A vector of breaks between x-axis labels, used in ggplot2::scale_x_continous() (numeric).
#' @param ybreaks A vector of breaks between y-axis labels, used in ggplot2::scale_y_continous() (numeric).
#' @param xlabels A vector of labels for the x-axis breaks.
#' @param ylabels A vector of labels for the y-axis breaks.
#' @param xlim A vector of lower and upper x-axis limits (e.g. c(1950, 2020)) (numeric).
#' @param ylim A vector of lower and upper y-axis limits (e.g. c(0,1)) (numeric).
#' @param scales Scales for ggplot2::facet_wrap(). Default is 'free', see ?ggplot2::facet_wrap for options.
#' @param ncol Number of columns for facet_wrap(). Default is 2.
#' @param direction Direction of facet wrap (character). Enter "v" for vertical or "h" for horizontal.
#' @param MLS Value of minimum legal size (or equivalent) (numeric). Leave blank to exclude from plot.
#' @param MLS_colour Colour of MLS line (character). Default is "black".
#' @param MLS_label Label of MLS line (character). Default is "MLS".
#' @param financial_year Set to TRUE if the assessment was based on financial year (logical). Adjusts the x-axis to show full financial year notation.
#'
#' @return A length plot, with an option to add model fits
#' @export
#'
#' @examples
#' data <- lengthplot_prep_SS(ss_mle)
#' lengthplot(data, show_fits=FALSE)
#' lengthplot(data)
lengthplot <- function(data,
                       fleet = 1,
                       scenario = 1,
                       partition = 0,
                       point_size = 1,
                       colours = c("#AD3D25", "#0085B8", "#FCC17E"),
                       legend_position = "top",
                       financial_year = FALSE,
                       xlab = "Length (cm)",
                       ylab = "Proportion",
                       xbreaks = NULL,
                       ybreaks = NULL,
                       xlabels = NULL,
                       ylabels = NULL,
                       xlim = NULL,
                       ylim = NULL,
                       scales = "fixed",
                       ncol = 4,
                       direction = "v",
                       MLS = NULL,
                       MLS_colour = "black",
                       MLS_label = "MLS",
                       show_fits = TRUE) {

  # Data input warnings
  if (!"year" %in% names(data)) {warning("Input data is missing year column")}
  if (!"fleet" %in% names(data)) {warning("Input data is missing fleet column")}
  if (!"bin" %in% names(data)) {warning("Input data is missing bin column")}
  if (!"sex" %in% names(data)) {warning("Input data is missing sex column")}
  if (!"obs" %in% names(data)) {warning("Input data is missing obs column")}
  if (!"exp" %in% names(data)) {warning("Input data is missing exp column")}
  if (!"scenario" %in% names(data)) {warning("Input data is missing scenario column")}

  if (direction == "vertical") {direction = "v"}
  if (direction == "horizontal") {direction = "h"}

  fleet_val <- fleet
  data <- data |> dplyr::filter(fleet==fleet_val)

  scenario_val <- scenario
  data <- data |> dplyr::filter(scenario==scenario_val)

  partition_val <- partition
  data <- data |> dplyr::filter(partition==partition_val)

  if (financial_year) {data <- data |> dplyr::mutate(year = paste0(year-1,"\U2013",year))}

  if (missing(xlim)) {xlim <- c(min(data$bin)-1,max(data$bin)+1)}
  if (missing(ylim)) {ylim <- c(min(min(data$obs),min(data$exp))-0.0001,
                                max(max(data$obs),max(data$exp)))}

  if (missing(xbreaks)) {xbreaks <- pretty(xlim)}
  if (missing(ybreaks)) {ybreaks <- pretty(ylim)}

  if (missing(xlabels)) {xlabels <- xbreaks}
  if (missing(ylabels)) {ylabels <- abs(ybreaks)}

  p <- ggplot2::ggplot(data) +
    ggplot2::facet_wrap(~year, scales=scales, ncol = ncol, dir = direction) +
    ggplot2::theme_bw() +
    ggplot2::scale_fill_manual(name = "Sex", values=c("grey60", "grey30", "grey80")) +
    ggplot2::theme(legend.position = legend_position) +
    ggplot2::xlab(xlab) +
    ggplot2::ylab(ylab) +
    ggplot2::scale_x_continuous(limits = xlim, breaks = xbreaks, labels = xlabels) +
    ggplot2::scale_y_continuous(limits = ylim, breaks = ybreaks, labels = ylabels)

  if (!show_fits) {
    p <- p +
      ggplot2::geom_bar(ggplot2::aes(x=bin, y=obs, group=sex, fill=as.factor(sex)), stat="identity")
  }

  if (show_fits) {
    p <- p +
      ggplot2::geom_area(ggplot2::aes(x=bin, y=obs, group=sex, fill=as.factor(sex)),alpha=0.7, colour="black", outline.type = "full") +
      ggplot2::geom_point(ggplot2::aes(x=bin, y=obs, group=sex), size = point_size) +
      ggplot2::geom_line(ggplot2::aes(x=bin, y=exp, group=sex, colour=sex), linewidth=1) +
      ggplot2::scale_colour_manual(name="Sex", values = colours)
  }


  if (!missing(MLS)) {
    p <- p +
      ggplot2::geom_vline(aes(xintercept = MLS, linetype = MLS_label), colour = MLS_colour) +
      ggplot2::scale_linetype_manual(values = "dashed", name = ggplot2::element_blank())
  }
  return(p)
}





