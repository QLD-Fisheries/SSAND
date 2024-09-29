#' Catch reconstruction data sources plot
#'
#' There is no prep function to assist with the production of this plot. Instead, look at the example provided to create a suitable data frame for your assessment.
#' \itemize{
#' \item Note that the endyr of one source and startyr of the next should be the same to avoid gaps.
#' \item col 'A' represents interpolated data, like between SRFS data points
#' \item col 'B' represents data sources
#' \item col 'C' represents extrabolated data, like hindcasts or forecasts
#' \item label '1' represents regular, large text
#' \item label '2' represents large text, rotated 90 degrees
#' \item label '4' represents small text, rotated 90 degrees
#' \item label '0' represents text that you've decided you don't want to appear; works the same as listing '' in the source column
#' }
#'
#' @param data No prep function provided, see example. A data frame with sector (factor), source (chr), startyr (num), endyr (num), col (a colour category, chr), label (a label category, num)
#' @param xlab Label for x-axis (character). Default is "".
#' @param ylab Label for y-axis (character). Default is "".
#' @param legend_position Position of the legend ("none", "left", "right", "bottom", "top", or two-element numeric vector for x and y position). Default is "none".
#' @param colours A vector of colours used (character). Default is c("#FFC000","#9D9D9D","#FFE699").
#' @param text_size Text size (num). Default is 12.
#' @param financial_year Set to TRUE if the assessment was based on financial year (logical). Adjusts the x-axis to show full financial year notation.
#' @param xangle Set to 90 to rotate x-axis labels 90 degrees.
#'
# Copyright 2024 Fisheries Queensland

# This file is part of SSAND.
# SSAND is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
# SSAND is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
# You should have received a copy of the GNU General Public License along with SSAND. If not, see <https://www.gnu.org/licenses/>.

#' @return Catch reconstruction data sources plot
#' @export
#'
#' @examples
#' data <- data.frame(c('Queensland commercial'  , 'Hindcast (linear)', 1958, 1989, 'C', 1),
#'                    c('Queensland commercial'  , 'Logbook records'  , 1989, 2021, 'B', 1),
#'                    c('Queensland charter'     , 'Hindcast (linear)', 1958, 1995, 'C', 1),
#'                    c('Queensland charter'     , 'Logbook records'  , 1995, 2021, 'B', 1),
#'                    c('Queensland recreational', 'Hindcast'         , 1958, 2001, 'C', 1),
#'                    c('Queensland recreational', 'NRIFS'            , 2001, 2002, 'B', 4),
#'                    c('Queensland recreational', 'RFish'            , 2002, 2003, 'B', 4),
#'                    c('Queensland recreational', 'Estimated'        , 2003, 2005, 'A', 0),
#'                    c('Queensland recreational', 'RFish'            , 2005, 2006, 'B', 4),
#'                    c('Queensland recreational', 'Estimated'        , 2006, 2011, 'A', 2),
#'                    c('Queensland recreational', 'SRFS'             , 2011, 2012, 'B', 4),
#'                    c('Queensland recreational', 'Estimated'        , 2012, 2014, 'A', 0),
#'                    c('Queensland recreational', 'SRFS'             , 2014, 2015, 'B', 4),
#'                    c('Queensland recreational', 'Estimated'        , 2015, 2020, 'A', 0),
#'                    c('Queensland recreational', 'SRFS'             , 2020, 2021, 'B', 4),
#'                    c('Queensland recreational', 'Estimated'        , 2021, 2021, 'B', 0),
#'                    c('Queensland Indigenous'  , 'Equal to NRIFS'   , 1958, 2001, 'C', 1),
#'                    c('Queensland Indigenous'  , 'NRIFS'            , 2001, 2002, 'B', 4),
#'                    c('Queensland Indigenous'  , 'Equal to NRIFS'   , 2002, 2021, 'C', 1)) |>
#'   t() |>
#'   `rownames<-`(NULL) |>
#'   `colnames<-`(c("sector","source","startyr","endyr","col","label")) |>
#'   as.data.frame() |>
#'   dplyr::mutate(startyr = as.numeric(startyr), endyr = as.numeric(endyr)) |>
#'   dplyr::mutate(sector = as.factor(sector))
#'
#'   data$sector <- factor(data$sector, levels = c('Queensland Indigenous',
#'                                                 'Queensland recreational',
#'                                                 'Queensland charter',
#'                                                 'Queensland commercial'))
#' catchdatasourcesplot(data)
#' catchdatasourcesplot(data, financial_year=TRUE)
catchdatasourcesplot <- function(data,
                                 ylab = "",
                                 xlab = "",
                                 legend_position = "none",
                                 colours = NULL,
                                 text_size = 12,
                                 xangle = NULL,
                                 financial_year = FALSE){

  # Data input warnings
  if (!"sector" %in% names(data)) {warning("Input data is missing sector column")}
  if (!"source" %in% names(data)) {warning("Input data is missing source column")}
  if (!"startyr" %in% names(data)) {warning("Input data is missing startyr column")}
  if (!"endyr" %in% names(data)) {warning("Input data is missing endyr column")}
  if (!"col" %in% names(data)) {warning("Input data is missing col column")}
  if (!"label" %in% names(data)) {warning("Input data is missing label column")}

  xlim <- c(min(data$startyr),max(data$endyr))
  xbreaks <- pretty(xlim)
  if (financial_year) {xlabels <- paste0(xbreaks-1,"-",xbreaks)} else {xlabels <- xbreaks}
  if (missing(xangle)) {xangle <- ifelse(financial_year,90,0)}

  if (missing(colours)) {colours = c("#FFC000","#9D9D9D","#FFE699")}

  p <- ggplot2::ggplot(data) +
    ggplot2::geom_segment(ggplot2::aes(x=startyr-0.5,xend=endyr+0.5,y=sector,yend=sector,color=col),linewidth=30) +
    ggplot2::scale_x_continuous(breaks=xbreaks, labels = xlabels) +
    ggplot2::scale_colour_manual(values=c(colours,"#C1C0C0")) +
    ggplot2::geom_text(data=data|>dplyr::filter(label==1),ggplot2::aes(x=startyr-0.5 + 0.5*(endyr - (startyr)), y=sector, label=source),color="#000000") +
    ggplot2::geom_text(data=data|>dplyr::filter(label==2),ggplot2::aes(x=startyr-0.5 + 0.5*(endyr - (startyr)), y=sector, label=source),color="#000000", angle = 90) +
    ggplot2::geom_text(data=data|>dplyr::filter(label==4),ggplot2::aes(x=startyr-0.5 + 0.5*(endyr - (startyr)), y=sector, label=source),color="#000000", angle = 90, size=3)+
    ggplot2::theme_bw() +
    ggplot2::theme(text = ggplot2::element_text(size=text_size)) +
    ggplot2::theme(legend.position = legend_position) +
    ggplot2::ylab(ylab) +
    ggplot2::xlab(xlab) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = xangle, vjust = 0.5, hjust=ifelse(xangle==90,0,0.5)))

  return(p)
}
