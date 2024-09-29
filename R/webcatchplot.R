# Copyright 2024 Fisheries Queensland

# This file is part of SSAND.
# SSAND is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
# SSAND is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
# You should have received a copy of the GNU General Public License along with SSAND. If not, see <https://www.gnu.org/licenses/>.

#' Web catch plot
#'
#' @param data A data frame frame from webcatchplot_prep() or created from scratch, with variables called year (num), sector (chr), value (num) and method (chr).
#' @param xlab Label for x-axis (character). Default is "Year".
#' @param ylab Label for y-axis (character). Default is "Queensland retained catch (t)".
#' @param xbreaks A vector of breaks between x-axis labels, used in ggplot2::scale_x_continous() (numeric).
#' @param ybreaks A vector of breaks between y-axis labels, used in ggplot2::scale_y_continous() (numeric).
#' @param xlabels A vector of labels for the x-axis breaks.
#' @param ylabels A vector of labels for the y-axis breaks.
#' @param xlim A vector of lower and upper x-axis limits (e.g. c(1950, 2020)) (numeric).
#' @param ylim A vector of lower and upper y-axis limits (e.g. c(0,1)) (numeric).
#' @param xangle Set to 90 to rotate x-axis labels 90 degrees.
#' @param legend_position Position of the legend ("none", "left", "right", "bottom", "top",
#' or two-element numeric vector for x and y position). Default is "top".
#' @param colours A vector of colours used (character). Avoid changing. These are from the DAF colour palette.
#' @param weight_unit Set to "kg" if data are input in kilograms, otherwise tonnes are assumed.
#' @param text_size Text size (num). Default is 12.
#' @param financial_year Set to TRUE if the assessment was based on financial year (logical). Adjusts the x-axis to show full financial year notation.
#'
#' @return A summary plot of catch for the DAF website
#' @export
#'
#' @examples
#' data <- webcatchplot_prep_SS(ss_mle)
#' webcatchplot(data)
webcatchplot <- function(data,
                         weight_unit = "kg",
                         xlab = 'Year',
                         ylab = 'Queensland retained catch (t)',
                         xbreaks = NULL,
                         ybreaks = NULL,
                         xlabels = NULL,
                         ylabels = NULL,
                         xlim = NULL,
                         ylim = NULL,
                         xangle = NULL,
                         financial_year = FALSE,
                         legend_position = c(0.2,0.6),
                         colours = c("#005572","#31B7C3","#005572","#31B7C3"),
                         text_size = 12){

  if (weight_unit == "kg") {data$value <- data$value / 1000}

  if (financial_year & xlab=="Year") {warning("Your x-axis implies calendar year, but you've indicated you're using financial year.")}

  if (missing(xlim)) {xlim <- c(min(data$year),max(data$year))}
  if (missing(ylim)) {ylim <- c(0,max(data$value))}

  if (missing(xbreaks)) {xbreaks <- pretty(xlim)}
  if (missing(ybreaks)) {ybreaks <- pretty(ylim)}

  if (missing(xlabels)) {xlabels <- xbreaks}
  if (missing(ylabels)) {ylabels <- ybreaks}

  if (financial_year) {xlabels <- paste0(xbreaks-1,"\U2013",xbreaks)} else {xlabels <- xbreaks}
  if (missing(xangle)) {xangle <- ifelse(financial_year,90,0)}


  p <- ggplot2::ggplot() +
    ggplot2::geom_line(data = data |> dplyr::filter(method%in%c("Commercial data","Commercial plot adjust")), ggplot2::aes(x=year,y=value,linetype="a",colour="a",size="a")) +
    ggplot2::geom_point(data= data |> dplyr::filter(method%in%c("Commercial data","Commercial plot adjust")), ggplot2::aes(x=year,y=value,shape="a",colour="a",size="a")) +

    ggplot2::geom_line(data = data |> dplyr::filter(method=="Commercial modelled"), ggplot2::aes(x=year,y=value,linetype="b",colour="b",size="b")) +
    ggplot2::geom_point(data= data |> dplyr::filter(method=="Commercial modelled"), ggplot2::aes(x=year,y=value,shape="b",colour="b",size="b")) +

    ggplot2::geom_line(data = data |> dplyr::filter(method%in%c("Recreational modelled","Recreational plot adjust")), ggplot2::aes(x=year,y=value,linetype="d",colour="d",size="d")) +
    ggplot2::geom_point(data= data |> dplyr::filter(method%in%c("Recreational modelled","Recreational plot adjust")), ggplot2::aes(x=year,y=value,shape="d",colour="d",size="d")) +

    ggplot2::geom_line(data = data |> dplyr::filter(method=="Recreational data"), ggplot2::aes(x=year,y=value,linetype="c",colour="c",size="c")) +
    ggplot2::geom_point(data= data |> dplyr::filter(method=="Recreational data"), ggplot2::aes(x=year,y=value,shape="c",colour="c",size="c")) +

    ggplot2::scale_linetype_manual(name="Guide1", rep("",3),values=c("solid","dashed","blank","twodash"),
                                   labels = c("Commercial data", "Commercial modelled","Recreational data","Recreational modelled")) +

    ggplot2::scale_shape_manual(name="Guide1",rep("",3),values=c(32,32,16,32),
                                labels = c("Commercial data", "Commercial modelled","Recreational data","Recreational modelled")) +

    ggplot2::scale_colour_manual(name="Guide1",rep("",3),values= colours,
                                 labels = c("Commercial data", "Commercial modelled","Recreational data","Recreational modelled")) +

    ggplot2::scale_size_manual(name="Guide1",rep("",3),values= c(1.1,1.1,1.8,1.1),
                               labels = c("Commercial data", "Commercial modelled","Recreational data","Recreational modelled")) +

    ggplot2::xlab(xlab) +
    ggplot2::ylab(ylab) +
    ggplot2::theme_bw() +
    ggplot2::theme(panel.grid.major.x = ggplot2::element_line(color = "gray70")) +
    ggplot2::theme(panel.grid.minor.x = ggplot2::element_line(color = "gray95")) +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_line(color = "gray70")) +
    ggplot2::theme(panel.grid.minor.y = ggplot2::element_line(color = "gray95")) +
    ggplot2::theme(legend.position=legend_position) +
    ggplot2::theme(legend.background = ggplot2::element_blank()) +
    ggplot2::theme(legend.title = ggplot2::element_blank()) +
    ggplot2::theme(legend.text = ggplot2::element_text(size=text_size)) +
    ggplot2::scale_x_continuous(limits = xlim, breaks = xbreaks, labels = xlabels) +
    ggplot2::scale_y_continuous(limits = ylim, breaks = ybreaks, labels = ylabels) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = xangle, vjust = 0.5, hjust=ifelse(xangle==90,0,0.5)))

  return(p)
}
