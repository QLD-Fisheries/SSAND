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
#' @param colours A vector of colours used (character). Default is c("#FFC000","#9D9D9D","#FFE699").
#' @param financial_year Set to TRUE if the assessment was based on financial year (logical). Adjusts the x-axis to show full financial year notation.
#' @param legend_size Size of legend markers. Default is 6.
#' @param text_size,legend_text_size,text_colour,legend_text_colour,legend_position,legend_box,legend_title_blank,panel_border,panel_border_colour,xangle Optional plotting theme overrides. Defaults are controlled by `theme_ssand()`
#'   and can be set globally via `set_ssand_style()`.
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
                                 colours = c("#FFC000","#9D9D9D","#FFE699"),
                                 legend_size = 6,
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
                                 panel_border_colour = NULL){
  # ___________________
  # Data validation
  # ___________________
  check_data_columns(data, c("sector","source","startyr","endyr","col","label"))
  xlim <- c(min(data$startyr),max(data$endyr))

  # ___________________
  # Basic plot set up
  # ___________________
  p <- ggplot2::ggplot(data)

  # ___________________
  # Build MLE plot
  # ___________________
  p <- p +
    ggplot2::scale_colour_manual(values=c(colours,"#C1C0C0")) +
    ggplot2::geom_segment(ggplot2::aes(x=startyr-0.5,xend=endyr+0.5,y=sector,yend=sector,color=col),linewidth=30) +
    ggplot2::geom_text(data=data|>dplyr::filter(label==1),ggplot2::aes(x=startyr-0.5 + 0.5*(endyr - (startyr)), y=sector, label=source),color="#000000") +
    ggplot2::geom_text(data=data|>dplyr::filter(label==2),ggplot2::aes(x=startyr-0.5 + 0.5*(endyr - (startyr)), y=sector, label=source),color="#000000", angle = 90) +
    ggplot2::geom_text(data=data|>dplyr::filter(label==4),ggplot2::aes(x=startyr-0.5 + 0.5*(endyr - (startyr)), y=sector, label=source),color="#000000", angle = 90, size=3)

  # ___________________
  # Axes and theme
  # ___________________
  x_axis <- build_x_axis(x = data$xvar,
                         xlab = xlab,
                         xlim = xlim,
                         xbreaks = xbreaks,
                         xlabels = xlabels,
                         financial_year = financial_year,
                         expand_upper = as.numeric(show_final_biomass),
                         xangle = xangle,
                         is_date = FALSE)

  y_axis <- build_y_axis(y = data$value,
                         ylab = ylab,
                         ylim = ylim,
                         ybreaks = ybreaks,
                         ylabels = ylabels,
                         lower = 0,
                         upper = data$upper)

  p <- add_x_scale_continuous(p, x_axis)
  p <- add_y_scale_continuous(p, y_axis)

  p <- p +
    ggplot2::scale_x_continuous(breaks=xbreaks, labels = xlabels, expand = c(0,0))

  # p <- p +
  #   ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(linewidth = legend_size)))

  if (is.null(text_colour)) legend_position <- "none"


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
                       xangle = x_axis$angle)

  return(p)
}
