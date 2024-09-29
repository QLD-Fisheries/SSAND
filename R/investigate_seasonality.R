# Copyright 2024 Fisheries Queensland

# This file is part of SSAND.
# SSAND is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
# SSAND is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
# You should have received a copy of the GNU General Public License along with SSAND. If not, see <https://www.gnu.org/licenses/>.

#' Seasonality plot
#'
#' @param data Output of format_logbooks()
#' @param show_cpue Set to TRUE to show CPUE on the y-axis
#' @param show_catch Set to TRUE to show catch on the y-axis
#' @param show_effort Set to TRUE to show effort on the y-axis
#' @param species_of_interest Optional. The name of the species of interest, as listed in the 'species' column of data. Filters data to only include that species.
#' @param max_days Numeric. Filter to include only records that were less than or equal to this maximum number of fishing days (i.e. exclude multi-day trips)
#' @param daily Set to TRUE to show daily seasonality. Set to FALSE for monthly.
#' @param xlab Label for x-axis of plot (character). Default is "".
#' @param ylab Label for y-axis of plot (character). Default is "".
#' @param colours A vector of colours used (character).
#' @param ncol Number of columns for facet wrap. Default is 1.
#' @param facet_var Variable by which plot is to be faceted (e.g. "region")
#' @param fill_var Variable by which plot is to be filled (e.g. "region")
#' @param extract_data Set to TRUE to return data instead of plot. Default is FALSE.
#'
#' @return Seasonality plot
#' @export
#'
#' @examples
#' \dontrun{seasonalityplot(data = format_logbooks(raw_data))}
seasonalityplot <- function(data,
                            show_cpue = FALSE,
                            show_catch = FALSE,
                            show_effort = FALSE,
                            species_of_interest = NULL,
                            facet_var = NULL,
                            fill_var = NULL,
                            max_days = 1,
                            daily = FALSE,
                            xlab = "Year",
                            ylab = NULL,
                            colours = rep(fq_palette("alisecolours"),10),
                            ncol = 2,
                            extract_data = FALSE
) {

  if (missing(ylab) && show_catch) {ylab = "Retained catch (kg)"}
  if (missing(ylab) && show_effort) {ylab = "Effort (fishing days)"}
  if (missing(ylab) && show_cpue) {ylab = "Catch per unit effort (kg/boat days)"}

  if ("maximum_fishing_day_count" %in% names(data)) {
    data <- data |> dplyr::filter(maximum_fishing_day_count <= max_days)
  } else {
    warning("The variable 'maximum_fishing_day_count' is not present in your dataset so not filtering out multi-day trips")
  }

  if (!missing(species_of_interest)) {
    data <- data |> dplyr::filter(species %in% species_of_interest)
    }

  if (show_catch)  {yvar = "weight"}
  if (show_effort) {yvar = "effort"}
  if (show_cpue)   {yvar = "cpue"}

  data <- data |>
    dplyr::mutate(day_number = as.numeric(strftime(date, format = "%j"))) |>
    dplyr::mutate(month = as.numeric(as.character(month))) |>
    dplyr::mutate(x_var = if(daily){day_number}else{month})

  if (missing(fill_var)  && missing(facet_var))  {data <- data |> dplyr::group_by(x_var)}
  if (missing(fill_var)  && !missing(facet_var)) {data <- data |> dplyr::group_by(x_var,.data[[facet_var]])}
  if (!missing(fill_var) && missing(facet_var))  {data <- data |> dplyr::group_by(x_var,.data[[fill_var]])}
  if (!missing(fill_var) && !missing(facet_var)) {data <- data |> dplyr::group_by(x_var,.data[[fill_var]],.data[[facet_var]])}


  data <- data |>
    dplyr::summarise(weight = sum(weight),
                     effort = dplyr::n(),
                     cpue = weight/dplyr::n(),
                     .groups='drop')

  p <- ggplot2::ggplot(data)

  if (!missing(fill_var)) {
    p <- p +
      ggplot2::geom_bar(ggplot2::aes(x=x_var, y = .data[[yvar]], fill = as.factor(.data[[fill_var]])), stat='identity')
  } else {
    p <- p +
      ggplot2::geom_line(ggplot2::aes(x=x_var, y = .data[[yvar]])) +
      ggplot2::geom_point(ggplot2::aes(x=x_var, y = .data[[yvar]]))
  }

  if (daily) {
    p <- p + ggplot2::scale_x_continuous(breaks = c(1,31,59,90,120,151,181,212,243,273,304,334),
                                         labels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))
  } else {
    p <- p + ggplot2::scale_x_continuous(breaks = c(1:12),
                                         labels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))
  }

  if (!missing(facet_var)) {
    p <- p + ggplot2::facet_wrap(~.data[[facet_var]], scales='free', ncol = ncol)
  }

  p <- p +
    ggplot2::ylim(0,NA) +
    ggplot2::theme_bw() +
    ggplot2::ylab(ylab) +
    ggplot2::xlab("")

  if (extract_data) {return(data)} else {return(p)}
}

