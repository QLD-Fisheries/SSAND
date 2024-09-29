# Copyright 2024 Fisheries Queensland

# This file is part of SSAND.
# SSAND is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
# SSAND is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
# You should have received a copy of the GNU General Public License along with SSAND. If not, see <https://www.gnu.org/licenses/>.

#' Heat plot
#' A plot displaying fishing patterns of each operator over time.
#' The x-axis shows the fishing operator, the y-axis shows the year, and timelines are filled by the magnitude of catch or effort.
#'
#' @param data Output of format_logbooks()
#' @param species_of_interest The name of the species of interest, as listed in the 'species' column of data.
#' @param fill_var Variable by which plot is filled. Options are "days" or "catch". Default is "days".
#' @param facet_var Variable by which plot is to be faceted (e.g. "region")
#' @param interesting_fishers Optional. A vector of fishers/operators by which to filter the dataset.
#' @param interesting_years Optional. A vector of years by which to filter the dataset.
#' @param filter_weight_lower A minimum weight to be included on the plot.
#' @param filter_weight_upper A maximum weight to be included on the plot.
#' @param filter_days_lower A minimum number of days to be included on the plot.
#' @param filter_days_upper A maximum number of days to be included on the plot.
#' @param show_operators Set to TRUE to show operator names on the x-axis
#' @param show_values Set to TRUE to show values on the plot
#' @param ncol Number of columns for facet wrap. Default is 2.
#' @param text_size Text size
#' @param xlab Label for x-axis of plot (character). Default is "".
#' @param ylab Label for y-axis of plot (character). Default is "".
#' @param clean_up_plots Set to TRUE to remove axis marks and gridlines.
#' @param legend_position Position of legend. Default is "top"
#' @param legend_width Width of legend. Default is 5 (unit is cm)
#' @param legend_height Height of legend. Default is 0.5 (unit is cm)
#' @param max_fishers Maximum number of fishers to show on a single plot
#' @param scales Scaled for facet_wrap. Default is 'fixed'
#' @param extract_data Set to TRUE to return data instead of plot. Default is FALSE.
#'
#' @return Heat plot
#' @export
#'
#' @examples
#' \dontrun{heatplot(data = format_logbooks(raw_data))}
heatplot <- function(data,
                     species_of_interest = NULL,
                     fill_var = "days",
                     facet_var = NULL,
                     ylab = "Year",
                     xlab = "Operator",
                     interesting_fishers = NULL,
                     interesting_years = NULL,
                     filter_weight_lower = NULL,
                     filter_weight_upper = NULL,
                     filter_days_lower = NULL,
                     filter_days_upper = NULL,
                     show_operators = FALSE,
                     show_values = FALSE,
                     text_size = 2,
                     clean_up_plots = TRUE,
                     ncol = 1,
                     legend_position = "top",
                     legend_width = 5, # cm
                     legend_height = 0.5, # cm
                     max_fishers = 100,
                     scales='fixed',
                     extract_data = FALSE
) {

  if (!missing(species_of_interest)) {data <- data |> dplyr::filter(species %in% species_of_interest)}
  if (!missing(interesting_fishers)) {data <- data |> dplyr::filter(operator %in% interesting_fishers)}
  if (!missing(interesting_years)) {data <- data |> dplyr::filter(year %in% interesting_years)}

  if (!missing(facet_var)) {
    data <- data |> dplyr::group_by(operator,year,.data[[facet_var]])
  } else {
    data <- data |> dplyr::group_by(operator,year)
  }

  # `Effort (days)`
  # `Retained catch (kg)`

  data <- data |>
    # dplyr::select(operator,year,date,latband,weight,region) |>
    # dplyr::group_by(operator,year,latband,region) |>
    dplyr::summarise(days = dplyr::n_distinct(date),
                     weight = sum(weight), .groups='drop')  |>
    dplyr::mutate(weight = round(weight, digits = 0))

  if (!missing(filter_weight_lower)) {data <- data |> dplyr::filter(weight >= filter_weight_lower)}
  if (!missing(filter_weight_upper)) {data <- data |> dplyr::filter(weight <= filter_weight_upper)}
  if (!missing(filter_days_lower)) {data <- data |> dplyr::filter(days >= filter_days_lower)}
  if (!missing(filter_days_upper)) {data <- data |> dplyr::filter(days <= filter_days_upper)}

  ranked_fishers <- rank_fishers(data, show_year=FALSE)$operator
  number_of_plots <- ceiling(length(unique(data$operator)) / max_fishers)
  plots <- list()

  for (i in 1:number_of_plots) {

    # Determine which fishers to display on this plot
    start <- (i-1)*max_fishers+1 ; end <-  (i-1)*max_fishers+max_fishers
    fishers_for_this_plot <- ranked_fishers[start:end]
    # Filter data for those fishers
    data_subset <- data |> dplyr::filter(operator %in% fishers_for_this_plot)

    if (fill_var == "days") {
      p <- ggplot2::ggplot(data_subset, ggplot2::aes(x = operator, y = year, fill = days))
    } else {
      p <- ggplot2::ggplot(data_subset, ggplot2::aes(x = operator, y = year, fill = weight))
    }

    p <- p +
      ggplot2::geom_tile()+
      ggplot2::scale_fill_gradient(low = "lightblue", high = "red") +
      ggplot2::theme_bw() +
      ggplot2::ylab(ylab) +
      ggplot2::xlab(xlab) +
      ggplot2::theme(axis.text.x = ggplot2::element_blank()) +
      ggplot2::theme_bw() +
      ggplot2::theme(legend.position = legend_position)  +
      ggplot2::theme(legend.key.height = ggplot2::unit(legend_height, 'cm'),
                     legend.key.width  = ggplot2::unit(legend_width, 'cm'))

    if (show_values) {
      if (fill_var == "days") {
        p <- p + ggplot2::geom_text(ggplot2::aes(label=days), colour="white", size = text_size)
      } else {
        p <- p + ggplot2::geom_text(ggplot2::aes(label=weight), colour="white", size = text_size)
      }
    }

    if (!show_operators) {
      p <- p + ggplot2::theme(axis.text.x=ggplot2::element_blank())
    }

    if (clean_up_plots) {
      p <- p +
        ggplot2::theme(axis.ticks.x =ggplot2::element_blank(),
                       panel.grid.major = ggplot2::element_blank(),
                       panel.grid.minor = ggplot2::element_blank())
    }

    # if (show_region) {p <- p + ggplot2::facet_wrap(~region,ncol=ncol)}

    if (!missing(facet_var)) {p <- p + ggplot2::facet_wrap(~.data[[facet_var]], scales=scales, ncol = ncol, dir='v')}

    plots[[i]] <- p
  }

  if (length(plots)==1) {plots <- plots[[1]]}

  # return(plots)

  if (extract_data) {return(data)} else {return(plots)}

}

#' Unusual catch table
#'
#' @param data Output of format_logbooks()
#' @param max_days Numeric. Filter to include only records that were less than or equal to this maximum number of fishing days (i.e. exclude multi-day trips)
#' @param quantile Operators are identified with maximum daily catch is `multiplier` times higher than `quantile` percentile
#' @param multiplier Operators are identified with maximum daily catch is `multiplier` times higher than `quantile` percentile
#' @param species_of_interest The name of the species of interest, as listed in the 'species' column of data.
#' @param interesting_fishers A vector of interesting fishers by which the data will be filtered
#' @param interesting_years A vector of interesting years by which the data will be filtered
#'
#' @return A dataframe of fishers whose activity has been flagged as unusual. This is defined as any fisher whose maximum catch record is greater than 10 times the 90th quantile of all their catch records. The multiplier and quantile can be specified.
#' @export
#'
#' @examples
#' \dontrun{unusualcatch(data = format_logbooks(raw_data))}
unusualcatch <- function(data,
                         species_of_interest = NULL,
                         interesting_fishers = NULL,
                         interesting_years = NULL,
                         max_days = 1,
                         quantile = 0.9,
                         multiplier = 10) {
  quan <- quantile

  if (!missing(species_of_interest)) {data <- data |> dplyr::filter(species %in% species_of_interest)}
  if (!missing(interesting_fishers)) {data <- data |> dplyr::filter(operator %in% interesting_fishers)}
  if (!missing(interesting_years)) {data <- data |> dplyr::filter(year %in% interesting_years)}

  unusualcatch <- data |>
    dplyr::filter(maximum_fishing_day_count <= max_days) |>
    dplyr::group_by(operator) |>
    dplyr::summarise(quant = quantile(weight, quan), max = max(weight), .groups='drop') |>
    dplyr::filter(max > multiplier*quant) |>
    dplyr::arrange(dplyr::desc(max))

  return(unusualcatch)
}



#' Unusual catch plot
#'
#' @param data Output of format_logbooks()
#' @param interesting_fishers A vector of interesting fishers by which the data will be filtered
#' @param max_days Numeric. Filter to include only records that were less than or equal to this maximum number of fishing days (i.e. exclude multi-day trips)
#' @param extract_data Set to TRUE to return data instead of plot. Default is FALSE.
#'
#' @return A plot of fishers whose activity has been flagged as unusual. This is defined as any fisher whose maximum catch record is greater than 10 times the 90th quantile of all their catch records. The multiplier and quantile can be specified.
#' @export
#'
#' @examples
#' \dontrun{unusualcatchplot(data = format_logbooks(raw_data))}
unusualcatchplot <- function(data,
                             max_days = 1,
                             interesting_fishers = NULL,
                             extract_data = FALSE
                             ) {


  if (!missing(interesting_fishers)) {
    data_filt <- data |>
      dplyr::filter(operator %in% interesting_fishers)
  } else {
    data_filt <- data
  }

  p <- ggplot2::ggplot(data_filt, ggplot2::aes(x = date, y = weight)) +
    ggplot2::geom_point() +
    ggplot2::geom_segment(ggplot2::aes(x=date, xend = date, y = 0, yend = weight )) +
    ggplot2::facet_wrap(~operator, ncol= 1, dir = "v", strip.position = "right",scales='free_y') +
    ggplot2::theme_bw() +
    ggplot2::ylab("Retained catch (kg)") +
    ggplot2::xlab("Date")

  if (extract_data) {return(data_filt)} else {return(p)}
  # return(p)
}

