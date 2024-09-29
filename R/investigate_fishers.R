# Copyright 2024 Fisheries Queensland

# This file is part of SSAND.
# SSAND is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
# SSAND is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
# You should have received a copy of the GNU General Public License along with SSAND. If not, see <https://www.gnu.org/licenses/>.

#' Rank fishers
#'
#' @param data Output of format_logbooks()
#' @param show_year Set to TRUE to show year in table. Set to FALSE to show total number of years operator has been in fishery.
#' @param round Set to TRUE to round total catch.
#' @param species_of_interest The name of the species of interest, as listed in the 'species' column of data.
#'
#' @return A table of fishers ranked by their total catch
#' @export
#'
#' @examples
#' \dontrun{rank_fishers(data = format_logbooks(raw_data))}
rank_fishers <- function(data,
                         species_of_interest = NULL,
                         show_year = TRUE,
                         round = TRUE) {

  if (!missing(species_of_interest)) {data <- data |> dplyr::filter(species %in% species_of_interest)}

  if (!show_year) {
    data <- data |>
      dplyr::group_by(year,operator) |>
      dplyr::summarise(days = dplyr::n(), catch = sum(weight), .groups='drop') |>
      dplyr::group_by(operator) |>
      dplyr::summarise(n_years = dplyr::n(),
                       days = sum(days),
                       catch = sum(catch),
                       .groups = 'drop') |>
      dplyr::arrange(dplyr::desc(catch)) |>
      dplyr::mutate(p = round(catch/sum(catch)*100,1)) |>
      dplyr::mutate(p_cum = cumsum(p))
  } else {
    data <- data |>
      dplyr::group_by(year,operator) |>
      dplyr::summarise(days = dplyr::n(),
                       catch = sum(weight),
                       .groups = 'drop') |>
      dplyr::arrange(dplyr::desc(catch)) |>
      dplyr::mutate(p = round(catch/sum(catch)*100,1)) |>
      dplyr::mutate(p_cum = cumsum(p))
  }

  if (round) {data$catch <- round(data$catch)}

  return(data)
}



#' Top fishers plot
#'
#' @param data Output of format_logbooks()
#' @param show_daily Set to TRUE to show daily catch of top fishers. Set to FALSE plot the total catch of fishers.
#' @param n_top_fishers Numeric. Maximum number of top fishers to display on plot. Default is 20.
#' @param interesting_years Optional. A vector of years to filter the dataset; only fishers operating in this years will be included.
#' @param interesting_fishers Optional. A vector of fishers/operators to filter the dataset by.
#' @param show_region Set to TRUE to display top fishers by region
#' @param show_latband Set to TRUE to display top fishers by latitude band
#' @param show_latband_regions Set to TRUE to display top fishers by latitude band grouping
#' @param remove_NA_latband Set to TRUE to remove data without valid latitude band.
#' @param species_of_interest The name of the species of interest, as listed in the 'species' column of data.
#' @param extract_data Set to TRUE to return data instead of plot. Default is FALSE.
#'
#' @return A plot of top fishers in the fishery
#' @export
#'
#' @examples
#' \dontrun{topfishersplot(data = format_logbooks(raw_data))}
topfishersplot <- function(data,
                           show_daily = TRUE,
                           species_of_interest = NULL,
                           n_top_fishers = 10,
                           interesting_years = NULL,
                           interesting_fishers = NULL,
                           show_region = FALSE,
                           show_latband = FALSE,
                           show_latband_regions = FALSE,
                           remove_NA_latband = FALSE,
                           extract_data = FALSE
) {

  if (!missing(species_of_interest)) {data <- data |> dplyr::filter(species %in% species_of_interest)}

  if (!missing(interesting_fishers)) {data <- data |> dplyr::filter(operator %in% interesting_fishers)}

  # Arrange fisher by total catch
  fishers_rank <- rank_fishers(data, show_year=FALSE)
  fishers_rank_year <- rank_fishers(data, show_year=TRUE)

  if (!missing(interesting_years)) {
    fishers_in_interesting_years <- data |>
      dplyr::group_by(year,operator) |>
      dplyr::summarise(days = dplyr::n(), catch = sum(weight), .groups='drop') |>
      dplyr::filter(year %in% interesting_years) |>
      dplyr::distinct(operator)

    fishers_rank <- fishers_rank |> dplyr::filter(operator %in% fishers_in_interesting_years$operator)
    fishers_rank_year <- fishers_rank_year |> dplyr::filter(operator %in% fishers_in_interesting_years$operator)
  }

  if (!show_daily) {
    # Top n fishers total catch # top_fisher_catch
    p <- fishers_rank |>
      head(n_top_fishers) |>
      dplyr::mutate(operator = factor(operator, levels = fishers_rank$operator[n_top_fishers:1])) |>
      ggplot2::ggplot() +
      ggplot2::geom_bar(ggplot2::aes(x= operator, y = catch), stat="identity", alpha = 0.7) +
      ggplot2::ylab("Total retained catch over all years") +
      ggplot2::xlab("Fisher") +
      ggplot2::theme_bw() +
      ggplot2::coord_flip()
  } else {
    # Top n fisher daily catch # top_fisher_daily_catch
    data <- data |>
      dplyr::filter(operator %in% fishers_rank$operator[1:n_top_fishers]) |>
      dplyr::mutate(operator = factor(operator, levels = fishers_rank$operator[1:n_top_fishers]))

    p <- ggplot2::ggplot(data, ggplot2::aes(x = date, y = weight)) +
      ggplot2::geom_point() +
      ggplot2::geom_segment(ggplot2::aes(x=date, xend = date, y = 0, yend = weight )) +
      ggplot2::facet_wrap(~operator, ncol= 1, dir = "v", strip.position = "right",scales='free_y') +
      ggplot2::theme_bw() +
      ggplot2::xlab("Year") +
      ggplot2::ylab("Retained catch (kg)")
  }

  if (extract_data) {return(data)} else {return(p)}
}



#' Number of fishers plot
#'
#' @param data Output of format_logbooks()
#' @param species_of_interest The name of the species of interest, as listed in the 'species' column of data.
#' @param interesting_years Optional. A vector of years to filter the dataset; only fishers operating in this years will be included.
#' @param interesting_fishers Optional. A vector of fishers/operators to filter the dataset by.
#' @param show_region Set to TRUE to display fishers by region
#' @param show_latband Set to TRUE to display fishers by latitude band
#' @param show_latband_regions Set to TRUE to display fishers by latitude band grouping
#' @param remove_NA_latband Set to TRUE to remove data without valid latitude band.
#' @param show_total Set to TRUE to show total number of fishers in fishery on plot.
#' @param extract_data Set to TRUE to return data instead of plot. Default is FALSE.
#'
#' @return A plot of the number of fishers in the fishery
#' @export
#'
#' @examples
#' \dontrun{nfishersplot(data = format_logbooks(raw_data))}
nfishersplot <- function(data,
                         species_of_interest = NULL,
                         interesting_years = NULL,
                         interesting_fishers = NULL,
                         show_region = FALSE,
                         show_latband = FALSE,
                         show_latband_regions = FALSE,
                         remove_NA_latband = FALSE,
                         show_total = FALSE,
                         extract_data = FALSE
) {

  if (!missing(interesting_fishers)) {data <- data |> dplyr::filter(operator %in% interesting_fishers)}
  if (!missing(species_of_interest)) {data <- data |> dplyr::filter(species %in% species_of_interest)}

  fishers <- rank_fishers(data, show_year = TRUE)

  if (!missing(interesting_years)) {
    fishers_in_interesting_years <- fishers |>
      dplyr::filter(year %in% interesting_years) |>
      dplyr::distinct(operator)

    fishers <- fishers |> dplyr::filter(operator %in% fishers_in_interesting_years$operator)
  }

  # Number of fishers operating in each year
  n_fishers <- fishers |>
    dplyr::group_by(year) |>
    dplyr::summarise(n_fishers = dplyr::n(), days = sum(days), catch = sum(catch), .groups= 'drop')

  # number_of_fishers
  p <- ggplot2::ggplot(n_fishers) +
    ggplot2::geom_bar(ggplot2::aes(x= year, y = n_fishers), stat="identity", alpha = 0.7) +
    ggplot2::ylab("Number of fishers") +
    ggplot2::xlab("Year") +
    ggplot2::theme_bw()

  if (show_total) {
    p <- p +
      ggplot2::annotate("text",label=paste0(length(unique(data$operator))," fishers ever"),x=max(data$year)-1, y=max(n_fishers$n_fishers), hjust=1)
  }
  if (extract_data) {return(n_fishers)} else {return(p)}
}


