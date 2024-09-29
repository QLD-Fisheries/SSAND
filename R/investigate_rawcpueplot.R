# Copyright 2024 Fisheries Queensland

# This file is part of SSAND.
# SSAND is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
# SSAND is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
# You should have received a copy of the GNU General Public License along with SSAND. If not, see <https://www.gnu.org/licenses/>.

#' Raw CPUE plot
#'
#' @param data Output of format_logbooks()
#' @param species_of_interest Optional. The name of the species of interest, as listed in the 'species' column of data. Filters data to only include that species.
#' @param interesting_fishers Optional. A vector of fishers/operators by which to filter the dataset.
#' @param show_monthly Set to TRUE to display by month
#' @param max_days Numeric. Filter to include only records that were less than or equal to this maximum number of fishing days (i.e. exclude multi-day trips)
#' @param xlab Label for x-axis of plot (character). Default is "".
#' @param ylab Label for y-axis of plot (character). Default is "".
#' @param show_region Set to TRUE to display by region
#' @param show_latband Set to TRUE to display by latitude band
#' @param show_latband_regions Set to TRUE to display by latitude band grouping
#' @param remove_NA_latband Set to TRUE to remove data without valid latitude band.
#' @param scales Scales used in facet_wrap. Deafult is 'free'
#' @param facet_labels A vector used to customise facets.
#' @param ncol Number of columns for facet wrap. Default is 2.
#' @param facet_var Variable by which plot is to be faceted (e.g. "region")
#' @param colour_var Variable by which plot is to be coloured (e.g. "region")
#' @param colours A vector of colours for the plot
#' @param legend_position Legend position
#' @param legend_title Legend text
#' @param extract_data Set to TRUE to return data instead of plot. Default is FALSE.
#'
#' @return Raw CPUE plot
#' @export
#'
#' @examples
#' \dontrun{rawcpueplot(data = format_logbooks(raw_data))}
rawcpueplot <- function(data,
                        show_monthly = FALSE,
                        species_of_interest = NULL,
                        interesting_fishers = NULL,
                        max_days = 1,
                        xlab = "Year",
                        ylab = "Raw catch per unit effort (kg/fishing day)",
                        show_region = FALSE,
                        show_latband = FALSE,
                        show_latband_regions = FALSE,
                        remove_NA_latband = FALSE,
                        facet_labels = NULL,
                        scales='free',
                        ncol = 2,
                        colour_var = "region",
                        facet_var = NULL,
                        colours = rep(fq_palette("alisecolours"),10),
                        legend_position = "top",
                        legend_title = "",
                        extract_data = FALSE) {


  if (remove_NA_latband) {data <- data |> dplyr::filter(!is.na(latband))}
  if (show_latband_regions) {data <- data |> dplyr::mutate(region = latband_group_name); show_region <- TRUE}
  if (!"region" %in% names(data)) {data <- data |> dplyr::mutate(region = latband_group_name)}

  if (!missing(interesting_fishers)) {data <- data |> dplyr::filter(operator %in% interesting_fishers)}
  if (!missing(species_of_interest)) {data <- data |> dplyr::filter(species %in% species_of_interest)}

  if ("maximum_fishing_day_count" %in% names(data)) {
    data <- data |> dplyr::filter(maximum_fishing_day_count <= max_days)
  } else {
    warning("The variable 'maximum_fishing_day_count' is not present in your dataset so not filtering out multi-day trips")
  }

  median <- quantile(data$weight,0.5)
  data <- data |>
    dplyr::mutate(quantiles = ifelse(weight<median,"Low catch (below median)","High catch (above median)"))


  if (missing(colour_var) && missing(facet_var)) {data <- data |> dplyr::group_by(year)}
  if (!missing(colour_var) && missing(facet_var)) {data <- data |> dplyr::group_by(year,.data[[colour_var]])}
  if (missing(colour_var) && !missing(facet_var)) {data <- data |> dplyr::group_by(year,.data[[facet_var]])}

  if (!missing(colour_var) && !missing(facet_var) && colour_var!=facet_var) {
    if (length(facet_var)==1) {
      data <- data |> dplyr::group_by(year,.data[[colour_var]],.data[[facet_var]])
    } else {
      data <- data |> dplyr::group_by(year,.data[[colour_var]],.data[[facet_var[[1]]]],.data[[facet_var[[2]]]])
    }
  }

  if (!missing(colour_var) && !missing(facet_var) && colour_var==facet_var) {
    if (length(facet_var)==1) {
      data <- data |> dplyr::group_by(year,.data[[facet_var]])
    } else {
      data <- data |> dplyr::group_by(year,.data[[facet_var[[1]]]],.data[[facet_var[[2]]]])
    }
  }


  data <- data |>
    dplyr::summarise(rate = sum(weight)/dplyr::n(), .groups = 'drop')

  p <- ggplot2::ggplot(data)

  if (missing(colour_var)) {
    p <- p + ggplot2::geom_line(ggplot2::aes(x=year,y=rate))
    p <- p + ggplot2::geom_point(ggplot2::aes(x=year,y=rate))
  } else {
    p <- p + ggplot2::geom_line(ggplot2::aes(x=year,y=rate,colour=.data[[colour_var]]))
    p <- p + ggplot2::geom_point(ggplot2::aes(x=year,y=rate,colour=.data[[colour_var]]))
  }

  p <- p +
    ggplot2::theme_bw() +
    ggplot2::xlab(xlab) +
    ggplot2::ylab(ylab) +
    ggplot2::ylim(0,NA) +
    ggplot2::scale_colour_manual(name = legend_title, values=colours) +
    ggplot2::theme(legend.position = legend_position)

  if (!missing(facet_var)) {
    if (length(facet_var)==1) {
      p <- p + ggplot2::facet_wrap(~.data[[facet_var]], scales=scales, ncol = ncol, dir='v')
    } else {
      facet_formula <- as.formula(paste(facet_var[1], "~", facet_var[2]))
      p <- p + ggplot2::facet_grid(facet_formula, scales=scales)
    }
  }

  if (extract_data) {return(data)} else {return(p)}
}

