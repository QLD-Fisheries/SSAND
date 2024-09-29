# Copyright 2024 Fisheries Queensland

# This file is part of SSAND.
# SSAND is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
# SSAND is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
# You should have received a copy of the GNU General Public License along with SSAND. If not, see <https://www.gnu.org/licenses/>.

#' Raw catch or effort plot
#'
#' @param data Output of format_logbooks()
#' @param y_var Variable on the y axis. Either "catch" or "effort"
#' @param species_of_interest The name of the species of interest, as listed in the 'species' column of data.
#' @param annual Set to TRUE to aggregate by year. Set to FALSE to show overall catch
#' @param threshold The percentile threshold used to 'zoom' in on the right hand side plot. Only for boxplots.
#' @param max_days Numeric. Filter to include only records that were less than or equal to this maximum number of fishing days (i.e. exclude multi day trips)
#' @param xlab Label for x-axis (character). Default is "Year".
#' @param ylab Label for y-axis (character). Default is "Daily catch (kg)".
#' @param facet_labels A vector of two strings, labels of each facet panel.
#' @param boxplot Set to TRUE to display as a boxplot. Set to FALSE to display as a barplot.
#' @param ncol Number of columns for facet wrap. Default is 2.
#' @param facet_var Variable by which plot is to be faceted (e.g. "region")
#' @param fill_var Variable by which plot is to be filled (e.g. "region")
#' @param scales Scaled for facet wrap. Default is 'fixed'.
#' @param dodge Set to TRUE to offset bars, set to FALSE to stack them
#' @param interesting_years Optional. A vector of years to filter the dataset; only fishers operating in this years will be included.
#' @param interesting_fishers Optional. A vector of fishers/operators to filter the dataset by.
#' @param show_proportion Set to TRUE to show relative proportions
#' @param proportion_only Set to TRUE to show only relative proportions
#' @param colours Colours for plot
#' @param legend_position Legend position
#' @param legend_title Legend text
#' @param include_method (Optional) A vector of fishing methods to include (if filling by method)
#' @param n_method Maximum number of fishing methods to include (if filling by method)
#' @param include_species (Optional) A vector of species to include (if filling by species)
#' @param n_species Maximum number of species to include (if filling by species)
#' @param extract_data Set to TRUE to return data instead of plot. Default is FALSE.
#'
#' @return Raw catch or effort plot
#' @export
rawcatcheffortplot <- function(data,
                               y_var = "catch",
                               species_of_interest = NULL,
                               interesting_years = NULL,
                               interesting_fishers = NULL,
                               show_proportion = FALSE,
                               proportion_only = FALSE,
                               threshold = NULL,
                               annual = TRUE,
                               boxplot = FALSE,
                               fill_var = "region",
                               facet_var = NULL,
                               max_days = 1,
                               xlab = "Year",
                               ylab = NULL, # "Daily catch (kg)",
                               facet_labels = NULL,
                               scales = 'fixed',
                               ncol = 2,
                               dodge = FALSE,
                               colours = rep(fq_palette("alisecolours"),10),
                               legend_position = "top",
                               legend_title = "",
                               include_method = NULL,
                               n_method = 15,
                               include_species = NULL,
                               n_species = 15,
                               extract_data = FALSE
) {

  # Filter data appropriately
  if (!missing(species_of_interest) && fill_var!="species") {data <- data |> dplyr::filter(species %in% species_of_interest)}
  if (!missing(interesting_years)) {data <- data |> dplyr::filter(year %in% interesting_years)}
  if (!missing(interesting_fishers)) {data <- data |> dplyr::filter(operator %in% interesting_fishers)}

  if ("maximum_fishing_day_count" %in% names(data)) {
    data <- data |> dplyr::filter(maximum_fishing_day_count <= max_days)
  } else {
    warning("The variable 'maximum_fishing_day_count' is not present in your dataset so not filtering out multi-day trips")
  }

  # Set up for catch or effort
  if (y_var == "catch") {
    data <- data |> dplyr::mutate(y=weight)
    if (missing(ylab) && boxplot) {ylab = "Daily retained catch (kg)"}
    if (missing(ylab) && !boxplot) {ylab = "Retained catch (kg)"}
    prop_label <- "Proportion of retained catch"

    if (missing(facet_labels)) {
      if (missing(threshold)) {
        facet_labels <- c("All catch", "Lower 90% of catch")
      } else {
        facet_labels <- c("All catch", paste0("Catch <=",threshold," kg"))
      }
    }
  }

  if (y_var == "effort") {
    data <- data |> dplyr::mutate(y=1)
    if (missing(ylab) && boxplot) {ylab = "Daily effort (days)"}
    if (missing(ylab) && !boxplot) {ylab = "Effort (days)"}
    prop_label <- "Proportion of retained effort"

    if (missing(facet_labels)) {
      if (missing(threshold)) {
        facet_labels <- c("All effort", "Lower 90% of effort")
      } else {
        facet_labels <- c("All effort", paste0("Effort <=",threshold," days"))
      }
    }
  }

  # There could be a large number of methods or species. Cap the number you fill or facet by.
  if (length(c(fill_var,facet_var))>0) {
    if ("method" %in% c(fill_var,facet_var)) {
      if (missing(include_method)) {
        include_method <- data |>
          dplyr::group_by(method) |>
          dplyr::summarise(y = sum(y), .groups='drop') |>
          dplyr::arrange(dplyr::desc(y)) |>
          head(n_method) |>
          dplyr::select(method) |>
          dplyr::pull()
      }
      data <- data |> dplyr::mutate(method = ifelse(method %in% include_method, method, "Other"))
    }

    if ("species" %in% c(fill_var,facet_var)) {
      if (length(species_of_interest)>1) {species_of_interest <- species_of_interest[[1]]}

      # If species_of_interest listed, include only fishing trips where that species was caught
      if (!missing(species_of_interest)) {
        present <- data |>
          dplyr::group_by(operator,date) |>
          dplyr::mutate(present = ifelse(species_of_interest %in% species, 1, 0)) |>
          dplyr::ungroup() |>
          dplyr::select(present)

        data <- data |>
          cbind(present) |>
          dplyr::filter(present == 1) |>
          dplyr::select(-present)
      }

      # Cap the number of species included in the summary
      if (missing(include_species)) {
        include_species <- data |>
          dplyr::group_by(species) |>
          dplyr::summarise(weight = sum(weight), .groups='drop') |>
          dplyr::arrange(dplyr::desc(weight)) |>
          head(n_species) |>
          dplyr::select(species) |>
          dplyr::pull()
      }
      data <- data |> dplyr::mutate(species = ifelse(species %in% include_species, species, "Other"))
    }
  }


  if (boxplot) {
    if (missing(threshold)) {threshold <- quantile(data$y,0.90)}

    data <- data |>
      dplyr::mutate(category = ifelse(y <= threshold, facet_labels[2], facet_labels[1]))

    if (!annual) {
      p <- ggplot2::ggplot(data) +
        ggplot2::geom_boxplot(ggplot2::aes(x=factor(" "), y=y))  +
        ggplot2::theme(axis.title.x=ggplot2::element_blank(),
                       axis.text.x=ggplot2::element_blank(),
                       axis.ticks.x=ggplot2::element_blank())
    }
    if (annual) {
      p <- ggplot2::ggplot(data) +
        ggplot2::geom_boxplot(ggplot2::aes(x=year, y=y, group=year))
    }

    p <- p +
      ggplot2::theme_bw() +
      ggplot2::xlab(xlab) +
      ggplot2::ylab(ylab) +
      ggplot2::ylim(0,NA)

    if (missing(facet_var)) {facet_var <- "category"}

    if (length(facet_var)==1) {
      p <- p + ggplot2::facet_wrap(~.data[[facet_var]], scales=scales, ncol = ncol, dir='v')
    } else {
      facet_formula <- as.formula(paste(facet_var[1], "~", facet_var[2]))
      p <- p + ggplot2::facet_grid(facet_formula, scales=scales)
    }
  }

  if (!boxplot) {
    if (show_proportion) {
      # if (missing(fill_var)) {prop <- data |> dplyr::group_by(year)}
      # if (!missing(fill_var)) {prop <- data |> dplyr::group_by(year,.data[[fill_var]])}
      # if (!missing(fill_var)) {prop <- data |> dplyr::group_by(year,.data[[fill_var]])}

      if (missing(fill_var) && missing(facet_var)) {prop <- data |> dplyr::group_by(year)}
      if (!missing(fill_var) && missing(facet_var)) {prop <- data |> dplyr::group_by(year,.data[[fill_var]])}
      if (missing(fill_var) && !missing(facet_var)) {prop <- data |> dplyr::group_by(year,.data[[facet_var]])}

      if (!missing(fill_var) && !missing(facet_var) && fill_var!=facet_var) {
        if (length(facet_var)==1) {
          prop <- data |> dplyr::group_by(year,.data[[fill_var]],.data[[facet_var]])
        } else {
          prop <- data |> dplyr::group_by(year,.data[[fill_var]],.data[[facet_var[[1]]]],.data[[facet_var[[2]]]])
        }
      }

      if (!missing(fill_var) && !missing(facet_var) && fill_var==facet_var) {
        if (length(facet_var)==1) {
          prop <- data |> dplyr::group_by(year,.data[[facet_var]])
        } else {
          prop <- data |> dplyr::group_by(year,.data[[facet_var[[1]]]],.data[[facet_var[[2]]]])
        }
      }

      prop <- prop |>
        dplyr::summarise(y = sum(y), .groups='drop')

      if (!missing(facet_var)) {
        prop <- prop |> dplyr::group_by(year,.data[[facet_var]])
      } else {
        prop <- prop |> dplyr::group_by(year)
      }

      # dplyr::group_by(year) |>
      prop <- prop |>
        dplyr::mutate(proportion = y/sum(y)) |>
        dplyr::ungroup() |>
        dplyr::select(-y) |>
        dplyr::rename(y = proportion) |>
        dplyr::mutate(category = prop_label)

      data <- data |>
        dplyr::mutate(category = ylab) |>
        dplyr::select(names(prop))|>
        rbind(prop)  |>
        dplyr::mutate(category = factor(category, levels = c(ylab, prop_label)))

      if (proportion_only) {data <- data |> dplyr::filter(category == prop_label)}
      if (!proportion_only) {facet_var <- "category"}
    }

    p <- ggplot2::ggplot(data)

    if (missing(fill_var)) {
      p <- p + ggplot2::geom_bar(ggplot2::aes(x=year,y=y), stat='identity')
    } else {
      if (dodge) {
        p <- p + ggplot2::geom_bar(ggplot2::aes(x=year,y=y,fill=.data[[fill_var]]), stat='identity', position='dodge2')
      } else {
        p <- p + ggplot2::geom_bar(ggplot2::aes(x=year,y=y,fill=.data[[fill_var]]), stat='identity')
      }
    }

    if (!missing(facet_var)) {
      if (length(facet_var)==1) {
        p <- p + ggplot2::facet_wrap(~.data[[facet_var]], scales=scales, ncol = ncol, dir='v')
      } else {
        facet_formula <- as.formula(paste(facet_var[1], "~", facet_var[2]))
        p <- p + ggplot2::facet_grid(facet_formula, scales=scales)
      }
    }

    p <- p +
      ggplot2::theme_bw() +
      ggplot2::xlab(xlab) +
      ggplot2::ylab(ylab) +
      ggplot2::ylim(0,NA) +
      ggplot2::scale_fill_manual(name=legend_title, values=colours) +
      ggplot2::theme(legend.position = legend_position)
  }

  if (extract_data) {return(data)} else {return(p)}
}
