# Copyright 2024 Fisheries Queensland

# This file is part of SSAND.
# SSAND is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
# SSAND is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
# You should have received a copy of the GNU General Public License along with SSAND. If not, see <https://www.gnu.org/licenses/>.

#' Upset plot to display co-caught species
#' Groups data by interview (BRS) or unique ACN-fisherday (CFISH) and plots frequency of co-caught species
#' The plot shows the frequency of different combinations of species caught on the same interview (BRS) or ACN-day, ranked by the most common combination.
#'
#' @param data A raw data file of catch, either from the Boat Ramp Survey or CFISH logbooks.
#' @param source Either "BRS" if using boat ramp survey data or "CFISH" if using logbook data.
#' If using "BRS" the code looks for columns called SpeciesName, SiteID, FishingMethodCode
#' If using "CFISH" the code looks for operator, date, species, method. Use `format_logbooks()` to format data.
#' If using a different source, ensure columns called species, SiteID, method are included.
#' @param species_of_interest Species name to display. Should appear in SpeciesName column if source=="BRS" or CAABSpeciesCommonName if source=="CFISH"
#' @param min_records Cut off value for tail end of plot. Do not plot species combinations who frequency is below this value.
#' @param show_fishing_method Set to TRUE to display sub-plot of to show fishing methods used, when more than one method was used.
#' @param colours A vector of colours used (character).
#' @param xlab_upset Label for x-axis of upset plot (character). Default is "".
#' @param ylab_upset Label for y-axis of upset plot (character). Default is "".
#' @param xlab_method Label for x-axis of fishing method plot (character). Default is "".
#' @param ylab_method Label for y-axis of fishing method plot (character). Default is "".
#' @param ncol Number of columns for facet wrap. Default is 2.
#' @param scales Scales for facet wrap. Default is 'free'
#' @param extract_data Set to TRUE to return data instead of plot. Default is FALSE.
#'
#' @return An upset plot to display co-caught species
#' @export
#'
#' @examples
#' head(upset) # View sample data
#' upsetplot(upset, species_of_interest = "Glitterfin snapper")
#'
#' \dontrun{
#' data <- format_logbooks(raw_data)
#' upsetplot(data, source = "CFISH", species_of_interest = "Prawn - tiger", min_records=100)
#' }
upsetplot <- function(data,
                      source = "BRS",
                      species_of_interest = NULL,
                      min_records = 1,
                      show_fishing_method = FALSE,
                      colours = c("#ff595e", "#ffca3a","#8ac926","#1982c4","#6a4c93"),
                      xlab_upset = "",
                      ylab_upset = "",
                      xlab_method = "",
                      ylab_method = "",
                      # show_region = FALSE,
                      # show_region_coarse = FALSE,
                      ncol = 2,
                      scales = 'free',
                      extract_data = FALSE) {

  # if ("region" %in% names(data) && "region_coarse" %in% names(data) && show_region_coarse) {data <- data |> dplyr::mutate(region = region_coarse); show_region <- TRUE}

  if (length(species_of_interest)>1) {species_of_interest <- species_of_interest[[1]]}

  # Upset plot
  if (!show_fishing_method) {
    if (!source=="CFISH") { # BRS or other

      if ("SpeciesName" %in% names(data)) {data <- data |> dplyr::rename(species = SpeciesName) }
      if ("FishingMethodCode" %in% names(data)) {data <- data |> dplyr::rename(method = FishingMethodCode) }

      interviews_with_species <- data |>
        dplyr::filter(species == species_of_interest) |>
        dplyr::pull(SiteID) |>
        unique()

      upset_prep <- data |>
        dplyr::filter(!species == "Nil") |>
        dplyr::filter(SiteID %in% interviews_with_species) |>
        dplyr::group_by(SiteID) |>
        dplyr::summarise(SpeciesName = list(species),
                         .groups='drop') |>
        dplyr::group_by(SpeciesName) |>
        dplyr::mutate(n_trips = dplyr::n()) |>
        dplyr::ungroup() |>
        dplyr::filter(n_trips >= min_records)
    }

    if (source=="CFISH") {
      data <- data |>
        dplyr::mutate(SiteID = paste0(operator,date))

      interviews_with_species <- data |>
        dplyr::filter(species == species_of_interest) |>
        dplyr::pull(SiteID) |>
        unique()

      upset_prep <- data |>
        dplyr::filter(SiteID %in% interviews_with_species) |>
        dplyr::arrange(species) # sort species names so that duplicates aren't created by the change in order species are listed below.

      # if (show_region) {
      #   upset_prep <- upset_prep |> dplyr::group_by(SiteID, region)
      # } else {
        upset_prep <- upset_prep |> dplyr::group_by(SiteID)
      # }

      upset_prep <- upset_prep |>
        dplyr::summarise(SpeciesName = list(species), .groups='drop')

      # if (show_region) {
      #   upset_prep <- upset_prep |> dplyr::group_by(SpeciesName, region)
      # } else {
        upset_prep <- upset_prep |> dplyr::group_by(SpeciesName)
      # }

      upset_prep <- upset_prep |>
        dplyr::mutate(n_trips = dplyr::n()) |>
        dplyr::ungroup() |>
        dplyr::filter(n_trips >= min_records)
    }

    p <- ggplot2::ggplot(upset_prep) +
      ggplot2::geom_bar(ggplot2::aes(x=SpeciesName), fill=colours[1]) +
      ggupset::scale_x_upset() +
      ggplot2::theme_bw() +
      ggupset::theme_combmatrix(combmatrix.label.make_space = TRUE) +
      ggplot2::xlab(xlab_upset) +
      ggplot2::ylab(ylab_upset) +
      ggplot2::ggtitle(paste0("Fishing trips that contained a ",tolower(species_of_interest))) +
      ggplot2::theme(plot.margin = ggplot2::margin(1,1,1,4, "cm"))

    # if (show_region) {
    #   p <- p + ggplot2::facet_wrap(~region, scales=scales, ncol=ncol)
    # }
  }

  # FishingMethodType plot
  if (show_fishing_method) {
    if (!source=="CFISH") {

      if ("SpeciesName" %in% names(data)) {data <- data |> dplyr::rename(species = SpeciesName) }
      if ("FishingMethodCode" %in% names(data)) {data <- data |> dplyr::rename(method = FishingMethodCode) }

      interviews_with_species <- data |>
        dplyr::filter(species == species_of_interest) |>
        dplyr::pull(SiteID) |>
        unique()

      upset_prep <- data |>
        dplyr::filter(!species == "Nil") |>
        dplyr::filter(SiteID %in% interviews_with_species) |>
        dplyr::group_by(SiteID) |>
        dplyr::summarise(SpeciesName = list(species),
                         nFishingMethodCode = length(unique(method)),
                         FishingMethodCode = list(method),
                         .groups='drop') |>
        dplyr::group_by(SpeciesName) |>
        dplyr::mutate(n_trips = dplyr::n()) |>
        dplyr::ungroup() |>
        dplyr::filter(n_trips >= min_records)
    }

    if (source=="CFISH") {
      data <- data |>
        dplyr::mutate(SiteID = paste0(operator,date))

      interviews_with_species <- data |>
        dplyr::filter(species == species_of_interest) |>
        dplyr::pull(SiteID) |>
        unique()

      upset_prep <- data |>
        dplyr::filter(SiteID %in% interviews_with_species) |>
        dplyr::group_by(SiteID) |>
        dplyr::summarise(SpeciesName = list(species),
                         nFishingMethodCode = length(unique(method)),
                         FishingMethodCode = list(method),
                         .groups='drop') |>
        dplyr::group_by(SpeciesName) |>
        dplyr::mutate(n_trips = dplyr::n()) |>
        dplyr::ungroup() |>
        dplyr::filter(n_trips >= min_records)
    }

    p <- ggplot2::ggplot(upset_prep) +
      ggplot2::geom_bar(ggplot2::aes(x=SpeciesName, fill=as.factor(nFishingMethodCode))) +
      ggupset::scale_x_upset() +
      ggplot2::theme_bw() +
      ggupset::theme_combmatrix(combmatrix.label.make_space = TRUE) +
      ggplot2::scale_fill_manual(values = colours,
                                 name = "Number of unique fishing methods") +
      ggplot2::theme(legend.position="top") +
      ggplot2::xlab(xlab_upset) +
      ggplot2::ylab(ylab_upset) +
      ggplot2::ggtitle(paste0("Fishing trips that contained a ",tolower(species_of_interest)))

    list_mixed_fishingmethods <- upset_prep |>
      dplyr::filter(nFishingMethodCode > 1) |>
      dplyr::pull(SiteID)

    if (length(list_mixed_fishingmethods)) {
      method <- ggplot2::ggplot(data |> dplyr::filter(SiteID %in% list_mixed_fishingmethods)) +
        ggplot2::geom_point(ggplot2::aes(x=method,
                                         y=species,
                                         colour=method,
                                         shape=method)) +
        ggplot2::facet_wrap(~SiteID) +
        ggplot2::theme_bw() +
        ggplot2::theme(legend.title=ggplot2::element_blank(), legend.position="top") +
        ggplot2::scale_colour_manual(values = colours) +
        ggplot2::xlab(xlab_method) +
        ggplot2::ylab(ylab_method) +
        ggplot2::guides(shape=ggplot2::guide_legend(nrow=2,byrow=TRUE),
                        colour=ggplot2::guide_legend(nrow=2,byrow=TRUE)) +
        ggplot2::theme(axis.title.x=ggplot2::element_blank(),
                       axis.text.x=ggplot2::element_blank(),
                       axis.ticks.x=ggplot2::element_blank())

      p <- cowplot::plot_grid(p,method, ncol=2, rel_widths = c(0.7,0.3))
    }
  }
  if (extract_data) {return(upset_prep)} else {return(p)}
}
