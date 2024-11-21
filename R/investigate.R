# Copyright 2024 Fisheries Queensland

# This file is part of SSAND.
# SSAND is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
# SSAND is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
# You should have received a copy of the GNU General Public License along with SSAND. If not, see <https://www.gnu.org/licenses/>.

#' Investigate raw catch and effort data
#'
#' This function produces a dashboard to visualise raw catch and effort data.
#' It produces a document (Rmd, which complies to a HTML) that shows various data summaries and investigations.
#' If render = FALSE, you will produce just the Rmd (not HTML). This allows you to then tweak the document before rendering. Note that you will need to manually load your data set into the first chunk of the Rmd file, ensuring it is called 'data' to feed into the subsequent chunks.
#' This function can take a long time to run if using a large dataset. We suggest you test the function on a smaller dataset to ensure it is configured and operating how you intend it to.
#' Sections can be toggled on and off in order to save compilation time.
#' Plots and tables are saved to a folder called "investigate" within the specified directory.
#'
#' @param data Output from format_logbooks() applied to raw logbook data.
#' @param dir Directory of file outputs (.Rmd, .html, and plots and tables). Default is working directory.
#' @param species_of_interest The name of the species of interest, as listed in the 'species' column of data. Can use a vector of species to duplicate plots for each species.
#' @param financial_year Set to TRUE to display as financial year
#' @param render Set to TRUE to render report. Set to FALSE to only produce Rmd file. If render = FALSE, you will need to manually load your data set into the first chunk of the Rmd file, ensuring it is called 'data' to feed into the subsequent chunks.
#' @param anonymous Set to TRUE to remove fisher/operator names.
#' @param maximum_fishing_day_count Default is 1. Filters data for trips of this maximum number of days, for all plots except those that explore the range of maximum number of fishing days.
#' @param interesting_years Optional. A vector of years to highlight in additional plots.
#' @param interesting_fishers Optional. A vector of fishers/operators to highlight in additional plots.
#' @param show_raw_catch Set to TRUE to show raw catch plots.
#' @param show_raw_effort Set to TRUE to show raw effort plots.
#' @param show_raw_cpue Set to TRUE to show raw CPUE plots.
#' @param show_fishing_methods Set to TRUE to show fishing method plots.
#' @param show_cocaught_species Set to TRUE to show plots. Default is TRUE.
#' @param show_seasonality Set to TRUE to show seasonality plots. Default is TRUE.
#' @param show_misreporting Set to TRUE to show misreporting plots. Default is TRUE.
#' @param show_unusual_catch Set to TRUE to show unusually high catch. Default is FALSE.
#' @param show_top_fishers Set to TRUE to show top fisher plots. Default is TRUE.
#' @param show_maps Set to TRUE to show maps. Default is TRUE. Not that if you're accessing from outside the QDAF network you will have to customise the directory for coastline shapefiles.
#' @param show_annual_maps Set to TRUE to show maps faceted by year. Default is FALSE, time intensive.
#' @param show_monthly_maps Set to TRUE to show maps faceted by month. Default is FALSE, time intensive.
#' @param show_method_maps Set to TRUE to show maps faceted by fishing method. Default is FALSE, time intensive.
#' @param show_regional_overview Set to TRUE to show regional overview. Default is TRUE.
#' @param metadata (Optional) A character string that will be entered into the Metadata tab
#' @param upset_n_trips Cut off value for tail end of upset plot. Do not plot species combinations who frequency is below this value. Default is 25. Depending on the dataset, a lower number might mean the plot takes a very long time, or renders with unreadable dimensions. A higher number might mean that no combinations are shown and the plot may break.
#' @param coast_directory (Optional) A file path for a shapefile containing a coastline to be used in maps.
#' @param filter_days_lower A minimum number of days to be included on the heat plot for identifying potential misreporting. Default is 10.
#'
#' @return If render==FALSE, returns investigate.Rmd at the specified directory. If render==TRUE, this Rmd is compiled and investigation.html and a folder called "investigate", containing all produced plots and tables, are generated in the specified directory.
#' @export
#'
#' @examples
#' \dontrun{
#' investigate(format_logbooks(logbooks),
#' species_of_interest = "Glitterfin snapper",
#' filter_days_lower = 1,
#' upset_n_trips = 1)
#' }
investigate <- function(data,
                        dir = getwd(),
                        species_of_interest,
                        financial_year = FALSE,
                        render = TRUE,
                        anonymous = FALSE,
                        maximum_fishing_day_count = 1,
                        interesting_years = NULL,
                        interesting_fishers = NULL,
                        show_raw_catch = TRUE,
                        show_raw_effort = TRUE,
                        show_raw_cpue = TRUE,
                        show_regional_overview = TRUE,
                        show_fishing_methods = TRUE,
                        show_cocaught_species = TRUE,
                        show_seasonality = TRUE,
                        show_misreporting = TRUE,
                        show_unusual_catch = FALSE,
                        show_top_fishers = TRUE,
                        show_maps = FALSE,
                        show_annual_maps = FALSE,
                        show_monthly_maps = FALSE,
                        show_method_maps = FALSE,
                        metadata = NULL,
                        upset_n_trips = 25,
                        coast_directory = NULL,
                        filter_days_lower = 10
) {

  if (nrow(data)>10000) {print("For large datasets, this can take a long time to run. You might want to test on a smaller dataset to ensure you are producing what you want.")}

  if (show_maps && missing(coast_directory)) {
    stop("You have set show_maps to TRUE but not specified a directory for a coast line shapefile in coast_directory. Please enter a directory for coast_directory or set show_maps to FALSE.")
  }

  if (!requireNamespace("DT", quietly = TRUE)) {
    stop("The 'DT' package is required but not installed. Please install it to use this function.")
  }

  if (!requireNamespace("ggupset", quietly = TRUE)) {
    stop("The 'ggupset' package is required but not installed. Please install it to use this function.")
  }

  # Check upset plot will produce something:
  interviews_with_species <- data |>
    dplyr::filter(species == species_of_interest) |>
    dplyr::pull(TripID) |>
    unique()

  upset_prep <- data |>
    dplyr::filter(TripID %in% interviews_with_species) |>
    dplyr::arrange(species) |> dplyr::group_by(TripID) |>
    dplyr::summarise(SpeciesName = list(species), .groups='drop') |>
    dplyr::group_by(SpeciesName) |>
    dplyr::mutate(n_trips = dplyr::n()) |>
    dplyr::ungroup() |>
    dplyr::filter(n_trips >= upset_n_trips)

  if (nrow(upset_prep)==0) {stop("Please decrease 'upset_n_trips' as the threshold is too high and no plots will be produced.")}
  rm(interviews_with_species,upset_prep)


  # ____________ ----
  # DATA AND FUNCTION SET UP ----
  max_days <- maximum_fishing_day_count

  # Account for fishing method


  if (financial_year) {data$year <- data$f.year}

  add_header <- function(string, rmd_file_name = "investigation.Rmd") {
    write(paste0("\n## ",string," {.tabset .tabset-fade .tabset-pills}"), rmd_file_name, append=TRUE)
  }

  add_plot <- function (string, name="", width=8, height=4,rmd_file_name = "investigation.Rmd") {
    new_chunk <- c("\n",
                   paste0("```{r ",name, ", echo=TRUE, fig.path='",dir,"/investigate/', fig.keep='all', fig.width=",width,", fig.height=",height,"}"),
                   string,
                   "```")
    new_chunk <- gsub(", species_of_interest = ''","",new_chunk) # if no species identified, remove

    write(new_chunk, rmd_file_name, append=TRUE)
  }

  add_text <- function(string, rmd_file_name = "investigation.Rmd") {
    write(paste0("\n",string), rmd_file_name, append=TRUE)
  }

  add_table <- function(string, caption = "", name="", rmd_file_name = "investigation.Rmd") {
    new_table <- c("\n",
                   paste0("```{r ",name,", echo=TRUE, results='asis'}"),
                   paste0("DT::datatable(",string,", caption = '",caption,"', rownames=FALSE)"),
                   "```")

    new_table <- gsub(", species_of_interest = ''","",new_table) # if no species identified, remove

    if (!dir.exists(paste0(dir,"/investigate/"))) {dir.create(paste0(dir,"/investigate/"))}

    utils::write.csv(eval(parse(text=string)), file=paste0(dir,"/investigate/",name,".csv"))
    write(new_table, rmd_file_name, append=TRUE)
  }

  # ____________ ----
  # DIMENSIONS ----
  # number of facets * default image dimensions * scale
  facet_stock_area_height <- ceiling(length(unique(data$stock_area))/2) * 4 * 0.75
  facet_stock_area_width  <- ifelse(length(unique(data$stock_area))>1,2,1) * 8 * 0.75

  facet_region_height <- ceiling(length(unique(data$region))/2) * 4 * 0.75
  facet_region_width  <- ifelse(length(unique(data$region))>1,2,1) * 8 * 0.75

  facet_coarse_region_height <- ceiling(length(unique(data$region_coarse))/2) * 4 * 0.75
  facet_coarse_region_width  <- ifelse(length(unique(data$region_coarse))>1,2,1) * 8 * 0.75

  facet_method_height <- ceiling(length(unique(data$method))/2) * 4 * 0.75
  facet_method_width  <- ifelse(length(unique(data$method))>1,2,1) * 8 * 0.75

  facet_methodtype_height <- ceiling(length(unique(data$method_type))/2) * 4 * 0.75
  facet_methodtype_width  <- ifelse(length(unique(data$method_type))>1,2,1) * 8 * 0.75

  facet_month_height <- 6 * 4 * 1
  facet_month_width  <- 2 * 8 * 1

  facet_region_height_heatmap <- ceiling(length(unique(data$region))/2) * 10 * 0.75
  facet_coarse_region_height_heatmap <- ceiling(length(unique(data$region_coarse))/2) * 10 * 0.75
  facet_stock_area_height_heatmap <- ceiling(length(unique(data$stock_area))/2) * 10 * 0.75


  # ____________ ----
  # YAML ----
  rmd_file_name <- "investigation.Rmd"

  yaml_header <- "---
title: 'Logbooks at a glance'
output:
  html_document:
---"

  write(yaml_header, rmd_file_name, append=FALSE)

  write("```{r, echo=TRUE, results='asis'}\nlibrary(SSAND)\n# data <- load('formatted_data.Rdata') # load data here if compiling from Rmd\n```\n", rmd_file_name, append=TRUE)

  write("# {.tabset}", rmd_file_name, append=TRUE)

  # ____________ ----
  # RAW CATCH SUMMARY ----
  if (show_raw_catch) {
    add_header("Catch summary")

    # Daily ----
    add_text("### Daily (boxplots) {.tabset .tabset-fade .tabset-pills}")
    add_text("#### Summary {.tabset .tabset-fade .tabset-pills}")
    add_plot(paste0("rawcatcheffortplot(data, y_var = 'catch', annual = TRUE, boxplot = TRUE, species_of_interest = '",species_of_interest,"', scales='free')"), name="raw_catch_box", height = 4, width = 8)

    add_plot(paste0("rawcatcheffortplot(data, y_var = 'catch', annual = FALSE, boxplot = TRUE, species_of_interest = '",species_of_interest,"')"), name="raw_catch_box_bulk", height = 4, width = 8)

    if ("stock_area" %in% names(data)) {
      add_text("#### By stock area {.tabset .tabset-fade .tabset-pills}")
      add_plot(paste0("rawcatcheffortplot(data, y_var = 'catch', annual = TRUE, boxplot = TRUE, species_of_interest = '",species_of_interest,"', facet_var = 'stock_area')"), name="raw_catch_box_region_stock_area", height = facet_stock_area_height, width = facet_stock_area_width)
    }

    add_text("#### By coarse region {.tabset .tabset-fade .tabset-pills}")
    add_plot(paste0("rawcatcheffortplot(data, y_var = 'catch', annual = TRUE, boxplot = TRUE, species_of_interest = '",species_of_interest,"', facet_var = 'region_coarse')"), name="raw_catch_box_region_coarse", height = facet_coarse_region_height, width = facet_coarse_region_width)


    add_text("#### By region {.tabset .tabset-fade .tabset-pills}")
    add_plot(paste0("rawcatcheffortplot(data, y_var = 'catch', annual = TRUE, boxplot = TRUE, species_of_interest = '",species_of_interest,"', facet_var = 'region')"), name="raw_catch_box_region", height = facet_region_height, width = facet_region_width)

    # Total ----
    add_text("### Total (barplots) {.tabset .tabset-fade .tabset-pills}")

    add_text("#### Summary {.tabset .tabset-fade .tabset-pills}")
    add_plot(paste0("rawcatcheffortplot(data, y_var = 'catch', species_of_interest = '",species_of_interest,"')"), name="raw_catch_bar", height = 4, width = 8)

    if ("stock_area" %in% names(data)) {
      add_text("#### By stock area {.tabset .tabset-fade .tabset-pills}")
      add_plot(paste0("rawcatcheffortplot(data, y_var = 'catch', species_of_interest = '",species_of_interest,"', fill_var = 'stock_area',show_proportion=TRUE, scales='free')"), name="raw_catch_bar_region_stock_area", height = 4, width = 8)
    }

    add_text("#### By coarse region {.tabset .tabset-fade .tabset-pills}")
    add_plot(paste0("rawcatcheffortplot(data, y_var = 'catch', species_of_interest = '",species_of_interest,"', fill_var = 'region_coarse',show_proportion=TRUE, scales='free')"), name="raw_catch_bar_region_coarse", height = 4, width = 8)


    add_text("#### By region {.tabset .tabset-fade .tabset-pills}")
    add_plot(paste0("rawcatcheffortplot(data, y_var = 'catch', species_of_interest = '",species_of_interest,"', fill_var = 'region',show_proportion=TRUE, scales='free')"), name="raw_catch_bar_region", height = 4, width = 8)

    add_plot(paste0("rawcatcheffortplot(data, y_var = 'catch', species_of_interest = '",species_of_interest,"', facet_var = 'region')"), name="raw_catch_bar_region_facet", height = facet_region_height, width = facet_region_width)

    add_text("#### By month {.tabset .tabset-fade .tabset-pills}")
    add_plot(paste0("rawcatcheffortplot(data, y_var = 'catch', species_of_interest = '",species_of_interest,"', fill_var = 'month',show_proportion=TRUE, scales='free')"), name="raw_catch_bar_month", height = 4, width = 8)

    add_plot(paste0("rawcatcheffortplot(data, y_var = 'catch', species_of_interest = '",species_of_interest,"', facet_var = 'month')"), name="raw_catch_bar_month_facet", height = facet_month_height, width = facet_month_width)
  }

  # ____________ ----
  # RAW EFFORT SUMMARY ----
  if (show_raw_effort) {
    add_header("Effort summary")

    # Bulk ----
    if ("maximum_fishing_day_count" %in% names(data)) {
      add_text("### Length of fishing trip {.tabset .tabset-fade .tabset-pills}")
      add_plot("ggplot2::ggplot(
      data |>
  dplyr::group_by(maximum_fishing_day_count) |>
  dplyr::summarise(n = dplyr::n(), .groups='drop')
  ) +
  ggplot2::theme_bw() +
  ggplot2::geom_point(ggplot2::aes(x=maximum_fishing_day_count, y=1)) +
  ggplot2::xlab('Length of fishing trip (days)') +
  ggplot2::ylab('') +
  ggplot2::theme(axis.ticks.y = ggplot2::element_blank(),
                 panel.grid.minor.y = ggplot2::element_blank(),
                 panel.grid.major.y = ggplot2::element_blank(),
                 axis.text.y = ggplot2::element_blank())", height=1, width=12, name="maximum_fishing_days")
      add_table("table(data$maximum_fishing_day_count) |> as.data.frame() |> `names<-`(c('Length of fishing trip (days)','Frequency'))", caption = "Table of maximum_fishing_day_count in dataset", name="tab_maximum_fishing_days")
    }

    # Total ----
    add_text("### Total (barplots) {.tabset .tabset-fade .tabset-pills}")

    add_text("#### Summary {.tabset .tabset-fade .tabset-pills}")
    add_plot(paste0("rawcatcheffortplot(data, y_var = 'effort', species_of_interest = '",species_of_interest,"')"), name="raw_effort_bar", height = 4, width = 8)

    if ("stock_area" %in% names(data)) {
      add_text("#### By stock area {.tabset .tabset-fade .tabset-pills}")
      add_plot(paste0("rawcatcheffortplot(data, y_var = 'effort', species_of_interest = '",species_of_interest,"', fill_var = 'stock_area',show_proportion=TRUE, scales='free')"), name="raw_effort_bar_region_stock_area", height = 4, width = 8)
    }

    add_text("#### By coarse region {.tabset .tabset-fade .tabset-pills}")
    add_plot(paste0("rawcatcheffortplot(data, y_var = 'effort', species_of_interest = '",species_of_interest,"', fill_var = 'region_coarse',show_proportion=TRUE, scales='free')"), name="raw_effort_bar_region_coarse", height = 4, width = 8)

    add_text("#### By region {.tabset .tabset-fade .tabset-pills}")
    add_plot(paste0("rawcatcheffortplot(data, y_var = 'effort', species_of_interest = '",species_of_interest,"', fill_var = 'region',show_proportion=TRUE, scales='free')"), name="raw_effort_bar_region", height = 4, width = 8)
    add_plot(paste0("rawcatcheffortplot(data, y_var = 'effort', species_of_interest = '",species_of_interest,"', facet_var = 'region')"), name="raw_effort_bar_region_facet", height = facet_region_height, width = facet_region_width)

    add_text("#### By month {.tabset .tabset-fade .tabset-pills}")
    add_plot(paste0("rawcatcheffortplot(data, y_var = 'effort', species_of_interest = '",species_of_interest,"', fill_var = 'month',show_proportion=TRUE, scales='free')"), name="raw_effort_bar_month", height = 4, width = 8)

    add_plot(paste0("rawcatcheffortplot(data, y_var = 'effort', species_of_interest = '",species_of_interest,"', facet_var = 'month')"), name="raw_effort_bar_month_facet", height = facet_month_height, width = facet_month_width)

  }

  # ____________ ----
  # RAW CATCH RATES ----
  if (show_raw_cpue) {
    add_header("Raw catch rates")

    add_text("### All {.tabset .tabset-fade .tabset-pills}")
    add_plot(paste0("rawcpueplot(data, species_of_interest = '",species_of_interest,"')"), name="raw_cpue")

    add_text("### By month {.tabset .tabset-fade .tabset-pills}")
    add_plot(paste0("rawcpueplot(data, species_of_interest = '",species_of_interest,"', colour_var = 'month',facet_var = 'month',scales='fixed')"), name="raw_cpue_monthly", height = facet_month_height, width = facet_month_width)

    if ("stock_area" %in% names(data)) {
      add_text("### By stock area {.tabset .tabset-fade .tabset-pills}")
      add_plot(paste0("rawcpueplot(data, species_of_interest = '",species_of_interest,"', colour_var = 'stock_area',facet_var = 'stock_area')"), name="raw_cpue_stock_area", height = facet_stock_area_width, width = facet_stock_area_width)
    }

    add_text("### By coarse region {.tabset .tabset-fade .tabset-pills}")
    add_plot(paste0("rawcpueplot(data, species_of_interest = '",species_of_interest,"', colour_var = 'region_coarse',facet_var = 'region_coarse')"), name="raw_cpue_region_coarse", height = facet_coarse_region_height, width = facet_coarse_region_width)

    add_text("### By region {.tabset .tabset-fade .tabset-pills}")
    add_plot(paste0("rawcpueplot(data, species_of_interest = '",species_of_interest,"', colour_var = 'region',facet_var = 'region')"), name="raw_cpue_region", height = facet_region_height, width = facet_region_width)

    add_text("### By fishing method type {.tabset .tabset-fade .tabset-pills}")
    add_plot(paste0("rawcpueplot(data, species_of_interest = '",species_of_interest,"', colour_var = 'method_type',facet_var = 'method_type') # see also facet_var='method'"), name="raw_cpue_fishingmethod", height = facet_methodtype_height, width = facet_methodtype_width)

    add_text("### By region and fishing method {.tabset .tabset-fade .tabset-pills}")
    add_plot(paste0("rawcpueplot(data, species_of_interest = '",species_of_interest,"', colour_var = 'method_type',facet_var = 'region') # see also facet_var='method'"), name="raw_cpue_fishingmethod_region", height = facet_region_height, width = facet_region_width)

    add_text("### By catch size {.tabset .tabset-fade .tabset-pills}")
    add_text("Daily catches are split into either above or below the median catch.")
    add_plot(paste0("rawcpueplot(data, species_of_interest = '",species_of_interest,"', colour_var = 'quantiles',facet_var = 'quantiles')"), name="raw_cpue_catchsize", height = 4, width = 8)
  }

  # ____________ ----
  # REGIONAL OVERVIEW ----
  if (show_regional_overview) {
    add_header("Regional overview")
    add_table("data |>\n   dplyr::group_by(year,region_coarse) |> \n   dplyr::summarise(`Number of operators` = dplyr::n_distinct(operator), \n   `Total catch (kg)` = round(sum(weight)), \n   `Total effort (days)`  = dplyr::n(), \n   .groups='drop')", name="regional_overview")

    add_plot("ggplot2::ggplot(data |>\n   dplyr::group_by(year,region_coarse) |> \n   dplyr::summarise(`Number of operators` = dplyr::n_distinct(operator), \n   `Total catch (kg)` = round(sum(weight)), \n   `Total effort (days)`  = dplyr::n(), \n   .groups='drop')\n  |> tidyr::pivot_longer(-c(year,region_coarse))) +
  ggplot2::geom_line(ggplot2::aes(x=year,y=value)) +
  ggplot2::geom_point(ggplot2::aes(x=year,y=value)) +
  ggplot2::facet_grid(cols=ggplot2::vars(region_coarse), rows=ggplot2::vars(name), scales='free') +
  ggplot2::theme_bw() +
  ggplot2::xlab('Year') +
  ggplot2::ylab('')", name="regional_overview_plot",height=12,width=12)
  }


  # ____________ ----
  # FISHING METHOD ----
  if (show_fishing_methods) {
    add_header("Fishing method")

    if (!missing(species_of_interest)) {
      add_text("### Table {.tabset .tabset-fade .tabset-pills}")
      add_table("data |> \n   dplyr::filter(species %in% species_of_interest) |> \n   dplyr::select(method) |> \n   table() |> \n   as.data.frame() |> \n   dplyr::arrange(desc((Freq))) |> \n   dplyr::mutate(Proportion = round(Freq/sum(Freq),4))", name="prop_fishing_method")
    }

    # All ----
    add_text("### All {.tabset .tabset-fade .tabset-pills}")

    add_plot(paste0("rawcatcheffortplot(data, y_var = 'catch', species_of_interest = '",species_of_interest,"', fill_var = 'method',show_proportion=TRUE, scales='free')"), name="fishing_method_catch", height = 4, width = 8)

    add_plot(paste0("rawcatcheffortplot(data, y_var = 'effort', species_of_interest = '",species_of_interest,"', fill_var = 'method',show_proportion=TRUE, scales='free')"), name="fishing_method_effort", height = 4, width = 8)

    # By month ----
    add_text("### By month {.tabset .tabset-fade .tabset-pills}")

    add_text("#### Catch {.tabset .tabset-fade .tabset-pills}")
    add_plot(paste0("rawcatcheffortplot(data, y_var = 'catch', species_of_interest = '",species_of_interest,"', fill_var = 'method', facet_var = 'month')"), name="fishing_method_catch_month", height = facet_month_height, width = facet_month_width)

    add_text("#### Proportion catch {.tabset .tabset-fade .tabset-pills}")
    add_plot(paste0("rawcatcheffortplot(data, y_var = 'catch', species_of_interest = '",species_of_interest,"', fill_var = 'method', facet_var = 'month', show_proportion = TRUE, proportion_only = TRUE)"), name="fishing_method_catch_month_prop", height = facet_month_height, width = facet_month_width)

    add_text("#### Effort {.tabset .tabset-fade .tabset-pills}")
    add_plot(paste0("rawcatcheffortplot(data, y_var = 'effort', species_of_interest = '",species_of_interest,"', fill_var = 'method', facet_var = 'month')"), name="fishing_method_effort_month", height = facet_month_height, width = facet_month_width)

    add_text("#### Proportion effort {.tabset .tabset-fade .tabset-pills}")
    add_plot(paste0("rawcatcheffortplot(data, y_var = 'effort', species_of_interest = '",species_of_interest,"', fill_var = 'method', facet_var = 'month', show_proportion = TRUE, proportion_only = TRUE)"), name="fishing_method_effort_month_prop", height = facet_month_height, width = facet_month_width)


    if ("stock_area" %in% names(data)) {
      # By stock area ----
      add_text("### By stock area {.tabset .tabset-fade .tabset-pills}")

      add_text("#### Catch {.tabset .tabset-fade .tabset-pills}")
      add_plot(paste0("rawcatcheffortplot(data, y_var = 'catch', species_of_interest = '",species_of_interest,"', fill_var = 'method', facet_var = 'stock_area')"), name="fishing_method_catch_stock_area", height = facet_stock_area_height, width = facet_stock_area_width)

      add_text("#### Proportion catch {.tabset .tabset-fade .tabset-pills}")
      add_plot(paste0("rawcatcheffortplot(data, y_var = 'catch', species_of_interest = '",species_of_interest,"', fill_var = 'method', facet_var = 'stock_area', show_proportion = TRUE, proportion_only = TRUE)"), name="fishing_method_catch_stock_area_prop", height = facet_stock_area_height, width = facet_stock_area_width)

      add_text("#### Effort {.tabset .tabset-fade .tabset-pills}")
      add_plot(paste0("rawcatcheffortplot(data, y_var = 'effort', species_of_interest = '",species_of_interest,"', fill_var = 'method', facet_var = 'stock_area')"), name="fishing_method_effort_stock_area", height = facet_stock_area_height, width = facet_stock_area_width)

      add_text("#### Proportion effort {.tabset .tabset-fade .tabset-pills}")
      add_plot(paste0("rawcatcheffortplot(data, y_var = 'effort', species_of_interest = '",species_of_interest,"', fill_var = 'method', facet_var = 'stock_area', show_proportion = TRUE, proportion_only = TRUE)"), name="fishing_method_effort_stock_area_prop", height = facet_stock_area_height, width = facet_stock_area_width)
    }

    # By coarse region ----
    add_text("### By coarse region {.tabset .tabset-fade .tabset-pills}")

    add_text("#### Catch {.tabset .tabset-fade .tabset-pills}")
    add_plot(paste0("rawcatcheffortplot(data, y_var = 'catch', species_of_interest = '",species_of_interest,"', fill_var = 'method', facet_var = 'region_coarse')"), name="fishing_method_catch_coarse_region", height = facet_coarse_region_height, width = facet_coarse_region_width)

    add_text("#### Proportion catch {.tabset .tabset-fade .tabset-pills}")
    add_plot(paste0("rawcatcheffortplot(data, y_var = 'catch', species_of_interest = '",species_of_interest,"', fill_var = 'method', facet_var = 'region_coarse', show_proportion = TRUE, proportion_only = TRUE)"), name="fishing_method_catch_coarse_region_prop", height = facet_coarse_region_height, width = facet_coarse_region_width)

    add_text("#### Effort {.tabset .tabset-fade .tabset-pills}")
    add_plot(paste0("rawcatcheffortplot(data, y_var = 'effort', species_of_interest = '",species_of_interest,"', fill_var = 'method', facet_var = 'region_coarse')"), name="fishing_method_effort_coarse_region", height = facet_coarse_region_height, width = facet_coarse_region_width)

    add_text("#### Proportion effort {.tabset .tabset-fade .tabset-pills}")
    add_plot(paste0("rawcatcheffortplot(data, y_var = 'effort', species_of_interest = '",species_of_interest,"', fill_var = 'method', facet_var = 'region_coarse', show_proportion = TRUE, proportion_only = TRUE)"), name="fishing_method_effort_coarse_region_prop", height = facet_coarse_region_height, width = facet_coarse_region_width)

    # By region ----
    add_text("### By region {.tabset .tabset-fade .tabset-pills}")

    add_text("#### Catch {.tabset .tabset-fade .tabset-pills}")
    add_plot(paste0("rawcatcheffortplot(data, y_var = 'catch', species_of_interest = '",species_of_interest,"', fill_var = 'method', facet_var = 'region')"), name="fishing_method_catch_region", height = facet_region_height, width = facet_region_width)

    add_text("#### Proportion catch {.tabset .tabset-fade .tabset-pills}")
    add_plot(paste0("rawcatcheffortplot(data, y_var = 'catch', species_of_interest = '",species_of_interest,"', fill_var = 'method', facet_var = 'region', show_proportion = TRUE, proportion_only = TRUE)"), name="fishing_method_catch_region_prop", height = facet_region_height, width = facet_region_width)

    add_text("#### Effort {.tabset .tabset-fade .tabset-pills}")
    add_plot(paste0("rawcatcheffortplot(data, y_var = 'effort', species_of_interest = '",species_of_interest,"', fill_var = 'method', facet_var = 'region')"), name="fishing_method_effort_region", height = facet_region_height, width = facet_region_width)

    add_text("#### Proportion effort {.tabset .tabset-fade .tabset-pills}")
    add_plot(paste0("rawcatcheffortplot(data, y_var = 'effort', species_of_interest = '",species_of_interest,"', fill_var = 'method', facet_var = 'region', show_proportion = TRUE, proportion_only = TRUE)"), name="fishing_method_effort_region_prop", height = facet_region_height, width = facet_region_width)

  }

  # ____________ ----
  # CO-CAUGHT SPECIES ----
  if (show_cocaught_species) {
    add_header("Co-caught species")

    if (missing(species_of_interest)) {add_text("Plots not included as no species of interest was specified.")}

    if (!missing(species_of_interest)) {

      # Species present ----
      add_text("### Trips where species of interest were caught {.tabset .tabset-fade .tabset-pills}")

      ## Table ----
      if (!missing(species_of_interest)) {
        add_text("#### Table {.tabset .tabset-fade .tabset-pills}") #  (species present)
        add_table(paste0("data |>
  dplyr::select(operator,date,species,weight) |>
  cbind(data |>
          dplyr::group_by(operator,date) |>
          dplyr::mutate(present = ifelse('",species_of_interest,"' %in% species, 1, 0)) |>
          dplyr::ungroup() |>
          dplyr::select(present)
        ) |>
  dplyr::filter(present == 1) |>
  dplyr::select(-present) |>
  dplyr::group_by(species) |>
  dplyr::summarise(weight = round(sum(weight)), .groups='drop') |>
  dplyr::arrange(desc(weight))"), name="cocaught_table_species_present")
      }


      ## All ----
      add_text("#### All {.tabset .tabset-fade .tabset-pills}")

      add_plot(paste0("rawcatcheffortplot(data, y_var = 'catch', species_of_interest = '",species_of_interest,"', fill_var = 'species',show_proportion=TRUE, scales='free')"), name="cocaught_species_catch", height = 4, width = 8)

      add_plot(paste0("rawcatcheffortplot(data, y_var = 'effort', species_of_interest = '",species_of_interest,"', fill_var = 'species',show_proportion=TRUE, scales='free')"), name="cocaught_species_effort", height = 4, width = 8)

      ## By month ----
      add_text("#### By month {.tabset .tabset-fade .tabset-pills}")

      add_text("##### Catch {.tabset .tabset-fade .tabset-pills}")
      add_plot(paste0("rawcatcheffortplot(data, y_var = 'catch', species_of_interest = '",species_of_interest,"', fill_var = 'species', facet_var = 'month')"), name="cocaught_species_catch_month", height = facet_month_height, width = facet_month_width)

      add_text("##### Proportion catch {.tabset .tabset-fade .tabset-pills}")
      add_plot(paste0("rawcatcheffortplot(data, y_var = 'catch', species_of_interest = '",species_of_interest,"', fill_var = 'species', facet_var = 'month', show_proportion = TRUE, proportion_only = TRUE)"), name="cocaught_species_catch_month_prop", height = facet_month_height, width = facet_month_width)

      add_text("##### Effort {.tabset .tabset-fade .tabset-pills}")
      add_plot(paste0("rawcatcheffortplot(data, y_var = 'effort', species_of_interest = '",species_of_interest,"', fill_var = 'species', facet_var = 'month')"), name="cocaught_species_effort_month", height = facet_month_height, width = facet_month_width)

      add_text("##### Proportion effort {.tabset .tabset-fade .tabset-pills}")
      add_plot(paste0("rawcatcheffortplot(data, y_var = 'effort', species_of_interest = '",species_of_interest,"', fill_var = 'species', facet_var = 'month', show_proportion = TRUE, proportion_only = TRUE)"), name="cocaught_species_effort_month_prop", height = facet_month_height, width = facet_month_width)


      if ("stock_area" %in% names(data)) {
        ## By stock area ----
        add_text("#### By stock area {.tabset .tabset-fade .tabset-pills}")

        add_text("##### Catch {.tabset .tabset-fade .tabset-pills}")
        add_plot(paste0("rawcatcheffortplot(data, y_var = 'catch', species_of_interest = '",species_of_interest,"', fill_var = 'species', facet_var = 'stock_area')"), name="cocaught_species_catch_stock_area", height = facet_stock_area_height, width = facet_stock_area_width)

        add_text("##### Proportion catch {.tabset .tabset-fade .tabset-pills}")
        add_plot(paste0("rawcatcheffortplot(data, y_var = 'catch', species_of_interest = '",species_of_interest,"', fill_var = 'species', facet_var = 'stock_area', show_proportion = TRUE, proportion_only = TRUE)"), name="cocaught_species_catch_stock_area_prop", height = facet_stock_area_height, width = facet_stock_area_width)

        add_text("##### Effort {.tabset .tabset-fade .tabset-pills}")
        add_plot(paste0("rawcatcheffortplot(data, y_var = 'effort', species_of_interest = '",species_of_interest,"', fill_var = 'species', facet_var = 'stock_area')"), name="cocaught_species_effort_stock_area", height = facet_stock_area_height, width = facet_stock_area_width)

        add_text("##### Proportion effort {.tabset .tabset-fade .tabset-pills}")
        add_plot(paste0("rawcatcheffortplot(data, y_var = 'effort', species_of_interest = '",species_of_interest,"', fill_var = 'species', facet_var = 'stock_area', show_proportion = TRUE, proportion_only = TRUE)"), name="cocaught_species_effort_stock_area_prop", height = facet_stock_area_height, width = facet_stock_area_width)
      }

      ## By coarse region ----
      add_text("#### By coarse region {.tabset .tabset-fade .tabset-pills}")

      add_text("##### Catch {.tabset .tabset-fade .tabset-pills}")
      add_plot(paste0("rawcatcheffortplot(data, y_var = 'catch', species_of_interest = '",species_of_interest,"', fill_var = 'species', facet_var = 'region_coarse')"), name="cocaught_species_catch_coarse_region", height = facet_coarse_region_height, width = facet_coarse_region_width)

      add_text("##### Proportion catch {.tabset .tabset-fade .tabset-pills}")
      add_plot(paste0("rawcatcheffortplot(data, y_var = 'catch', species_of_interest = '",species_of_interest,"', fill_var = 'species', facet_var = 'region_coarse', show_proportion = TRUE, proportion_only = TRUE)"), name="cocaught_species_catch_coarse_region_prop", height = facet_coarse_region_height, width = facet_coarse_region_width)

      add_text("##### Effort {.tabset .tabset-fade .tabset-pills}")
      add_plot(paste0("rawcatcheffortplot(data, y_var = 'effort', species_of_interest = '",species_of_interest,"', fill_var = 'species', facet_var = 'region_coarse')"), name="cocaught_species_effort_coarse_region", height = facet_coarse_region_height, width = facet_coarse_region_width)

      add_text("##### Proportion effort {.tabset .tabset-fade .tabset-pills}")
      add_plot(paste0("rawcatcheffortplot(data, y_var = 'effort', species_of_interest = '",species_of_interest,"', fill_var = 'species', facet_var = 'region_coarse', show_proportion = TRUE, proportion_only = TRUE)"), name="cocaught_species_effort_coarse_region_prop", height = facet_coarse_region_height, width = facet_coarse_region_width)

      ## By region ----
      add_text("#### By region {.tabset .tabset-fade .tabset-pills}")

      add_text("##### Catch {.tabset .tabset-fade .tabset-pills}")
      add_plot(paste0("rawcatcheffortplot(data, y_var = 'catch', species_of_interest = '",species_of_interest,"', fill_var = 'species', facet_var = 'region')"), name="cocaught_species_catch_region", height = facet_region_height, width = facet_region_width)

      add_text("##### Proportion catch {.tabset .tabset-fade .tabset-pills}")
      add_plot(paste0("rawcatcheffortplot(data, y_var = 'catch', species_of_interest = '",species_of_interest,"', fill_var = 'species', facet_var = 'region', show_proportion = TRUE, proportion_only = TRUE)"), name="cocaught_species_catch_region_prop", height = facet_region_height, width = facet_region_width)

      add_text("##### Effort {.tabset .tabset-fade .tabset-pills}")
      add_plot(paste0("rawcatcheffortplot(data, y_var = 'effort', species_of_interest = '",species_of_interest,"', fill_var = 'species', facet_var = 'region')"), name="cocaught_species_effort_region", height = facet_region_height, width = facet_region_width)

      add_text("##### Proportion effort {.tabset .tabset-fade .tabset-pills}")
      add_plot(paste0("rawcatcheffortplot(data, y_var = 'effort', species_of_interest = '",species_of_interest,"', fill_var = 'species', facet_var = 'region', show_proportion = TRUE, proportion_only = TRUE)"), name="cocaught_species_effort_region_prop", height = facet_region_height, width = facet_region_width)



      # add_text("#### Catch by year {.tabset .tabset-fade .tabset-pills}") # (species of interest present)
      # add_plot(paste0("cocaughtspeciesplot(data, n_species=15, species_of_interest = '",species_of_interest,"')"), name="cocaught_species_present", height = 10, width = 10)
      #
      # add_text("#### Catch by region {.tabset .tabset-fade .tabset-pills}") # , species of interest present
      # add_plot(paste0("cocaughtspeciesplot(data, n_species=15, species_of_interest = '",species_of_interest,"', show_proportion = FALSE, show_region = TRUE)"), name="cocaught_species_region_present", height = facet_region_height, width = facet_region_width)
      #
      # add_text("#### Proportion by region {.tabset .tabset-fade .tabset-pills}") # , species of interest present
      # add_plot(paste0("cocaughtspeciesplot(data, n_species=15, species_of_interest = '",species_of_interest,"', show_region = TRUE, proportion_only = TRUE)"), name="cocaught_species_prop_region_present", height = facet_region_height, width = facet_region_width)
      #
      # add_text("#### Catch by coarse region {.tabset .tabset-fade .tabset-pills}") # , species of interest present
      # add_plot(paste0("cocaughtspeciesplot(data, n_species=15, species_of_interest = '",species_of_interest,"', show_proportion = FALSE, show_region_coarse = TRUE)"), name="cocaught_species_coarse_region_present", height = facet_coarse_region_height, width = facet_coarse_region_width)
      #
      # add_text("#### Proportion by coarse region {.tabset .tabset-fade .tabset-pills}") # , species of interest present
      # add_plot(paste0("cocaughtspeciesplot(data, n_species=15, species_of_interest = '",species_of_interest,"', show_region_coarse = TRUE, proportion_only = TRUE)"), name="cocaught_species_prop_coarse_region_present", height = facet_coarse_region_height, width = facet_coarse_region_width)

      # All trips ----

      add_text("### All trips{.tabset .tabset-fade .tabset-pills}")

      ## Table ----
      add_text("#### Table {.tabset .tabset-fade .tabset-pills}")
      add_table("data |> \n  dplyr::group_by(species) |> \n  dplyr::summarise(weight = round(sum(weight)), .groups='drop') |> \n  dplyr::arrange(desc(weight))", name="cocaught_table_alltrips")

      ## All ----
      add_text("#### All {.tabset .tabset-fade .tabset-pills}")

      add_plot(paste0("rawcatcheffortplot(data, y_var = 'catch', fill_var = 'species',show_proportion=TRUE, scales='free')"), name="cocaught_species_catch_alltrips", height = 4, width = 8)

      add_plot(paste0("rawcatcheffortplot(data, y_var = 'effort', fill_var = 'species',show_proportion=TRUE, scales='free')"), name="cocaught_species_effort_alltrips", height = 4, width = 8)

      ## By month ----
      add_text("#### By month {.tabset .tabset-fade .tabset-pills}")

      add_text("##### Catch {.tabset .tabset-fade .tabset-pills}")
      add_plot(paste0("rawcatcheffortplot(data, y_var = 'catch', fill_var = 'species', facet_var = 'month')"), name="cocaught_species_catch_month_alltrips", height = facet_month_height, width = facet_month_width)

      add_text("##### Proportion catch {.tabset .tabset-fade .tabset-pills}")
      add_plot(paste0("rawcatcheffortplot(data, y_var = 'catch', fill_var = 'species', facet_var = 'month', show_proportion = TRUE, proportion_only = TRUE)"), name="cocaught_species_catch_month_prop_alltrips", height = facet_month_height, width = facet_month_width)

      add_text("##### Effort {.tabset .tabset-fade .tabset-pills}")
      add_plot(paste0("rawcatcheffortplot(data, y_var = 'effort', fill_var = 'species', facet_var = 'month')"), name="cocaught_species_effort_month_alltrips", height = facet_month_height, width = facet_month_width)

      add_text("##### Proportion effort {.tabset .tabset-fade .tabset-pills}")
      add_plot(paste0("rawcatcheffortplot(data, y_var = 'effort', fill_var = 'species', facet_var = 'month', show_proportion = TRUE, proportion_only = TRUE)"), name="cocaught_species_effort_month_prop_alltrips", height = facet_month_height, width = facet_month_width)


      if ("stock_area" %in% names(data)) {
        ## By stock area ----
        add_text("#### By stock area {.tabset .tabset-fade .tabset-pills}")

        add_text("##### Catch {.tabset .tabset-fade .tabset-pills}")
        add_plot(paste0("rawcatcheffortplot(data, y_var = 'catch', fill_var = 'species', facet_var = 'stock_area')"), name="cocaught_species_catch_stock_area_alltrips", height = facet_stock_area_height, width = facet_stock_area_width)

        add_text("##### Proportion catch {.tabset .tabset-fade .tabset-pills}")
        add_plot(paste0("rawcatcheffortplot(data, y_var = 'catch', fill_var = 'species', facet_var = 'stock_area', show_proportion = TRUE, proportion_only = TRUE)"), name="cocaught_species_catch_stock_area_prop_alltrips", height = facet_stock_area_height, width = facet_stock_area_width)

        add_text("##### Effort {.tabset .tabset-fade .tabset-pills}")
        add_plot(paste0("rawcatcheffortplot(data, y_var = 'effort', fill_var = 'species', facet_var = 'stock_area')"), name="cocaught_species_effort_stock_area_alltrips", height = facet_stock_area_height, width = facet_stock_area_width)

        add_text("##### Proportion effort {.tabset .tabset-fade .tabset-pills}")
        add_plot(paste0("rawcatcheffortplot(data, y_var = 'effort', fill_var = 'species', facet_var = 'stock_area', show_proportion = TRUE, proportion_only = TRUE)"), name="cocaught_species_effort_stock_area_prop_alltrips", height = facet_stock_area_height, width = facet_stock_area_width)
      }

      ## By coarse region ----
      add_text("#### By coarse region {.tabset .tabset-fade .tabset-pills}")

      add_text("##### Catch {.tabset .tabset-fade .tabset-pills}")
      add_plot(paste0("rawcatcheffortplot(data, y_var = 'catch', fill_var = 'species', facet_var = 'region_coarse')"), name="cocaught_species_catch_coarse_region_alltrips", height = facet_coarse_region_height, width = facet_coarse_region_width)

      add_text("##### Proportion catch {.tabset .tabset-fade .tabset-pills}")
      add_plot(paste0("rawcatcheffortplot(data, y_var = 'catch', fill_var = 'species', facet_var = 'region_coarse', show_proportion = TRUE, proportion_only = TRUE)"), name="cocaught_species_catch_coarse_region_prop_alltrips", height = facet_coarse_region_height, width = facet_coarse_region_width)

      add_text("##### Effort {.tabset .tabset-fade .tabset-pills}")
      add_plot(paste0("rawcatcheffortplot(data, y_var = 'effort', fill_var = 'species', facet_var = 'region_coarse')"), name="cocaught_species_effort_coarse_region_alltrips", height = facet_coarse_region_height, width = facet_coarse_region_width)

      add_text("##### Proportion effort {.tabset .tabset-fade .tabset-pills}")
      add_plot(paste0("rawcatcheffortplot(data, y_var = 'effort', fill_var = 'species', facet_var = 'region_coarse', show_proportion = TRUE, proportion_only = TRUE)"), name="cocaught_species_effort_coarse_region_prop_alltrips", height = facet_coarse_region_height, width = facet_coarse_region_width)

      ## By region ----
      add_text("#### By region {.tabset .tabset-fade .tabset-pills}")

      add_text("##### Catch {.tabset .tabset-fade .tabset-pills}")
      add_plot(paste0("rawcatcheffortplot(data, y_var = 'catch', fill_var = 'species', facet_var = 'region')"), name="cocaught_species_catch_region_alltrips", height = facet_region_height, width = facet_region_width)

      add_text("##### Proportion catch {.tabset .tabset-fade .tabset-pills}")
      add_plot(paste0("rawcatcheffortplot(data, y_var = 'catch', fill_var = 'species', facet_var = 'region', show_proportion = TRUE, proportion_only = TRUE)"), name="cocaught_species_catch_region_prop_alltrips", height = facet_region_height, width = facet_region_width)

      add_text("##### Effort {.tabset .tabset-fade .tabset-pills}")
      add_plot(paste0("rawcatcheffortplot(data, y_var = 'effort', fill_var = 'species', facet_var = 'region')"), name="cocaught_species_effort_region_alltrips", height = facet_region_height, width = facet_region_width)

      add_text("##### Proportion effort {.tabset .tabset-fade .tabset-pills}")
      add_plot(paste0("rawcatcheffortplot(data, y_var = 'effort', fill_var = 'species', facet_var = 'region', show_proportion = TRUE, proportion_only = TRUE)"), name="cocaught_species_effort_region_prop_alltrips", height = facet_region_height, width = facet_region_width)

      # Upset ----
      add_text("### Upset plot {.tabset .tabset-fade .tabset-pills}")
      add_text("Below is an upset plot. It groups data by unique ACN-fisherday and plots frequency of co-caught species. The plot shows the frequency of different combinations of species caught on the same interview ACN-day, ranked by the most common combination.")
      add_plot(paste0("upsetplot(data, source = 'CFISH', species_of_interest = '",species_of_interest,"', min_records=",upset_n_trips,")"), name="cocaught_species_upset", height = 12, width = 20)
    }
  }

  # ____________ ----
  # SEASONALITY ----
  if (show_seasonality) {
    add_header("Seasonality")
    # All ----
    add_text("### All {.tabset .tabset-fade .tabset-pills}")
    add_text("#### CPUE {.tabset .tabset-fade .tabset-pills}")
    add_plot(paste0("seasonalityplot(data, species_of_interest = '",species_of_interest,"', daily=FALSE, show_cpue=TRUE)"), name="seasonality_cpue")
    add_plot(paste0("seasonalityplot(data, species_of_interest = '",species_of_interest,"', daily=TRUE, show_cpue=TRUE)"), name="seasonality_cpue_daily")
    add_text("#### Catch {.tabset .tabset-fade .tabset-pills}")
    add_plot(paste0("seasonalityplot(data, species_of_interest = '",species_of_interest,"', daily=FALSE, show_catch=TRUE)"), name="seasonality_catch")
    add_plot(paste0("seasonalityplot(data, species_of_interest = '",species_of_interest,"', daily=TRUE, show_catch=TRUE)"), name="seasonality_catch_daily")
    add_text("#### Effort {.tabset .tabset-fade .tabset-pills}")
    add_plot(paste0("seasonalityplot(data, species_of_interest = '",species_of_interest,"', daily=FALSE, show_effort=TRUE)"), name="seasonality_effort")
    add_plot(paste0("seasonalityplot(data, species_of_interest = '",species_of_interest,"', daily=TRUE, show_effort=TRUE)"), name="seasonality_effort_daily")


    # By coarse region ----
    add_text("### By coarse region {.tabset .tabset-fade .tabset-pills}")
    add_text("#### CPUE {.tabset .tabset-fade .tabset-pills}")
    add_text("##### Summary{.tabset .tabset-fade .tabset-pills}")
    add_plot(paste0("seasonalityplot(data, species_of_interest = '",species_of_interest,"', daily=FALSE, show_cpue=TRUE, facet_var='region_coarse')"), name="seasonality_coarse_region_cpue", height = facet_coarse_region_height, width = facet_coarse_region_width)
    add_plot(paste0("seasonalityplot(data, species_of_interest = '",species_of_interest,"', daily=TRUE, show_cpue=TRUE, facet_var='region_coarse')"), name="seasonality_coarse_region_cpue_daily", height = facet_coarse_region_height, width = facet_coarse_region_width)
    add_text("##### By year {.tabset .tabset-fade .tabset-pills}")
    add_plot(paste0("seasonalityplot(data, species_of_interest = '",species_of_interest,"', daily=FALSE, show_cpue=TRUE, facet_var='region_coarse', fill_var='year')"), name="seasonality_coarse_region_year_cpue", height = facet_coarse_region_height, width = facet_coarse_region_width)
    add_plot(paste0("seasonalityplot(data, species_of_interest = '",species_of_interest,"', daily=TRUE, show_cpue=TRUE, facet_var='region_coarse', fill_var='year')"), name="seasonality_coarse_region_year_cpue_daily", height = facet_coarse_region_height, width = facet_coarse_region_width)

    add_text("#### Catch {.tabset .tabset-fade .tabset-pills}")
    add_text("##### Summary{.tabset .tabset-fade .tabset-pills}")
    add_plot(paste0("seasonalityplot(data, species_of_interest = '",species_of_interest,"', daily=FALSE, show_catch=TRUE, facet_var='region_coarse')"), name="seasonality_coarse_region_catch", height = facet_coarse_region_height, width = facet_coarse_region_width)
    add_plot(paste0("seasonalityplot(data, species_of_interest = '",species_of_interest,"', daily=TRUE, show_catch=TRUE, facet_var='region_coarse')"), name="seasonality_coarse_region_catch_daily", height = facet_coarse_region_height, width = facet_coarse_region_width)
    add_text("##### By year {.tabset .tabset-fade .tabset-pills}")
    add_plot(paste0("seasonalityplot(data, species_of_interest = '",species_of_interest,"', daily=FALSE, show_catch=TRUE, facet_var='region_coarse', fill_var='year')"), name="seasonality_coarse_region_year_catch", height = facet_coarse_region_height, width = facet_coarse_region_width)
    add_plot(paste0("seasonalityplot(data, species_of_interest = '",species_of_interest,"', daily=TRUE, show_catch=TRUE, facet_var='region_coarse', fill_var='year')"), name="seasonality_coarse_region_year_catch_daily", height = facet_coarse_region_height, width = facet_coarse_region_width)

    add_text("#### Effort {.tabset .tabset-fade .tabset-pills}")
    add_text("##### Summary{.tabset .tabset-fade .tabset-pills}")
    add_plot(paste0("seasonalityplot(data, species_of_interest = '",species_of_interest,"', daily=FALSE, show_effort=TRUE, facet_var='region_coarse')"), name="seasonality_coarse_region_effort", height = facet_coarse_region_height, width = facet_coarse_region_width)
    add_plot(paste0("seasonalityplot(data, species_of_interest = '",species_of_interest,"', daily=TRUE, show_effort=TRUE, facet_var='region_coarse')"), name="seasonality_coarse_region_effort_daily", height = facet_coarse_region_height, width = facet_coarse_region_width)
    add_text("##### By year {.tabset .tabset-fade .tabset-pills}")
    add_text("#### Effort {.tabset .tabset-fade .tabset-pills}")
    add_plot(paste0("seasonalityplot(data, species_of_interest = '",species_of_interest,"', daily=FALSE, show_effort=TRUE, facet_var='region_coarse', fill_var='year')"), name="seasonality_coarse_region_year_effort", height = facet_coarse_region_height, width = facet_coarse_region_width)
    add_plot(paste0("seasonalityplot(data, species_of_interest = '",species_of_interest,"', daily=TRUE, show_effort=TRUE, facet_var='region_coarse', fill_var='year')"), name="seasonality_coarse_region_year_effort_daily", height = facet_coarse_region_height, width = facet_coarse_region_width)

    # By region ----
    add_text("### By region {.tabset .tabset-fade .tabset-pills}")
    add_text("#### CPUE {.tabset .tabset-fade .tabset-pills}")
    add_text("##### Summary{.tabset .tabset-fade .tabset-pills}")
    add_plot(paste0("seasonalityplot(data, species_of_interest = '",species_of_interest,"', daily=FALSE, show_cpue=TRUE, facet_var='region')"), name="seasonality_region_cpue", height = facet_region_height, width = facet_region_width)
    add_plot(paste0("seasonalityplot(data, species_of_interest = '",species_of_interest,"', daily=TRUE, show_cpue=TRUE, facet_var='region')"), name="seasonality_region_cpue_daily", height = facet_region_height, width = facet_region_width)
    add_text("##### By year {.tabset .tabset-fade .tabset-pills}")
    add_plot(paste0("seasonalityplot(data, species_of_interest = '",species_of_interest,"', daily=FALSE, show_cpue=TRUE, facet_var='region', fill_var='year')"), name="seasonality_region_year_cpue", height = facet_region_height, width = facet_region_width)
    add_plot(paste0("seasonalityplot(data, species_of_interest = '",species_of_interest,"', daily=TRUE, show_cpue=TRUE, facet_var='region', fill_var='year')"), name="seasonality_region_year_cpue_daily", height = facet_region_height, width = facet_region_width)

    add_text("#### Catch {.tabset .tabset-fade .tabset-pills}")
    add_text("##### Summary{.tabset .tabset-fade .tabset-pills}")
    add_plot(paste0("seasonalityplot(data, species_of_interest = '",species_of_interest,"', daily=FALSE, show_catch=TRUE, facet_var='region')"), name="seasonality_region_catch", height = facet_region_height, width = facet_region_width)
    add_plot(paste0("seasonalityplot(data, species_of_interest = '",species_of_interest,"', daily=TRUE, show_catch=TRUE, facet_var='region')"), name="seasonality_region_catch_daily", height = facet_region_height, width = facet_region_width)
    add_text("##### By year {.tabset .tabset-fade .tabset-pills}")
    add_plot(paste0("seasonalityplot(data, species_of_interest = '",species_of_interest,"', daily=FALSE, show_catch=TRUE, facet_var='region', fill_var='year')"), name="seasonality_region_year_catch", height = facet_region_height, width = facet_region_width)
    add_plot(paste0("seasonalityplot(data, species_of_interest = '",species_of_interest,"', daily=TRUE, show_catch=TRUE, facet_var='region', fill_var='year')"), name="seasonality_region_year_catch_daily", height = facet_region_height, width = facet_region_width)

    add_text("#### Effort {.tabset .tabset-fade .tabset-pills}")
    add_text("##### Summary{.tabset .tabset-fade .tabset-pills}")
    add_plot(paste0("seasonalityplot(data, species_of_interest = '",species_of_interest,"', daily=FALSE, show_effort=TRUE, facet_var='region')"), name="seasonality_region_effort", height = facet_region_height, width = facet_region_width)
    add_plot(paste0("seasonalityplot(data, species_of_interest = '",species_of_interest,"', daily=TRUE, show_effort=TRUE, facet_var='region')"), name="seasonality_region_effort_daily", height = facet_region_height, width = facet_region_width)
    add_text("##### By year {.tabset .tabset-fade .tabset-pills}")
    add_text("#### Effort {.tabset .tabset-fade .tabset-pills}")
    add_plot(paste0("seasonalityplot(data, species_of_interest = '",species_of_interest,"', daily=FALSE, show_effort=TRUE, facet_var='region', fill_var='year')"), name="seasonality_region_year_effort", height = facet_region_height, width = facet_region_width)
    add_plot(paste0("seasonalityplot(data, species_of_interest = '",species_of_interest,"', daily=TRUE, show_effort=TRUE, facet_var='region', fill_var='year')"), name="seasonality_region_year_effort_daily", height = facet_region_height, width = facet_region_width)
  }

  # ____________ ----
  # MISREPORTING ----
  if (show_misreporting) {
    add_header("Misreporting")

    # Heat plots ----
    # add_text("### Heat plots {.tabset .tabset-fade .tabset-pills}")
    # add_text("Fishers are shown on the x-axis and ranked by total catch over time. For easier interpretability, this plot may have been split into multiple plots, grouping fishers by total catch. The first plot shows the most prominent fishers, the last plot shows the least prominent fishers.")

    ## All ----
    add_text("### Summary {.tabset .tabset-fade .tabset-pills}")
    add_text("Fishers are shown on the x-axis and ranked by total catch over time. For easier interpretability, this plot may have been split into multiple plots, grouping fishers by total catch. The first plot shows the most prominent fishers, the last plot shows the least prominent fishers.")
    add_text("#### Days fished {.tabset .tabset-fade .tabset-pills}")
    add_plot(paste0("heatplot(data, species_of_interest = '",species_of_interest,"', fill='days', filter_days_lower = ",filter_days_lower,")"), name="heatplot_days", height = 10, width = 12)
    add_text("#### Weight {.tabset .tabset-fade .tabset-pills}")
    add_plot(paste0("heatplot(data, species_of_interest = '",species_of_interest,"', fill='weight', filter_days_lower = ",filter_days_lower,")"), name="heatplot_weight", height = 10, width = 12)

    ## By stock area region ----
    if ("stock_area" %in% names(data)) {
      add_text("### By stock area region {.tabset .tabset-fade .tabset-pills}")
      add_text("Fishers are shown on the x-axis and ranked by total catch over time. For easier interpretability, this plot may have been split into multiple plots, grouping fishers by total catch. The first plot shows the most prominent fishers, the last plot shows the least prominent fishers.")
      add_text("#### Days fished {.tabset .tabset-fade .tabset-pills}")
      add_plot(paste0("heatplot(data, species_of_interest = '",species_of_interest,"', fill='days', filter_days_lower = ",filter_days_lower,", facet_var = 'stock_area')"), name="heatplot_stock_area_days", height = facet_stock_area_height_heatmap, width = 12)
      add_text("#### Weight {.tabset .tabset-fade .tabset-pills}")
      add_plot(paste0("heatplot(data, species_of_interest = '",species_of_interest,"', fill='weight', filter_days_lower = ",filter_days_lower,", facet_var = 'stock_area')"), name="heatplot_stock_area_weight", height = facet_stock_area_height_heatmap, width = 12)
    }

    ## By coarse region ----
    add_text("### By coarse region {.tabset .tabset-fade .tabset-pills}")
    add_text("Fishers are shown on the x-axis and ranked by total catch over time. For easier interpretability, this plot may have been split into multiple plots, grouping fishers by total catch. The first plot shows the most prominent fishers, the last plot shows the least prominent fishers.")
    add_text("#### Days fished {.tabset .tabset-fade .tabset-pills}")
    add_plot(paste0("heatplot(data, species_of_interest = '",species_of_interest,"', fill='days', filter_days_lower = ",filter_days_lower,", facet_var = 'region_coarse')"), name="heatplot_region_coarse_days", height = facet_coarse_region_height_heatmap, width = 12)
    add_text("#### Weight {.tabset .tabset-fade .tabset-pills}")
    add_plot(paste0("heatplot(data, species_of_interest = '",species_of_interest,"', fill='weight', filter_days_lower = ",filter_days_lower,", facet_var = 'region_coarse')"), name="heatplot_region_coarse_weight", height = facet_coarse_region_height_heatmap, width = 12)

    ## By region ----
    add_text("### By region {.tabset .tabset-fade .tabset-pills}")
    add_text("Fishers are shown on the x-axis and ranked by total catch over time. For easier interpretability, this plot may have been split into multiple plots, grouping fishers by total catch. The first plot shows the most prominent fishers, the last plot shows the least prominent fishers.")
    add_text("#### Days fished {.tabset .tabset-fade .tabset-pills}")
    add_plot(paste0("heatplot(data, species_of_interest = '",species_of_interest,"', fill='days', filter_days_lower = ",filter_days_lower,", facet_var = 'region')"), name="heatplot_region_days", height = facet_region_height_heatmap, width = 12)
    add_text("#### Weight {.tabset .tabset-fade .tabset-pills}")
    add_plot(paste0("heatplot(data, species_of_interest = '",species_of_interest,"', fill='weight', filter_days_lower = ",filter_days_lower,", facet_var = 'region')"), name="heatplot_region_weight", height = facet_region_height_heatmap, width = 12)

    ## High catch ----
    add_text("### For high catch {.tabset .tabset-fade .tabset-pills}")
    add_text("Fishers are shown on the x-axis and ranked by total catch over time. For easier interpretability, this plot may have been split into multiple plots, grouping fishers by total catch. The first plot shows the most prominent fishers, the last plot shows the least prominent fishers.")
    add_text("Focus on individual records where catch is in the top 75% quantile")
    add_text("#### Days fished {.tabset .tabset-fade .tabset-pills}")
    add_plot(paste0("filter_weight_lower <- data |>\n   dplyr::filter(species %in% species_of_interest) |>\n   dplyr::group_by(operator,year) |>\n   dplyr::summarise(days = dplyr::n_distinct(date), weight = sum(weight), .groups='drop') |> \n   dplyr::mutate(weight = round(weight, digits = 0)) |> \n   dplyr::select(weight) |>  \n    dplyr::pull() |> \n  quantile(0.75) \nheatplot(data, species_of_interest = '",species_of_interest,"', fill='days', filter_weight_lower = filter_weight_lower)"), name="heatplot_high_catch_days", height = 10, width = 12)
    add_text("#### Weight {.tabset .tabset-fade .tabset-pills}")
    add_plot(paste0("filter_weight_lower <- data |>\n   dplyr::filter(species %in% species_of_interest) |>\n   dplyr::group_by(operator,year) |>\n   dplyr::summarise(days = dplyr::n_distinct(date), weight = sum(weight), .groups='drop') |> \n   dplyr::mutate(weight = round(weight, digits = 0)) |> \n   dplyr::select(weight) |>  \n    dplyr::pull() |> \n  quantile(0.75) \nheatplot(data, species_of_interest = '",species_of_interest,"', fill='weight', filter_weight_lower = filter_weight_lower)"), name="heatplot_high_catch_weight", height = 10, width = 12)

    ## Low catch ----
    add_text("### For low catch {.tabset .tabset-fade .tabset-pills}")
    add_text("Fishers are shown on the x-axis and ranked by total catch over time. For easier interpretability, this plot may have been split into multiple plots, grouping fishers by total catch. The first plot shows the most prominent fishers, the last plot shows the least prominent fishers.")
    add_text("Focus on individual records where catch is in the bottom 25% quantile")
    add_text("#### Days fished {.tabset .tabset-fade .tabset-pills}")
    add_plot(paste0("filter_weight_upper <- data |>\n   dplyr::filter(species %in% species_of_interest) |>\n   dplyr::group_by(operator,year) |>\n   dplyr::summarise(days = dplyr::n_distinct(date), weight = sum(weight), .groups='drop') |> \n   dplyr::mutate(weight = round(weight, digits = 0)) |> \n   dplyr::select(weight) |> \n    dplyr::pull() |>\n   quantile(0.25) \n \nheatplot(data, species_of_interest = '",species_of_interest,"', fill='days', filter_weight_upper = filter_weight_upper)"), name="heatplot_low_catch_days", height = 10, width = 12)
    add_text("#### Weight {.tabset .tabset-fade .tabset-pills}")
    add_plot(paste0("filter_weight_upper <- data |>\n   dplyr::filter(species %in% species_of_interest) |>\n   dplyr::group_by(operator,year) |>\n   dplyr::summarise(days = dplyr::n_distinct(date), weight = sum(weight), .groups='drop') |> \n   dplyr::mutate(weight = round(weight, digits = 0)) |> \n   dplyr::select(weight) |> \n    dplyr::pull()  |>  \n    quantile(0.25) \nheatplot(data, species_of_interest = '",species_of_interest,"', fill='weight', filter_weight_upper = filter_weight_upper)"), name="heatplot_low_catch_weight", height = 10, width = 12)

    ## Top fishers ----
    add_text("### For top fishers {.tabset .tabset-fade .tabset-pills}")
    add_text("Fishers are shown on the x-axis and ranked by total catch over time. For easier interpretability, this plot may have been split into multiple plots, grouping fishers by total catch. The first plot shows the most prominent fishers, the last plot shows the least prominent fishers.")
    add_text("Focus on top 10 fishers who caught the most over time")
    add_text("#### Days fished {.tabset .tabset-fade .tabset-pills}")
    add_plot(paste0("heatplot(data, species_of_interest = '",species_of_interest,"', fill='days', interesting_fishers = head(rank_fishers(data, species_of_interest = '",species_of_interest,"', show_year= FALSE)$operator,10))"), name="heatplot_ranked_catch_days", height = 10, width = 12)
    add_text("#### Weight {.tabset .tabset-fade .tabset-pills}")
    add_plot(paste0("heatplot(data, species_of_interest = '",species_of_interest,"', fill='weight', interesting_fishers = head(rank_fishers(data, species_of_interest = '",species_of_interest,"', show_year= FALSE)$operator,10))"), name="heatplot_ranked_catch_weight", height = 10, width = 12)

    ## Unusually high catch ----

    if (show_unusual_catch) {
      if (nrow(unusualcatch(data,species_of_interest = species_of_interest)) > 0) {

        add_text("### Unusually high catch {.tabset .tabset-fade .tabset-pills}")
        add_text("Focus on fishers whose maximum daily catch was more than 10 times greater than their 90th percentile")
        if (!anonymous) {
          if (!missing(species_of_interest)) {
            add_text("#### Fisher history {.tabset .tabset-fade .tabset-pills}")
            add_plot(paste0("interesting_fishers <- show_unusual_catchunusualcatch(data,\nspecies_of_interest = '",species_of_interest,"') |>\n dplyr::select(operator) |>\ndplyr::pull() |>\nhead(20)\n\nunusualcatchplot(data)"), name="unusual_catch", height = 50)}
        }
        add_text("#### All {.tabset .tabset-fade .tabset-pills}")
        add_text("##### Days fished {.tabset .tabset-fade .tabset-pills}")
        add_plot(paste0("heatplot(data, species_of_interest = '",species_of_interest,"', fill='days', filter_weight_lower = 1, interesting_fishers = unusualcatch(data, species_of_interest = '",species_of_interest,"')$operator)"), name="heatplot_unusual_catch_days", height = 10, width = 12)
        add_text("##### Weight {.tabset .tabset-fade .tabset-pills}")
        add_plot(paste0("heatplot(data, species_of_interest = '",species_of_interest,"', fill='weight', filter_weight_lower = 1, interesting_fishers = unusualcatch(data, species_of_interest = '",species_of_interest,"')$operator)"), name="heatplot_unusual_catch_weight", height = 10, width = 12)

        add_text("#### By region {.tabset .tabset-fade .tabset-pills}")
        add_text("##### Days fished {.tabset .tabset-fade .tabset-pills}")
        add_plot(paste0("heatplot(data, species_of_interest = '",species_of_interest,"', fill='days', filter_weight_lower = 1, facet_var='region', interesting_fishers = unusualcatch(data, species_of_interest = '",species_of_interest,"')$operator)"), name="heatplot_unusual_catch_region_days", height = facet_region_height_heatmap, width = 12)
        add_text("##### Weight {.tabset .tabset-fade .tabset-pills}")
        add_plot(paste0("heatplot(data, species_of_interest = '",species_of_interest,"', fill='weight', filter_weight_lower = 1, facet_var='region', interesting_fishers = unusualcatch(data, species_of_interest = '",species_of_interest,"')$operator)"), name="heatplot_unusual_catch_region_weight", height = facet_region_height_heatmap, width = 12)

        add_text("#### By coarse region {.tabset .tabset-fade .tabset-pills}")
        add_text("##### Days fished {.tabset .tabset-fade .tabset-pills}")
        add_plot(paste0("heatplot(data, species_of_interest = '",species_of_interest,"', fill='days', filter_weight_lower = 1, facet_var='region_coarse', interesting_fishers = unusualcatch(data, species_of_interest = '",species_of_interest,"')$operator)"), name="heatplot_unusual_catch_coarse_region_days", height = facet_coarse_region_height_heatmap, width = 12)
        add_text("##### Weight {.tabset .tabset-fade .tabset-pills}")
        add_plot(paste0("heatplot(data, species_of_interest = '",species_of_interest,"', fill='weight', filter_weight_lower = 1, facet_var='region_coarse', interesting_fishers = unusualcatch(data, species_of_interest = '",species_of_interest,"')$operator)"), name="heatplot_unusual_catch_coarse_region_weight", height = facet_coarse_region_height_heatmap, width = 12)
      } else {
        add_text("### Unusually high catch {.tabset .tabset-fade .tabset-pills}")
        add_text("There were no fishers whose maximum daily catch was more than 10 times greater than their 90th percentile")
        write(paste0("```{r unsualcatch, eval=FALSE} \n unusualcatch(data,species_of_interest = '",species_of_interest,"')) \n```"), rmd_file_name, append=TRUE)
      }
    } else {
      add_text("### Unusually high catch {.tabset .tabset-fade .tabset-pills}")
      add_text("The unusualcatch function allows you to explore fishers whose maximum daily catch was more than X times greater than their Xth percentile. This function is not rendered as a default because it can be very computationally heavy. ")
      write("```{r unsualcatch, eval=FALSE} \n unusualcatch(data,species_of_interest = 'species name')) \n```", rmd_file_name, append=TRUE)
    }
  }

  # ____________ ----
  # TOP FISHERS ----
  if (show_top_fishers) {
    add_header("Top fishers")

    if (!missing(species_of_interest)) {
      add_text(paste0("There have been ", nrow(unique(data |> dplyr::filter(species %in% species_of_interest) |> dplyr::select(operator))), " unique fishers who have ever caught the species of interest."))

      if (!missing(interesting_years)) {
        add_text(paste0("There were ", nrow(unique(data |> dplyr::filter(year %in% interesting_years, species%in%species_of_interest) |> dplyr::select(operator))), " fishers in the fishery between ", min(interesting_years), " and ", max(interesting_years)))
      }

      add_text("### Tables {.tabset .tabset-fade .tabset-pills}")
      if (!anonymous) {
        add_table(paste0("rank_fishers(data |> dplyr::filter(species %in% '", species_of_interest,"'), show_year=FALSE)[,1:4]"), name="rank_fishers")
        add_table(paste0("rank_fishers(data |> dplyr::filter(species %in% '", species_of_interest,"'), show_year=TRUE)[,1:4]"), name="rank_fishers_year")
      } else {
        add_table(paste0("rank_fishers(data |> dplyr::filter(species %in% '", species_of_interest,"'), show_year=FALSE)[,2:4]"), name="rank_fishers")
        add_table(paste0("rank_fishers(data |> dplyr::filter(species %in% '", species_of_interest,"'), show_year=TRUE)[,c(1,3,4)]"), name="rank_fishers_year")
      }

      add_text("### Number of fishers {.tabset .tabset-fade .tabset-pills}")
      add_plot(paste0("nfishersplot(data, species_of_interest = '",species_of_interest,"')"), name="fishers_total_count")
      if (!missing(interesting_years)) {
        add_text(paste0("#### Fishers between ",min(interesting_years)," and ", max(interesting_years), ")"))
        add_plot(paste0("nfishersplot(data, species_of_interest = '",species_of_interest,"', interesting_years=",interesting_years,")"), name="fishers_total_count_specific_years")
      }

      if (!anonymous) {
        add_text("### Top fishers {.tabset .tabset-fade .tabset-pills}")
        add_plot(paste0("topfishersplot(data, species_of_interest = '",species_of_interest,"', show_daily=FALSE, n_top_fishers = 10)"), name="fishers_top")
        add_plot(paste0("topfishersplot(data, species_of_interest = '",species_of_interest,"', show_daily=TRUE,  n_top_fishers = 10)"), name="fishers_top_daily", height=30)

        if (!missing(interesting_years)) {
          add_text(paste0("#### Fishers between ",min(interesting_years)," and ", max(interesting_years), ")"))
          add_plot(paste0("topfishersplot(data, species_of_interest = '",species_of_interest,"', show_daily=FALSE, n_top_fishers = 10, interesting_years=",interesting_years,")"), name="fishers_top_specific_years")
          add_plot(paste0("topfishersplot(data, species_of_interest = '",species_of_interest,"', show_daily=TRUE,  n_top_fishers = 10, interesting_years=",interesting_years,")"), name="fishers_top_daily_specific_years")
        }
      }
    }
  }

  # ____________ ----
  # MAPS ----
  if (all(data$region=="No latitude data available")) {show_maps <- FALSE}

  if (show_maps) {
    add_header("Maps")

    if (!missing(species_of_interest)) {

      add_text(paste0("Showing only records for ", species_of_interest))

      ## Retained catch ----
      add_text("### Retained catch (kg)")

      add_text("#### Retained catch (kg)")
      add_plot(paste0("heat <- data |> dplyr::filter(species %in% species_of_interest) |> dplyr::group_by(grid) |> dplyr::summarise(value=sum(weight), .groups='drop')  \nmap(region = 'QLD', coast_directory = '",coast_directory,"', show_heat_map = TRUE, heat_map = heat, legend_position = 'right')"), name="heatmap_catch, warning=FALSE", height=16, width=12)
      add_text("#### Log of retained catch (kg)")
      add_plot(paste0("heat <- data |> dplyr::filter(species %in% species_of_interest) |> dplyr::group_by(grid) |> dplyr::summarise(value=sum(weight), .groups='drop') |> dplyr::mutate(value = log(value)) \nmap(region = 'QLD', coast_directory = '",coast_directory,"', show_heat_map = TRUE, heat_map = heat, legend_position = 'right')"), name="heatmap_catch_log, warning=FALSE", height=16, width=12)

      if (show_annual_maps) {add_plot(paste0("heat <- data |> dplyr::filter(species %in% species_of_interest) |> dplyr::group_by(grid,year) |> dplyr::summarise(value=sum(weight), .groups='drop') \nmap(region = 'QLD', coast_directory = '",coast_directory,"', show_heat_map = TRUE, heat_map = heat, legend_position = 'right') + ggplot2::facet_wrap(~as.factor(year))"), name="heatmap_catch_year, warning=FALSE")}
      if (show_monthly_maps) {add_plot(paste0("heat <- data |> dplyr::filter(species %in% species_of_interest) |> dplyr::group_by(grid,month) |> dplyr::summarise(value=sum(weight), .groups='drop') \nmap(region = 'QLD', coast_directory = '",coast_directory,"', show_heat_map = TRUE, heat_map = heat, legend_position = 'right') + ggplot2::facet_wrap(~month)"), name="heatmap_catch_month, warning=FALSE")}
      if (show_method_maps) {add_plot(paste0("heat <- data |> dplyr::filter(species %in% species_of_interest) |> dplyr::group_by(grid,method) |> dplyr::summarise(value=sum(weight), .groups='drop') \nmap(region = 'QLD', coast_directory = '",coast_directory,"', show_heat_map = TRUE, heat_map = heat, legend_position = 'right') + ggplot2::facet_wrap(~method)"), name="heatmap_catch_method, warning=FALSE")}

      ## Effort ----
      add_text("### Effort (days fished)")

      add_text("#### Effort (days fished)")
      add_plot(paste0("heat <- data |> dplyr::filter(species %in% species_of_interest) |> dplyr::group_by(grid) |> dplyr::summarise(value=dplyr::n(), .groups='drop') \nmap(region = 'QLD', coast_directory = '",coast_directory,"', show_heat_map = TRUE, heat_map = heat, legend_position = 'right')"), name="heatmap_effort, warning=FALSE", height=16, width=12)
      add_text("#### Log of effort (days fished)")
      add_plot(paste0("heat <- data |> dplyr::filter(species %in% species_of_interest) |> dplyr::group_by(grid) |> dplyr::summarise(value=dplyr::n(), .groups='drop') |> dplyr::mutate(value = log(value)) \nmap(region = 'QLD', coast_directory = '",coast_directory,"', show_heat_map = TRUE, heat_map = heat, legend_position = 'right')"), name="heatmap_effort_log, warning=FALSE", height=16, width=12)

      if (show_annual_maps) {add_plot(paste0("heat <- data |> dplyr::filter(species %in% species_of_interest) |> dplyr::group_by(grid,year) |> dplyr::summarise(value=dplyr::n(), .groups='drop') \nmap(region = 'QLD', coast_directory = '",coast_directory,"', show_heat_map = TRUE, heat_map = heat, legend_position = 'right') + ggplot2::facet_wrap(~as.factor(year))"), name="heatmap_effort_year, warning=FALSE")}
      if (show_monthly_maps) {add_plot(paste0("heat <- data |> dplyr::filter(species %in% species_of_interest) |> dplyr::group_by(grid,month) |> dplyr::summarise(value=dplyr::n(), .groups='drop') \nmap(region = 'QLD', coast_directory = '",coast_directory,"', show_heat_map = TRUE, heat_map = heat, legend_position = 'right') + ggplot2::facet_wrap(~month)"), name="heatmap_effort_month, warning=FALSE")}
      if (show_method_maps) {add_plot(paste0("heat <- data |> dplyr::filter(species %in% species_of_interest) |> dplyr::group_by(grid,method) |> dplyr::summarise(value=dplyr::n(), .groups='drop') \nmap(region = 'QLD', coast_directory = '",coast_directory,"', show_heat_map = TRUE, heat_map = heat, legend_position = 'right') + ggplot2::facet_wrap(~method)"), name="heatmap_effort_method, warning=FALSE")}

      ## Raw catch rates ----
      add_text("### Raw catch per unit effort (kg/days fished)")
      add_plot(paste0("heat <- data |> dplyr::filter(species %in% species_of_interest) |> dplyr::group_by(grid) |> dplyr::summarise(value=sum(weight)/dplyr::n(), .groups='drop') \nmap(region = 'QLD', coast_directory = '",coast_directory,"', show_heat_map = TRUE, heat_map = heat, legend_position = 'right')"), name="heatmap_cpue, warning=FALSE", height=16, width=12)
      add_text("#### Log of raw catch per unit effort (kg/days fished)")
      add_plot(paste0("heat <- data |> dplyr::filter(species %in% species_of_interest) |> dplyr::group_by(grid) |> dplyr::summarise(value=sum(weight)/dplyr::n(), .groups='drop') |> dplyr::mutate(value = log(value)) \nmap(region = 'QLD', coast_directory = '",coast_directory,"', show_heat_map = TRUE, heat_map = heat, legend_position = 'right')"), name="heatmap_cpue_log, warning=FALSE", height=16, width=12)

      if (show_annual_maps) {add_plot(paste0("heat <- data |> dplyr::filter(species %in% species_of_interest) |> dplyr::group_by(grid,year) |> dplyr::summarise(value=sum(weight)/dplyr::n(), .groups='drop') \nmap(region = 'QLD', coast_directory = '",coast_directory,"', show_heat_map = TRUE, heat_map = heat, legend_position = 'right') + ggplot2::facet_wrap(~as.factor(year))"), name="heatmap_cpue_year, warning=FALSE")}
      if (show_monthly_maps) {add_plot(paste0("heat <- data |> dplyr::filter(species %in% species_of_interest) |> dplyr::group_by(grid,month) |> dplyr::summarise(value=sum(weight)/dplyr::n(), .groups='drop') \nmap(region = 'QLD', coast_directory = '",coast_directory,"', show_heat_map = TRUE, heat_map = heat, legend_position = 'right') + ggplot2::facet_wrap(~month)"), name="heatmap_cpue_month, warning=FALSE")}
      if (show_method_maps) {add_plot(paste0("heat <- data |> dplyr::filter(species %in% species_of_interest) |> dplyr::group_by(grid,method) |> dplyr::summarise(value=sum(weight)/dplyr::n(), .groups='drop') \nmap(region = 'QLD', coast_directory = '",coast_directory,"', show_heat_map = TRUE, heat_map = heat, legend_position = 'right') + ggplot2::facet_wrap(~method)"), name="heatmap_cpue_method, warning=FALSE")}
    } else {

      add_text("Maps not filtered for a species of interest.")

      ## Retained catch ----
      add_text("### Retained catch (kg)")
      add_plot(paste0("heat <- data |> dplyr::group_by(grid) |> dplyr::summarise(value=sum(weight), .groups='drop')  \nmap(region = 'QLD', coast_directory = '",coast_directory,"', show_heat_map = TRUE, heat_map = heat, legend_position = 'right')"), name="heatmap_catch")
      add_plot(paste0("heat <- data |> dplyr::group_by(grid) |> dplyr::summarise(value=sum(weight), .groups='drop') |> dplyr::mutate(value = log(value)) \nmap(region = 'QLD', coast_directory = '",coast_directory,"', show_heat_map = TRUE, heat_map = heat, legend_position = 'right')"), name="heatmap_catch_log")

      if (show_annual_maps) {add_plot(paste0("heat <- data |> dplyr::group_by(grid,year) |> dplyr::summarise(value=sum(weight), .groups='drop') \nmap(region = 'QLD', coast_directory = '",coast_directory,"', show_heat_map = TRUE, heat_map = heat, legend_position = 'right') + ggplot2::facet_wrap(~as.factor(year))"), name="heatmap_catch_year")}
      if (show_monthly_maps) {add_plot(paste0("heat <- data |> dplyr::group_by(grid,month) |> dplyr::summarise(value=sum(weight), .groups='drop') \nmap(region = 'QLD', coast_directory = '",coast_directory,"', show_heat_map = TRUE, heat_map = heat, legend_position = 'right') + ggplot2::facet_wrap(~month)"), name="heatmap_catch_month")}
      if (show_method_maps) {add_plot(paste0("heat <- data |> dplyr::group_by(grid,method) |> dplyr::summarise(value=sum(weight), .groups='drop') \nmap(region = 'QLD', coast_directory = '",coast_directory,"', show_heat_map = TRUE, heat_map = heat, legend_position = 'right') + ggplot2::facet_wrap(~method)"), name="heatmap_catch_method")}

      ## Effort ----
      add_text("### Effort (days fished)")
      add_plot(paste0("heat <- data |> dplyr::group_by(grid) |> dplyr::summarise(value=dplyr::n(), .groups='drop') \nmap(region = 'QLD', coast_directory = '",coast_directory,"', show_heat_map = TRUE, heat_map = heat, legend_position = 'right')"), name="heatmap_effort")
      add_plot(paste0("heat <- data |> dplyr::group_by(grid) |> dplyr::summarise(value=dplyr::n(), .groups='drop') |> dplyr::mutate(value = log(value)) \nmap(region = 'QLD', coast_directory = '",coast_directory,"', show_heat_map = TRUE, heat_map = heat, legend_position = 'right')"), name="heatmap_effort_log")

      if (show_annual_maps) {add_plot(paste0("heat <- data |> dplyr::group_by(grid,year) |> dplyr::summarise(value=dplyr::n(), .groups='drop') \nmap(region = 'QLD', coast_directory = '",coast_directory,"', show_heat_map = TRUE, heat_map = heat, legend_position = 'right') + ggplot2::facet_wrap(~as.factor(year))"), name="heatmap_effort_year")}
      if (show_monthly_maps) {add_plot(paste0("heat <- data |> dplyr::group_by(grid,month) |> dplyr::summarise(value=dplyr::n(), .groups='drop') \nmap(region = 'QLD', coast_directory = '",coast_directory,"', show_heat_map = TRUE, heat_map = heat, legend_position = 'right') + ggplot2::facet_wrap(~month)"), name="heatmap_effort_month")}
      if (show_method_maps) {add_plot(paste0("heat <- data |> dplyr::group_by(grid,method) |> dplyr::summarise(value=dplyr::n(), .groups='drop') \nmap(region = 'QLD', coast_directory = '",coast_directory,"', show_heat_map = TRUE, heat_map = heat, legend_position = 'right') + ggplot2::facet_wrap(~method)"), name="heatmap_effort_method")}

      ## Raw catch rates ----
      add_text("### Raw catch per unit effort (kg/days fished)")
      add_plot(paste0("heat <- data |> dplyr::group_by(grid) |> dplyr::summarise(value=sum(weight)/dplyr::n(), .groups='drop') \nmap(region = 'QLD', coast_directory = '",coast_directory,"', show_heat_map = TRUE, heat_map = heat, legend_position = 'right')"), name="heatmap_cpue")
      add_text("#### Log of raw catch per unit effort (kg/days fished)")
      add_plot(paste0("heat <- data |> dplyr::group_by(grid) |> dplyr::summarise(value=sum(weight)/dplyr::n(), .groups='drop') |> dplyr::mutate(value = log(value)) \nmap(region = 'QLD', coast_directory = '",coast_directory,"', show_heat_map = TRUE, heat_map = heat, legend_position = 'right')", name="heatmap_cpue_log, warning=FALSE"), height=16, width=12)

      if (show_annual_maps) {add_plot(paste0("heat <- data |> dplyr::group_by(grid,year) |> dplyr::summarise(value=sum(weight)/dplyr::n(), .groups='drop') \nmap(region = 'QLD', coast_directory = '",coast_directory,"', show_heat_map = TRUE, heat_map = heat, legend_position = 'right') + ggplot2::facet_wrap(~as.factor(year))"), name="heatmap_cpue_year")}
      if (show_monthly_maps) {add_plot(paste0("heat <- data |> dplyr::group_by(grid,month) |> dplyr::summarise(value=sum(weight)/dplyr::n(), .groups='drop') \nmap(region = 'QLD', coast_directory = '",coast_directory,"', show_heat_map = TRUE, heat_map = heat, legend_position = 'right') + ggplot2::facet_wrap(~month)"), name="heatmap_cpue_month")}
      if (show_method_maps) {add_plot(paste0("heat <- data |> dplyr::group_by(grid,method) |> dplyr::summarise(value=sum(weight)/dplyr::n(), .groups='drop') \nmap(region = 'QLD', coast_directory = '",coast_directory,"', show_heat_map = TRUE, heat_map = heat, legend_position = 'right') + ggplot2::facet_wrap(~method)"), name="heatmap_cpue_method")}
    }
  }

  # ____________ ----
  # METADATA ----
  add_header("Metadata")

  if (missing(metadata)) {
    add_text("Please ammend the .Rmd file to include the metadata and other notes associated with this investigation. For example, the SQL code used and date retrieved.")
    write("```{r metadata, eval=FALSE} \n# Your code here \n```", rmd_file_name, append=TRUE)
  } else {
    write(paste0("```{r metadata, eval=FALSE} \n",metadata," \n```"), rmd_file_name, append=TRUE)
  }

  # ____________ ----
  # COMPILE ----
  if (render) {
    rmarkdown::render(input = rmd_file_name,
                      output_file = "investigation.html",
                      output_dir = dir,
                      clean = TRUE)

    system2("open",paste0(dir,"/investigation.html"))
  }


  if (!render) {
    print("investigation.Rmd has been produced in the directory specified (default is working directory). To compile investigation.Rmd, you will need to load your data in at line 8. Note that the dataset should be called 'data' for subsequent code chunks to work.")
  }
}


