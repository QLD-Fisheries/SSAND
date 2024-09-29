# Copyright 2024 Fisheries Queensland

# This file is part of SSAND.
# SSAND is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
# SSAND is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
# You should have received a copy of the GNU General Public License along with SSAND. If not, see <https://www.gnu.org/licenses/>.

#' Generate a customised map
#'
#' Mapping components have been modularised to allow you to add components
#'
#' @param show_coast Set to TRUE to show coastline (logical, default is TRUE)
#' @param show_gbrmp_zoning Set to TRUE to show Great Barrier Reef Marine Park zoning (logical).
#' @param show_cities Set to TRUE to show cities (logical).
#' @param show_historic_zoning Set to TRUE to show historic zoning maps for trawl fishery (logical).
#' @param show_grids Set to TRUE to show all grids related to your region (logical).
#' @param show_NSW_grids Set to TRUE to show NSW grids (logical).
#' @param show_NSW_zones Set to TRUE to show NSW zones (logical). These are just latitude bands but labels are added.
#' @param show_specific_grids Set to TRUE to show specific grids on map (logical).
#' @param show_heat_map Set to TRUE to show heat map (logical).
#' @param show_monitoring_regions Set to TRUE to show Fishery Monitoring regions (logical).
#' @param show_gbrmp_boundary Set to TRUE to show Great Barrier Reef Marine Park boundary (logical).
#' @param show_reefs Set to TRUE to show reefs (logical).
#' @param region Define the broad region as a shortcut for specifying xlim and ylim. Options are "QLD" (Queensland), "EC" (east coast), "GOC" (Gulf of Carpentaria), "TS" (Torres Strait).
#' Default is "QLD". For other regions, specify using xlim and ylim.
#' @param coast_directory Directory to coast shapefile (character).
#' @param reefs_directory Directory to reef shapefile (character).
#' @param gbrmp_boundary_directory Directory to GBRMP boundary shapefile (character).
#' @param gbrmp_zoning_directory Directory to GBRMP zoning shapefile (character).
#' @param monitoring_region_directory Directory to monitoring region shapefile (character).
#' @param monitoring_region_object Name of monitoring region sf object (variable name) if already in the R environment, as an alternative to providing the directory of the shapefile.
#' @param historic_directory Directory to history shapefiles (vector, character).
#' @param gbrmp_zoning_exclude Vector of Zoning names to exclude on map (e.g. "Light Blue Zone"). Either include or exclude zones, not both.
#' @param gbrmp_zoning_include Vector of Zoning names to include on map (e.g. "Light Blue Zone"). Either include or exclude zones, not both.
#' @param cities A vector of cities to show on the map, from a pre-made list.
#' If none are specified, some are selected for you, based on region.
#' Options are: "Sunshine Coast", "Yeppoon", "Karumba", "Pormpuraaw",
#' "Aurukun","Mapoon","Thursday Island","Cooktown",
#' "Lockhart River","Port Douglas", "Airlie Beach","Stanage",
#' "Agnes Water","Hervey Bay","K\'gari","Gold Coast","Cairns",
#' "Townsville","Mackay","Rockhampton","Brisbane","Bundaberg"
#' @param specific_grids A vector of specific grids to be shown. For example, grids that were used in catch rate analysis.
#' @param heat_map A dataframe with variables grid (chr) and value (num). Heat map colours grid according to value.
#' @param monitoring_regions A vector of monitoring regions to show on map.
#' @param xlim Longitude limits of graph
#' @param ylim Latitude limits of graph (note they should be negative)
#' @param legend_title Title of legend (chr)
#' @param legend_position Position of the legend ("none", "left", "right", "bottom", "top", or two-element numeric vector for x and y position). Default is "top".
#' @param caption Caption of plot (chr)
#' @param colours A vector of colours (chr) where the first element is the colour of the landmass.
#' @param labels A vector of labels for areas distinguished by 'fill'. Not required is show_monitoring_regions==TRUE.
#' @param text_size Text size for grids or regions (num). Default is 2.5.
#' @param alpha Alpha on grids (only applies if not using a heat map). Default is 0.5.
#' @param grid_fill Colour to fill grids. Default is "white".
#' @param region_border Colour for border on regions (default is NA to exclude borders).
#' @param show_custom Set to TRUE to set up map with custom shapefile.
#' @param custom_directory Directory to custom shapefile (character). Can be a list of directories if plotting multiple custom layers.
#' @param custom_object Name of custom sf object (variable name) if already in the R environment. Can be a list of objects if plotting multiple custom layers.
#' @param custom_fill_variable Variable name in custom shapefile that will be used to guide fill aesthetic. If plotting multiple custom layers, must be a vector as long as the list of layers. Can enter NA for an entry.
#' @param custom_fill_colour_static Colour to fill, if fill is not dynamic. If plotting multiple custom layers, must be a vector as long as the list of layers.
#' @param custom_border_colour Colour of custom region border, if fill is not dynamic. If plotting multiple custom layers, must be a vector as long as the list of layers.
#' @param custom_border_thickness Thickness of border. If plotting multiple custom layers, must be a vector as long as the list of layers.
#' @param coast_colour Colour of coast fill (character.)
#' @param show_labels Set to FALSE to turn off fill variable labels on the map. Legend remains.
#' @param show_latbands Set to TRUE to show latitude bands on y-axis.
#' @param latbands If show_latbands==TRUE, enter latbands to plot on axis. By default, ylim is used.
#' @param latbands_level Set to "top" to place latbands as the top layer or "bottom" to place them below the coast layer
#' @param show_scale Display scale on map
#' @param scale_location Location of scale. Default is top left ("tl")
#' @param show_north Display north pointing arrow on map
#' @param north_location Location of north pointing arrow. Default is top left ("tl")
#' @param reef_fill Colour to fill reefs
#' @param show_reefs_in_legend Set to TRUE to show the fill colour of reefs in the legend.
#' @param reef_alpha Alpha of reefs
#' @param reef_border Colour of border of reefs
#' @param plot_title Enter a string to add a plot title.
#'
#' @return A customised map.
#' @export
#'
#' @examples
#' \dontrun{
#' # To generate a basic map with grids overlaid:
#' map(region = "EC", coast_directory = "shapefiles/coast.shp", show_grids=TRUE)
#'
#' # To change region:
#' map(region = "GOC", coast_directory = "shapefiles/coast.shp", show_grids=TRUE)
#'
#' # To add cities (could also specify cities using `cities` variable)
#' map(region = "EC", coast_directory = "shapefiles/coast.shp",
#'     show_grids=TRUE, show_cities = TRUE)
#'
#' # To generate a map with specific grids, such as those used in catch rate analysis:
#' map(region = "EC", coast_directory = "shapefiles/coast.shp",
#'     show_specific_grids = TRUE,
#'     specific_grids = c("I15", "K17", "H10", "L16", "Q18", "R12", "P18"))
#'
#' # To generate a heat map:
#' heat_map <- data.frame(grid = c("E4", "Z8", "A33", "G29", "C19",
#'                                 "A32", "Y37", "T12", "S9", "F40",
#'                                 "J8", "J12", "Z40", "M5", "N23"),
#'                        value= c(0.74, 0.14, 0.301, 1.172, 0.863,
#'                                 0.444, 1.044, 0.558, 0.31, 1.861,
#'                                 0.63, 0.682, 0.168, 0.82, 0.898))
#' map(region = "EC", coast_directory = "shapefiles/coast.shp",
#'     show_heat_map = TRUE, heat_map = heat_map,
#'     legend_position = "right")
#'
#' # To show monitoring regions:
#' map(region = "EC", coast_directory = "shapefiles/coast.shp",
#'     show_monitoring_regions = TRUE,
#'     monitoring_region_directory = "shapefiles/monitoring.shp",
#'     monitoring_regions = c("Lucinda Region", "Swains Region", "Bowen Region"),
#'     labels = c("Lucinda Region", "Swains Region", "Bowen Region"))
#'
#' # To show reefs and GBRMP boundary
#' map(region = "EC", show_gbrmp_boundary = TRUE, show_reefs = TRUE,
#'     gbrmp_boundary_directory = "shapefiles/gbrmp_boundary.shp",
#'     reefs_directory = "shapefiles/reefs.shp",
#'     colours = c("grey90", "white", "darkgreen"),
#'     labels = c("Great Barrier Reef Marine Park", "Reef"))
#'
#' # To show GBRMP zoning
#' map(region = "EC", coast_directory = "shapefiles/coast.shp",
#'     show_gbrmp_zoning = TRUE,
#'     gbrmp_zoning_directory = "shapefiles/gbrmp_zoning.shp",
#'     labels = c("Green Zone",
#'                "Yellow Zone",
#'                "Dark Blue Zone",
#'                "Olive Green Zone",
#'                "Orange Zone",
#'                "Pink Zone",
#'                "Light Blue Zone",
#'                "Commonwealth Island",
#'                "Commonwealth Island Zone"),
#'     colours = c("grey70","#41ba07", "#FFC000",
#'                         "darkblue","#26590d", "#ff770f",
#'                         "#ff0fd7", "#0fe3ff", "#340942", "#453d4d"))
#'
#' # To plot using a custom shapefile:
#' map(region="EC", coast_directory = "shapefiles/coast.shp",
#'     show_custom = TRUE,
#'     custom_directory = "custom.shp",
#'     custom_fill_variable = "BDM_ZONE",
#'     xlim = c(149,153),
#'     ylim = c(-24,-20),
#'     colours = rep(c("#F7C915","#3F73D3", "#F47E7A"),100),
#'     region_border = "black",
#'     legend_position = "none",
#'     coast_colour = "#dadf77",
#'     alpha = 1)
#'
#' # To plot a map with latbands:
#' map(coast_directory = "shapefiles/coast.shp",
#'   show_latbands = TRUE, latbands_level = "bottom")
#'
#' # To plot a map with QLD and NSW grids:
#' map(coast_directory = "shapefiles/coast.shp",
#'       ylim = c(-30, -20),
#'       show_grids = TRUE,
#'       show_NSW_grids = TRUE)
#'
#' }
map <- function(show_coast = TRUE,
                show_gbrmp_zoning = FALSE,
                show_cities = FALSE,
                show_historic_zoning = FALSE,
                show_grids = FALSE,
                show_NSW_grids = FALSE,
                show_NSW_zones = FALSE,
                show_specific_grids = FALSE,
                show_heat_map = FALSE,
                show_monitoring_regions = FALSE,
                show_gbrmp_boundary = FALSE,
                show_reefs = FALSE,
                show_custom = FALSE,
                # DIRECTORIES
                coast_directory = NULL,
                reefs_directory = NULL,
                gbrmp_boundary_directory = NULL,
                gbrmp_zoning_directory = NULL,
                monitoring_region_directory = NULL,
                monitoring_region_object = NULL,
                historic_directory = NULL,
                # DATA SPECIFICS
                gbrmp_zoning_exclude = NULL,
                gbrmp_zoning_include = NULL,
                cities = NULL,
                specific_grids = NULL,
                heat_map = NULL,
                monitoring_regions = NULL,
                # MAP SPECIFICS
                region = "QLD",
                xlim = NULL,
                ylim = NULL,
                legend_title = NULL,
                legend_position = c(0.75,0.75),
                caption = NULL,
                colours = NULL,
                labels = NULL,
                text_size = 2.5,
                alpha = 0.5,
                grid_fill = "white",
                region_border = NA,
                coast_colour = "grey90",
                show_labels = TRUE,
                show_latbands = FALSE,
                latbands = NULL,
                latbands_level = "bottom",
                show_scale =FALSE,
                scale_location ="tl",
                show_north = FALSE,
                north_location = "tl",
                reef_fill = "white",
                reef_alpha = 0.2,
                reef_border = "grey80",
                show_reefs_in_legend = FALSE,
                plot_title = NULL,
                # CUSTOM
                custom_directory = NULL,
                custom_object = NULL,
                custom_fill_variable = NULL,
                custom_border_colour = rep("grey30",99),
                custom_fill_colour_static = NA,
                custom_border_thickness = rep(0.5,99)
) {

  # Warnings
  if (show_specific_grids & missing(specific_grids)) {warning("Please specify which grids to show or change show_specific_grids to FALSE.")}
  if (show_monitoring_regions & missing(monitoring_regions)) {warning("Please specify which regions to show or change show_monitoring_regions to FALSE.")}

  # Generate some dummy data for heat map
  if (missing(heat_map) & show_heat_map) {
    set.seed(123)

    heat_map <- tibble::tibble(grid = c(LETTERS[1:26],rep(NA,14)),
                               number = c(1:40)) |>
      tidyr::complete(grid, number) |>
      dplyr::mutate(grid = paste0(grid,number)) |>
      dplyr::select(-number) |>
      dplyr::mutate(value = abs(rnorm(1080)))

    heat_map <- heat_map[sample(nrow(heat_map), 15), ]
    warning("Showing example heat_map. Please supply real heatmap data (grid and value) or turn of show_heat_map.")
  }

  if (!missing(gbrmp_zoning_exclude) & !missing(gbrmp_zoning_include)) {warning("You have specified zones to include AND exclude. Please only choose one of these options.")}

  # Define boundaries
  if (missing(xlim) & region=="QLD") {xlim = c(138,155.1)}
  if (missing(ylim) & region=="QLD") {ylim = c(-29,-9.5)}
  if (missing(xlim) & region=="TS")  {xlim = c(138,155.1)}
  if (missing(ylim) & region=="TS")  {ylim = c(-12,-9)}
  if (missing(xlim) & region=="GOC") {xlim = c(138,144.6)}
  if (missing(ylim) & region=="GOC") {ylim = c(-18.2,-9.76)}
  if (missing(xlim) & region=="EC")  {xlim = c(142,155.1)}
  if (missing(ylim) & region=="EC")  {ylim = c(-29,-9.5)}
  if (ylim[2]<ylim[1]) {tmp <- c(ylim[2],ylim[1]); ylim <- tmp; rm(tmp)}

  # Define map aesthetics
  if (missing(legend_title)) {legend_title = ggplot2::element_blank()}
  if (missing(colours) & show_historic_zoning) {colours = c("#88CCEE", "#99C945","#DDCC77","#AA4499")}
  if (missing(colours) & show_monitoring_regions) {colours = fq_palette("cols")}
  if (missing(colours) & !show_historic_zoning& !show_monitoring_regions) {colours = rep(fq_palette("alisecolours"),50)}

  # Load data
  coast <- sf::st_read(coast_directory,quiet=TRUE)
  if (show_reefs) {reefs <- sf::st_read(reefs_directory,quiet=TRUE)}
  if (show_gbrmp_boundary) {gbrmp_boundary <- sf::st_read(gbrmp_boundary_directory,quiet=TRUE)}
  if (show_monitoring_regions) {
    if (missing(monitoring_region_object)) {
      monitoring <- sf::st_read(monitoring_region_directory,quiet=TRUE)
    } else {
      monitoring <- monitoring_region_object
    }
  }
  if (show_gbrmp_zoning | show_historic_zoning) {
    gbrmp_zoning <- sf::st_read(gbrmp_zoning_directory,quiet=TRUE) |>
      sf::st_transform(crs = sf::st_crs(4326)) |>
      dplyr::mutate(ALT_ZONE = factor(ALT_ZONE,
                                      levels = c("Green Zone",
                                                 "Yellow Zone",
                                                 "Dark Blue Zone",
                                                 "Olive Green Zone",
                                                 "Orange Zone",
                                                 "Pink Zone",
                                                 "Light Blue Zone",
                                                 "Commonwealth Island",
                                                 "Commonwealth Island Zone")) )
  }

  # Set up cities
  if (missing(cities) & region == "QLD")  {cities = c("Cairns", "Townsville", "Mackay", "Rockhampton", "Brisbane", "Bundaberg")}
  if (missing(cities) & region == "EC")  {cities = c("Cairns", "Townsville", "Mackay", "Rockhampton", "Brisbane", "Bundaberg")}
  if (missing(cities) & region == "GOC") {cities = c("Karumba", "Pormpuraaw", "Aurukun", "Mapoon")}
  if (missing(cities) & region == "TS")  {cities = c("Thursday Island")}

  cities_map <- data.frame(label = "Sunshine Coast" , y= -26.6019, x = 153.2405)  |>
    rbind(data.frame(label = "Yeppoon"        , y= -23.0126, x = 150.7246)) |>
    rbind(data.frame(label = "Karumba"        , y= -17.4557, x = 140.9336)) |>
    rbind(data.frame(label = "Pormpuraaw"     , y= -14.8494, x = 141.7466)) |>
    rbind(data.frame(label = "Aurukun"        , y= -13.2829, x = 141.6807)) |>
    rbind(data.frame(label = "Mapoon"         , y= -11.9965, x = 142.1311)) |>
    rbind(data.frame(label = "Thursday Island", y= -10.5204, x = 142.2629)) |>
    rbind(data.frame(label = "Cooktown"       , y= -15.4962, x = 145.2293)) |>
    rbind(data.frame(label = "Lockhart River" , y= -12.6941, x = 143.3618)) |>
    rbind(data.frame(label = "Port Douglas"   , y= -16.4995, x = 145.4710)) |>
    rbind(data.frame(label = "Airlie Beach"   , y= -20.2933, x = 148.7229)) |>
    rbind(data.frame(label = "Stanage"        , y= -22.1416, x = 150.0516)) |>
    rbind(data.frame(label = "Agnes Water"    , y= -24.2155, x = 151.8978)) |>
    rbind(data.frame(label = "Hervey Bay"     , y= -25.2894, x = 152.7593)) |>
    rbind(data.frame(label = "K\'gari"        , y= -25.2434, x = 153.1404)) |>
    rbind(data.frame(label = "Gold Coast"     , y= -28.0248, x = 153.4037)) |>
    rbind(data.frame(label = "Cairns"         , y= -16.9200, x = 145.7700)) |>
    rbind(data.frame(label = "Townsville"     , y= -19.2500, x = 146.8100)) |>
    rbind(data.frame(label = "Mackay"         , y= -21.1500, x = 149.1700)) |>
    rbind(data.frame(label = "Rockhampton"    , y= -23.3800, x = 150.5100)) |>
    rbind(data.frame(label = "Brisbane"       , y= -27.4698, x = 153.0251)) |>
    rbind(data.frame(label = "Bundaberg"      , y= -24.8670, x = 152.3510)) |>
    rbind(data.frame(label = "Rainbow Beach"  , y= -25.9040, x = 153.0917)) |>
    dplyr::filter(label %in% cities)

  # Filter for any specified GBRMP zones
  if (show_gbrmp_zoning) {
    if (!missing(gbrmp_zoning_exclude)) {gbrmp_zoning <- gbrmp_zoning |> dplyr::filter(!ALT_ZONE == gbrmp_zoning_exclude)}
    if (!missing(gbrmp_zoning_include)) {gbrmp_zoning <- gbrmp_zoning |> dplyr::filter(ALT_ZONE == gbrmp_zoning_include)}
  }

  # Filter for any specified Fishery Monitoring regions
  if (!missing(monitoring_regions)) {
    monitoring <- monitoring |> dplyr::filter(Region %in% monitoring_regions)
  }

  # Prepare historic zoning data
  if (show_historic_zoning) {
    pre2003 <- sf::st_read(historic_directory[1], quiet=TRUE) |>
      dplyr::filter(!ZONE %in% c("General Use", "General Use A", "Unzoned")) # These are the closed zones

    pre1986 <- sf::st_read(historic_directory[2], quiet=TRUE) |>
      dplyr::filter(ZONE == "Habitat Protection" & OBJECTID > 476) # These are the closed zones
  }

  # Generate site and grid geoms
  if (region %in% c("QLD","EC","GOC")) {
    letters <- c(paste0("A", LETTERS[9:2]),LETTERS)

    sitelabels <- rep(c(rep(21:25,length(letters)),
                        rep(16:20,length(letters)),
                        rep(11:15,length(letters)),
                        rep(6:10,length(letters)),
                        rep(1:5,length(letters))),40)

    gridlabels <- paste0(rep(rep(letters,each=5),(-9+29)/0.1),
                         rep(40:1,each=length(letters)*5*5))

    site_cells <- sf::st_sf(SiteGrid=paste0(sitelabels,gridlabels),
                            site=sitelabels,
                            grid=gridlabels,
                            geom=sf::st_make_grid(cellsize=0.1,
                                                  offset=c(138,-29),
                                                  n=c((155-138)/0.1,(-9+29)/0.1),
                                                  crs=4326,what='polygons'),
                            stringsAsFactors = FALSE)
  }

  if (show_NSW_grids) {
    if (length(grid_fill)==1) {grid_fill <- c("darkred","darkblue")}
    NSWgridlabels <- paste0(rep(LETTERS[1:14],11), rep(10:0,each=14))
    grid_cellsNSW <- sf::st_sf(grid = NSWgridlabels,
                               geom = sf::st_make_grid(cellsize=1,
                                                       offset=c(141,-38),
                                                       n=c(14,11),
                                                       crs=4326,
                                                       what='polygons'),
                               stringsAsFactors = FALSE)
  }
  grid_cells <- site_cells |> dplyr::group_by(grid) |> dplyr::summarise() |> dplyr::ungroup()

  if (missing(latbands)) {latbands <- seq(ylim[1]-1,ylim[2]+1,1)}

  # Prepare heat_map
  if (show_heat_map) {
    grid_cells <- grid_cells |>
      dplyr::left_join(heat_map, by = "grid") |>
      dplyr::filter(!is.na(value))
  }

  # Filter grids for specic grids
  if (show_specific_grids) {
    grid_cells <- grid_cells |> dplyr::filter(grid %in% specific_grids)
  }

  if (show_NSW_zones) {show_latbands <- TRUE}

  # Generate plot
  p <- ggplot2::ggplot() +
    ggplot2::xlim(xlim) +
    ggplot2::xlab('')

  if (!show_latbands) {
    p <- p +
      ggplot2::ylim(ylim) +
      ggplot2::ylab('')
  } else {
    p <- p +
      ggplot2::scale_y_continuous(breaks = latbands, limits = c(latbands[1],latbands[length(latbands)]), name = '')
  }

  p <- p +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.title = legend_title,
                   legend.position = legend_position,
                   plot.caption = ggplot2::element_text(hjust = 0))

  if (!show_heat_map & !show_custom) {
    if (show_monitoring_regions) {
      p <- p +
        ggplot2::scale_fill_manual(values=colours[-1])
    } else {
      p <- p +
        ggplot2::scale_fill_manual(values=colours[-1], labels=labels)
    }
  } else {
    p <- p +
      ggplot2::scale_fill_viridis_c()
  }

  if (!missing(caption)) {
    p <- p +
      ggplot2::labs(caption = caption)
  }

  if (show_grids & !show_heat_map & !show_NSW_grids) {
    p <- p +
      ggplot2::geom_sf(data = grid_cells, fill = grid_fill, alpha = alpha) +
      ggplot2::geom_sf_text(data = grid_cells, ggplot2::aes(label = grid), size = text_size)
  }

  if (show_grids & !show_heat_map & show_NSW_grids) {
    p <- p +
      ggplot2::geom_sf(data = grid_cells, alpha = 0.2, fill=grid_fill[1]) +
      ggplot2::geom_sf_text(data = grid_cells, ggplot2::aes(label = grid), size = 2.5, colour=grid_fill[1]) +
      ggplot2::geom_sf(data = grid_cellsNSW, alpha = 0.2, fill=grid_fill[2]) +
      ggplot2::geom_sf_text(data = grid_cellsNSW, ggplot2::aes(label = grid), size = 2.5, colour=grid_fill[2])
  }

  if (!show_grids & !show_heat_map & show_NSW_grids) {
    p <- p +
      ggplot2::geom_sf(data = grid_cellsNSW, alpha = 0.2, fill=grid_fill[2]) +
      ggplot2::geom_sf_text(data = grid_cellsNSW, ggplot2::aes(label = grid), size = 2.5, colour=grid_fill[2])
  }

  if (show_heat_map) {
    p <- p +
      ggplot2::geom_sf(data = grid_cells, ggplot2::aes(fill = value), alpha = alpha) +
      ggplot2::geom_sf_text(data = grid_cells, ggplot2::aes(label = grid), size = text_size)
  }

  if (show_specific_grids & !show_heat_map) {
    p <- p +
      ggplot2::geom_sf(data = grid_cells, fill = grid_fill, alpha = alpha) +
      ggplot2::geom_sf_text(data = grid_cells, ggplot2::aes(label = grid), size = text_size)
  }

  if (show_latbands & latbands_level=="bottom") {
    if (missing(latbands)) {latbands <- seq(ylim[1]-1,ylim[2]+1,1)}
    for (i in latbands) {
      p <- p +
        ggplot2::geom_hline(yintercept = latbands[i], linetype="dashed")
    }
  }

  if (show_coast) {
    p <- p +
      ggplot2::geom_sf(data=coast, fill=coast_colour)
  }

  if (show_gbrmp_boundary) {
    p <- p +
      ggplot2::geom_sf(data=gbrmp_boundary, ggplot2::aes(fill="B"))
  }

  if (show_gbrmp_zoning) {
    p <- p +
      ggplot2::geom_sf(data=gbrmp_zoning, ggplot2::aes(fill=ALT_ZONE))
    warning("Work in progress")
  }

  if (show_historic_zoning) {
    p <- p +
      ggplot2::geom_sf(data=gbrmp_zoning, ggplot2::aes(fill="C")) +
      ggplot2::geom_sf(data=pre2003, ggplot2::aes(fill="D")) +
      ggplot2::geom_sf(data=pre1986, ggplot2::aes(fill="E"))
    warning("Work in progress")
  }

  if (show_monitoring_regions) {
    p <- p +
      ggplot2::geom_sf(data=monitoring, ggplot2::aes(fill=Region), colour = region_border)
  }

  if (show_reefs) {
    if (show_reefs_in_legend) {
      p <- p +
        ggplot2::geom_sf(data=reefs, ggplot2::aes(fill="F"), alpha = reef_alpha, colour = reef_border)
    } else {
      p <- p +
        ggplot2::geom_sf(data=reefs, fill=reef_fill, alpha = reef_alpha, colour = reef_border)
    }
  }

  if (show_cities) {
    p <- p +
      ggplot2::geom_text(data= cities_map,ggplot2::aes(x=x, y=y, label=label),size = 3,hjust = "right",nudge_x = -0.1) +
      ggplot2::geom_point(data=cities_map,ggplot2::aes(x=x, y=y))
  }

  if (show_custom) {
    if (!missing(custom_directory)) {
      for (i in 1:length(custom_directory)) {
        if (length(custom_directory)==1) {
          custom[[i]] <- sf::st_read(custom_directory,quiet=TRUE)
        } else {
          custom[[i]] <- sf::st_read(custom_directory[[i]],quiet=TRUE)
        }
      }
    }
    if (!missing(custom_object)) {custom <- custom_object}

    if (!missing(custom_object) & !missing(custom_directory)) {warning("You have specified both a custom directory and custom R shape object. Please only pick one, I'm not currently clever enough to handle both.")}

    for (i in 1:length(custom)) {
      custom_layer <- custom[[i]]
      custom_fill_variable_layer <- custom_fill_variable[i]


      if (!is.na(custom_fill_variable_layer)) {
        p <- p +
          ggplot2::geom_sf(data=custom_layer, ggplot2::aes(fill=.data[[custom_fill_variable_layer]]), alpha = alpha, colour = custom_border_colour[[i]], linewidth = custom_border_thickness[[i]] )

      } else {
        p <- p +
          ggplot2::geom_sf(data=custom_layer, fill=custom_fill_colour_static[[i]], alpha = alpha, colour = custom_border_colour[[i]], linewidth = custom_border_thickness[[i]])
      }

      if (show_labels) {
        p <- p +
          # ggplot2::geom_sf_text(data = custom, ggplot2::aes(label = .data[[custom_fill_variable]]), size = text_size)
          ggplot2::geom_sf_text(data = custom_layer, ggplot2::aes(label = .data[[custom_fill_variable_layer]]), size = text_size)
      }

    }
    p <- p +
      ggplot2::scale_fill_manual(name = legend_title, values=colours)  +
      ggplot2::theme(legend.position = legend_position)
  }

  if (show_latbands & latbands_level=="top") {
    for (i in latbands) {
      p <- p +
        ggplot2::geom_hline(yintercept = latbands[i], linetype="dashed")
    }
  }

  if (show_NSW_zones) {
    p <- p +
      ggplot2::geom_text(ggplot2::aes(x = 155, y = -37.5:-28.5, label = paste0("Zone", 10:1)))
  }

  if (show_scale) {
    p <- p +
      ggspatial::annotation_scale(
        location = scale_location,
        bar_cols = c("grey60", "white"))
  }

  if (show_north) {
    p <- p +
      ggspatial::annotation_north_arrow(
        location = north_location,
        which_north = "true",
        pad_x = ggplot2::unit(0.4, "in"),
        pad_y = ggplot2::unit(0.4, "in"))
  }

  if (!missing(plot_title)) {
    p <- p +
      ggplot2::ggtitle(plot_title)
  }

  return(p)
}




