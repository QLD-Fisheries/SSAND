# Copyright 2024 Fisheries Queensland

# This file is part of SSAND.
# SSAND is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
# SSAND is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
# You should have received a copy of the GNU General Public License along with SSAND. If not, see <https://www.gnu.org/licenses/>.

#' Data filtering and manipulation for logbook data
#' Creates columns for year, month, day, financial year and financial month,
#' Filters fishing method, CAAB species name, assessment time period and longitudinal and latitudinal extents.
#'
#' @param data A data frame containing the variables for commercial data.
#' @param logbook_type (Optional) A character string to automatically enter variable_names based on pre-defined logbook conventions. Options are "QLD", "NSW" and "NSW_monthly". Not needed if using custom variable_names input.
#' @param variable_names (Optional) Not needed if using logbook_type. A named vector of logbook field names, named as target variable names.
#' For example, variable_names = c(species = "CAABSpeciesCommonName", species_code = "CaabSpeciesID", grid = "GridDerived", site = "SiteDerived", operator = "AuthorityChainNumber")
#' Target variable names are used in subsequent data investigation code.
#' Target variable names include: species, species_code, grid, site, operator, date, weight, latitude, longitude, ncrew, nboats, method, boatmark, maximum_fishing_day_count
#' Additional target variable names can be included.
#' @param variable_names_append (Optional) If using logbook_type and more variables are to be added or overwritten. A named vector of logbook field names, named as target variable names.
#' For example, variable_names = c(species = "CAABSpeciesCommonName", species_code = "CaabSpeciesID", grid = "GridDerived", site = "SiteDerived", operator = "AuthorityChainNumber")
#' @param end_year A year value specifying the end year of catch reconstruction (numeric). Used to filter data set.
#' @param end_year_financial A logical value indicating whether the assessment is in financial year (TRUE) or calendar year (FALSE), used to filter data temporally.
#' @param species_code A vector of character strings specifying the CAAB code of the species being assessed or investigated (e.g. 37441911).
#' @param species_name A vector of character strings specifying the CAAB species name of the species being assessed or investigated.
#' @param spatial_extent A numeric vector specifying the extent of the assessment in the form: c(longitude_west, longitude_east, latitude_north, latitude_south).
#' @param assessment_area An optional character string to override the "spatial_extent", for commonly used assessment areas. Pre-defined options are:
#' - "EC" for Queensland east coast (c(142,155.1,-9.5,-29)),
#' - "GOC" for Gulf of Carpentaria (c(138,144.6,-9.76,-18.2)),
#' - "TS" for Torres Strait (c(138,155.1,-9,-12))
#' @param methods A vector of fishing method types to specify. Anything not included here will be categorised as 'Other'
#' @param other_name A name to call other methods e.g. 'Net + Other'. Default is 'Other'.
#' @param regions_dir Optional. A file path to a csv file containing a look-up table of grids to regions. File must contain columns "grid","region" and can optionally include "site" and "sitegrid"
#' @param regions_obj Optional. A dataframe containing a look-up table of grids to regions. Dataframe must contain columns "grid","region" and can optionally include "site" and "sitegrid"
#' @param nregions_coarse A numeric value for the number of coarse regions to be determined. Aggregates latitude bands into this number of groups. For example, if nregions_coarse=3, data will be grouped into "North", "Middle" and "South"
#' @param regions_coarse_labels Optional. A vector of labels for coarse regions. Coarse regions are aggregated from north to south.
#' @param remove_excess_columns Set to TRUE to keep only the columns that are specified within this function and arguments. Default is FALSE.
#'
#' @return Returns a dataframe containing a filtered, formatted catch dataset.
#' @export
#'
#' @examples
#' \dontrun{
#' QLD_commercial_filtered <- format_logbooks(data)
#' }
format_logbooks <- function(data,
                            logbook_type = "QLD",
                            variable_names = NULL,
                            variable_names_append = NULL,
                            end_year = NULL,
                            end_year_financial = FALSE,
                            species_code = NULL,
                            species_name = NULL,
                            spatial_extent = NULL,
                            assessment_area = NULL,
                            methods = NULL,
                            other_name = 'Other',
                            regions_dir = NULL,
                            regions_obj = NULL,
                            nregions_coarse = 2,
                            remove_excess_columns = FALSE,
                            regions_coarse_labels = NULL
) {

  # Error and warning messages
  if (missing(spatial_extent) && !missing(assessment_area) && assessment_area=="EC")  {spatial_extent <- c(142, 155.1, -9.50, -29  )}
  if (missing(spatial_extent) && !missing(assessment_area) && assessment_area=="GOC") {spatial_extent <- c(138, 144.6, -9.76, -18.2)}
  if (missing(spatial_extent) && !missing(assessment_area) && assessment_area=="TS")  {spatial_extent <- c(138, 155.1, -9.00, -12  )}

  if (!missing(regions_dir)) {regions_obj <- utils::read.csv(regions_dir)}

  # ______________________________________________________
  # Standardise variable names and select relevant columns ----
  # ______________________________________________________

  # Set up look up table of logbook variables and standardised variable names.
  # This will contain all the columns that will be selected for the final output.

  # Set up default variable names if using a logbook_type shortcut
  if (logbook_type == "QLD" && missing(variable_names)) {
    variable_names <- c(species = "CAABSpeciesCommonName",
                        species_code = "CaabSpeciesID",
                        grid = "GridDerived",
                        site = "SiteDerived",
                        operator = "AuthorityChainNumber",
                        date = "FishingStartDate",
                        weight = "RetainedWholeWeightDerived",
                        number = "RetainedNumber",
                        latitude = "LatitudeDerived",
                        longitude = "LongitudeDerived",
                        ncrew = "NumberOfCrew",
                        nboats = "NumberOfBoats",
                        method_type = "FishingMethodTypeDescription",
                        method = "FishingMethodDescription",
                        boatmark = "BoatMark",
                        maximum_fishing_day_count = "MaximumFishingDayCount",
                        effort_qty = "EffortHours")
  }

  if (logbook_type == "NSW" && missing(variable_names)) {
    variable_names <- c(species = "CommonName",
                        species_code = "CAABCode",
                        grid = "GridCode",
                        site = "SiteCode",
                        operator = "FishingBusinessOwnerID",
                        date = "EventDate",
                        weight = "WholeWtFishOnlineKg",
                        numbers = "CatchAmount_Numbers",
                        latitude = "LatitudeDecimal",
                        longitude = "LongitudeDecimal",
                        ncrew = "TotalAnglers",
                        crew_hours = "Angler Hours",
                        # nboats = "NumberOfBoats",
                        method = "FishingMethod",
                        effort_qty = "EffortQty")
  }

  if (logbook_type == "NSW_monthly") {
    data <- data |>
      # dplyr::mutate(year = floor(Period/100),
      dplyr::mutate(year = as.numeric(CalendarYear),
                    # month = as.character(Period %% 100),
                    month = as.numeric(MonthMM),
                    date = as.Date(paste(year,month,01,sep="-"), format = "%Y-%m-%d", origin = "1899-12-30"))

    if (missing(variable_names)) {
      variable_names <- c(
        date = "EventDate",
        latitude = "LatitudeDecimal",
        longitude = "LongitudeDecimal",
        species = "CommonName",
        acn = "AuthorisedFL_Encr",
        weight = "SumWholeWeight",
        species_code = "species_code",
        AreaID = "NewAreaID",
        species = "Species",
        acn = "FL_Encr",
        weight = "kg")
    }
  }

  if (missing(variable_names)) {
    variable_names <- c(species = "species",
                        species_code = "species_code",
                        grid = "grid",
                        site = "site",
                        operator = "operator",
                        date = "date",
                        weight = "weight",
                        latitude = "latitude",
                        longitude = "longitude",
                        method = "method")}


  if (!missing(variable_names_append)) {
    variables_to_keep <- setdiff(names(variable_names), names(variable_names_append))
    variable_names <- c(variable_names[variables_to_keep],variable_names_append)
  }


  # Create look-up table for desired variable names
  variable_names <- variable_names |> as.data.frame()
  variable_names$standardised_variable_names <- row.names(variable_names)
  row.names(variable_names) <- NULL
  colnames(variable_names) <- c("logbook_variable_names", "standardised_variable_names")

  # Select columns of interest then rename to standardised variable names
  present_variable_names <- intersect(variable_names$logbook_variable_names,names(data))

  data_standardised_names <- data |>
    dplyr::select(dplyr::all_of(present_variable_names)) |>
    dplyr::rename_with(~ variable_names$standardised_variable_names[match(., variable_names$logbook_variable_names)], .cols = dplyr::everything())

  # Select other columns, not renamed to something standarised
  data_non_standardised_names <- data |>
    dplyr::select(-dplyr::all_of(present_variable_names))

  if (remove_excess_columns) {
    data <- data_standardised_names
  } else {
    data <- data_standardised_names |> cbind(data_non_standardised_names)
  }


    # ______________________________________________________
    # Generate new variables ----
    # ______________________________________________________

    if ("method" %in% names(data) && !"method_type" %in% names(data)) {
      data <- data |> dplyr::mutate(method_type = method)
    }

    if (!"method" %in% names(data) && "method_type" %in% names(data)) {
      data <- data |> dplyr::mutate(method = method_type)
    }

    if (!"method" %in% names(data) && !"method_type" %in% names(data)) {
      data <- data |> dplyr::mutate(method = "No method information supplied", method_type = "No method type information supplied")
    }

    if(missing(methods)) methods <- unique(data$method)

    data <- data |>
      dplyr::mutate(date = as.Date(date, format = "%Y-%m-%d"),
                    year = as.numeric(format(date, format = "%Y")),
                    month = format(date, format = "%m"),
                    day = format(date, format = "%d"),
                    f.year = ifelse(lubridate::month(date)>6, year+1, year),
                    f.month = (lubridate::month(date)+5) %% 12 + 1,
                    latband = floor(latitude)*-1,
                    latband = ifelse(latband==0, NA, latband),
                    method = ifelse(method %in% methods, method, other_name))


    if (!missing(regions_obj)) {
      data <- data |>
        dplyr::left_join(regions_obj, by = intersect(names(regions_obj),names(data))) # specify "by" to avoid it printing to console
    }

    data <- data |>
      dplyr::mutate(season = dplyr::case_when(
        month %in% c("12","01","02") ~ "Summer",
        month %in% c("03","04","05") ~ "Autumn",
        month %in% c("06","07","08") ~ "Winter",
        month %in% c("09","10","11") ~ "Spring",
      )) |>
      dplyr::mutate(season= factor(season, levels = c("Summer","Autumn","Winter","Spring")))

    data <- data |>
      dplyr::mutate(TripID = paste0(operator,date))


    if (logbook_type == "QLD") {
      data <- data |>
        dplyr::mutate(stock_area = dplyr::case_when(
          latitude <= -9.50 & latitude>-29.0 & longitude>=142 & longitude < 156.0 ~ "East coast",
          latitude <= -9.76 & latitude>-18.2 & longitude>=137 & longitude < 144.6 ~ "Gulf of Carpentaria",
          latitude > -9.50  & longitude>=138 & longitude < 155.1 ~ "Torres Strait",
          .default = "Other"
        ))
    }


    # ______________________________________________________
    # Tidy data ----
    # ______________________________________________________

    # Ensure grid and site are uniformly capitalised
    if ("grid" %in% names(data)) {data <- data |> dplyr::mutate(grid = toupper(grid))}
    if ("site" %in% names(data)) {data <- data |> dplyr::mutate(site = toupper(site))}

    if (logbook_type == "NSW_monthly") {
      data <- data |> dplyr::mutate(species_code = species_code + 37000000)
    }

    # ______________________________________________________
    # Filter data ----
    # ______________________________________________________

    # Spatial extent
    if(!missing(spatial_extent)){
      if (!is.list(spatial_extent)) {
        data <- data |>
          dplyr::filter(longitude >= spatial_extent[1], longitude <= spatial_extent[2],
                        latitude >= spatial_extent[4], latitude <= spatial_extent[3])
      } else {
        spatially_filtered_data <- data.frame()
        for (i in 1:length(spatial_extent)) {
          spatially_filtered_data_tmp <- data |>
            dplyr::filter(longitude >= spatial_extent[[i]][1], longitude <= spatial_extent[[i]][2],
                          latitude >= spatial_extent[[i]][4], latitude <= spatial_extent[[i]][3])

          spatially_filtered_data <- rbind(spatially_filtered_data,spatially_filtered_data_tmp)
        }
        data <- spatially_filtered_data
      }
    }

    # End year
    if (!missing(end_year)) {
      if(end_year_financial){
        data <- data |> dplyr::filter(f.year <= end_year)
      } else {
        data <- data |> dplyr::filter(year <= end_year)
      }
    }

    # Species
    if(!missing(species_name)){species_name_var <- species_name; data <- data |> dplyr::filter(species %in% species_name_var)}
    if(!missing(species_code)){species_code_var <- species_code; data <- data |> dplyr::filter(species_code %in% species_code_var)}

    # Set up coarse regions (unless all lat or long are NA)
    if (all(is.na(data$latitude))) {
      data <- data |> dplyr::mutate(stock_area = "No latitude data available",
                                    region_coarse = "No latitude data available",
                                    region = "No latitude data available")
    } else {
      if (missing(regions_coarse_labels)) {
        if (nregions_coarse==2) {regions_coarse_labels = c("North","South")}
        if (nregions_coarse==3) {regions_coarse_labels = c("North","Middle","South")}
        if (nregions_coarse==4) {regions_coarse_labels = c("North","Middle north", "Middle south","South")}
        if (nregions_coarse>4) {regions_coarse_labels = paste0("Region ",1:nregions_coarse)}
      }
      data <- data |>
        dplyr::mutate(region_coarse = cut(data$latband, breaks=nregions_coarse, labels=regions_coarse_labels))
    }

    if (!"region" %in% names(data) && "region_coarse" %in% names(data) ) {
      data <- data |> dplyr::mutate(region = region_coarse)
    }


  return(data)
}
