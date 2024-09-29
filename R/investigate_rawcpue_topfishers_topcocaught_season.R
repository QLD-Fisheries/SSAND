# Copyright 2024 Fisheries Queensland

# This file is part of SSAND.
# SSAND is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
# SSAND is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
# You should have received a copy of the GNU General Public License along with SSAND. If not, see <https://www.gnu.org/licenses/>.

# max_days = 1
# species_of_interest = "Mackerel - spanish"
# n_fishers = 5
# n_trips = 5
#
#
# if ("maximum_fishing_day_count" %in% names(data)) {
#   data <- data |> dplyr::filter(maximum_fishing_day_count <= max_days)
# } else {
#   warning("The variable 'maximum_fishing_day_count' is not present in your dataset so not filtering out multi-day trips")
# }
#
#
# # Fishing trips where species of interest was caught
# trips_with_species <- data |>
#   dplyr::filter(species == species_of_interest) |>
#   dplyr::pull(TripID) |>
#   unique()
#
# # Top fisher combinations for each season and coarse region
# top_fishers <- data |>
#   dplyr::group_by(season, region_coarse, operator) |>
#   dplyr::summarise(totalweight = sum(weight), .groups='drop') |>
#   dplyr::group_by(region_coarse, season) |>
#   dplyr::slice_max(totalweight, n = n_fishers) |>
#   dplyr::ungroup() |>
#   dplyr::mutate(fisher_keep = 1)
#
# # Top co-caught species combinations for each season and coarse region
# species_combos <- data |>
#   dplyr::filter(TripID %in% trips_with_species) |>
#   dplyr::arrange(species) |>
#   dplyr::group_by(TripID, region_coarse, season) |>
#   dplyr::summarise(SpeciesCombo = list(species), .groups='drop')
#
# top_species_combos <- species_combos |>
#   dplyr::group_by(SpeciesCombo, region_coarse) |>
#   dplyr::mutate(n_trips = dplyr::n()) |>
#   dplyr::ungroup() |>
#   dplyr::filter(n_trips >= 1) |>
#   dplyr::select(-TripID) |>
#   dplyr::distinct() |>
#   dplyr::group_by(region_coarse, season) |>
#   dplyr::slice_max(n_trips, n = n_trips) |>
#   dplyr::ungroup() |>
#   dplyr::mutate(species_combo = 1)
#
#
# data <- data |>
#   dplyr::group_by()
#
#
# top_everything <- top_fishers |>
#   dplyr::left_join(top_species_combos, by = c("season", "region_coarse"))
#
#
#
# #
#
#
# # Determine catch rates
# data_combine <- data.frame()
# for (iregion_coarse in unique(data$region_coarse)) {
#   for (iseason in unique(data$season)) {
#     tmp_top_fishers <- top_fishers |> dplyr::filter(region_coarse == iregion_coarse, season == iseason)
#     tmp_top_species_combos <- top_species_combos |> dplyr::filter(region_coarse == iregion_coarse, season == iseason)
#
#     tmp_data <- data |>
#       dplyr::filter(region_coarse == iregion_coarse, season == iseason) |>
#       dplyr::filter(operator %in% tmp_top_fishers$operator) |>
#       dplyr::left_join(species_combos, by = "TripID") |>
#       dplyr::filter(SpeciesCombo %in% top_species_combos$SpeciesCombo) |>
#       dplyr::group_by(operator, year) |>
#       dplyr::summarise(rate = sum(weight)/dplyr::n(), .groups='drop') |>
#       dplyr::mutate(season = iseason, region_coarse = iregion_coarse)
#
#     data_combine <- data_combine |> rbind(tmp_data)
#   }
# }
#
#
#
#
#
