# Copyright 2024 Fisheries Queensland

# This file is part of SSAND.
# SSAND is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
# SSAND is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
# You should have received a copy of the GNU General Public License along with SSAND. If not, see <https://www.gnu.org/licenses/>.

#' Prepare Stock Synthesis data for webcatchplot()
#'
#' This prep function works on the assumption that all harvest to be plotted on the web summary is contained in a model fleet.
#'
#' @param ss_mle A list of outputs from r4ss::SS_output() with one element per scenario. Will automatically reformat as a list if a single r4ss::SS_output() output (i.e. one scenario) is entered.
#' @param scenario Which scenario to plot (numeric). Default is 1.
#' @param commercial_fleets A vector of fleet numbers in the ss_mle object that contribute to total commercial catch (numeric).
#' @param commercial_data A vector of years to define when commercial data exist. Data are entered in pairs.
#' For example commercial_data = c(1942,1972,1988,2023) implies that data exists from 1942 to 1972, then 1988 to 2023.
#' @param recreational_fleets A vector of fleet numbers in the ss_mle object that contribute to total commercial catch (numeric).
#' @param recreational_data A vector of years for which recreational data exist.
#'
#' @return A data frame with variables called year (num), sector (chr), value (num) and method (chr)
#' @export
#'
#' @examples
#' data <- webcatchplot_prep_SS(ss_mle)
#' webcatchplot(data)
webcatchplot_prep_SS <- function(ss_mle,
                                 scenario = 1,
                                 commercial_fleets = c(1,2,3),
                                 commercial_data = c(1970,2003,2008,2023),
                                 recreational_fleets = c(4,5,6),
                                 recreational_data = c(1995,2001,2005,2011)) {
  if (check_scenarios(ss_mle,"SS","MLE")=="single scenario"){ss_mle <- list(ss_mle); warning("Assuming you are entering a single scenario, not a list of scenarios. Wrap ss_mle input inside a list() to avoid this warning.")}

  commercial_data_years <- c()
  for (i in 1:(length(commercial_data)/2)) {
    add <- commercial_data[i*2-1]:commercial_data[i*2]
    commercial_data_years <- c(commercial_data_years, add)
  }

  commercial_catch <- ss_mle[[scenario]]$catch |>
    dplyr::filter(Fleet %in% commercial_fleets) |>
    dplyr::group_by(Yr) |>
    dplyr::summarise(value = sum(Obs), .groups='drop') |>
    dplyr::rename(year = Yr) |>
    dplyr::mutate(sector = "Commercial",
                  method = ifelse(year %in% commercial_data_years, "Commercial data", "Commercial modelled"))

  commercial_plot_adjust1 <- commercial_catch |>
    dplyr::filter(method=="Commercial modelled") |>
    dplyr::mutate(method="Commercial plot adjust") |>
    dplyr::mutate(value=NA)

  commercial_plot_adjust2 <- commercial_catch |>
    dplyr::filter(year %in% commercial_data[2:(length(commercial_data)-1)]) |>
    dplyr::mutate(method="Commercial modelled")


  commercial_catch <- commercial_catch |>
    rbind(commercial_plot_adjust1, commercial_plot_adjust2)

  recreational_catch <- ss_mle[[scenario]]$catch |>
    dplyr::filter(Fleet %in% recreational_fleets) |>
    dplyr::group_by(Yr) |>
    dplyr::summarise(value = sum(Obs), .groups='drop') |>
    dplyr::rename(year = Yr) |>
    dplyr::mutate(sector = "Recreational",
                  method = ifelse(year %in% recreational_data, "Recreational data", "Recreational modelled"))

  recreational_plot_adjust <- recreational_catch |>
    dplyr::filter(method=="Recreational data") |>
    dplyr::mutate(method="Recreational plot adjust")

  recreational_catch <- recreational_catch |>
    rbind(recreational_plot_adjust)

  data <- rbind(commercial_catch,recreational_catch)
}
