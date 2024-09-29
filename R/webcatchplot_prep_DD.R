# Copyright 2024 Fisheries Queensland

# This file is part of SSAND.
# SSAND is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
# SSAND is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
# You should have received a copy of the GNU General Public License along with SSAND. If not, see <https://www.gnu.org/licenses/>.

#' Prepare Stock Synthesis data for webcatchplot()
#'
#' This prep function works on the assumption that all harvest to be plotted on the web summary is contained in a model fleet.
#'
#' @param dd_mle A list of outputs from DDUST::makefullreport() with one list element per scenario. Will automatically reformat as a list if a single DDUST::makefullreport() output (i.e. one scenario) is entered
#' @param scenario Which scenario to plot (numeric). Default is 1.
#' @param commercial_data A vector of years to define when commercial data exist. Data are entered in pairs.
#' For example commercial_data = c(1942,1972,1988,2023) implies that data exists from 1942 to 1972, then 1988 to 2023.
#'
#' @return A data frame with variables called year (num), sector (chr), value (num) and method (chr)
#' @export
#'
#' @examples
#' data <- webcatchplot_prep_DD(dd_mle)
#' webcatchplot(data)
webcatchplot_prep_DD <- function(dd_mle,
                                 scenario = 1,
                                 commercial_data = c(1970,2003,2008,2023)) {
  if (check_scenarios(dd_mle,"DD","MLE")=="single scenario"){dd_mle <- list(dd_mle)}

  commercial_data_years <- c()
  for (i in 1:(length(commercial_data)/2)) {
    add <- commercial_data[i*2-1]:commercial_data[i*2]
    commercial_data_years <- c(commercial_data_years, add)
  }

  commercial_catch <- catchplot_prep_DD(dd_mle,scenarios=scenario) |>
    dplyr::mutate(year = lubridate::year(date)) |>
    dplyr::group_by(year) |>
    dplyr::summarise(value = sum(value), .groups='drop') |>
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

  data <- rbind(commercial_catch)
}
