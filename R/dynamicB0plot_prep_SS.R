# Copyright 2024 Fisheries Queensland

# This file is part of SSAND.
# SSAND is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
# SSAND is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
# You should have received a copy of the GNU General Public License along with SSAND. If not, see <https://www.gnu.org/licenses/>.

#' Prepare Stock Synthesis data for dynamicB0plot()
#'
#' @param ss_mle A list of outputs from r4ss::SS_output() with one element per scenario. Will automatically reformat as a list if a single r4ss::SS_output() output (i.e. one scenario) is entered.
#' @param scenarios A vector of scenarios to plot (numeric). Shows all scenarios if left blank. Can be overridden in the plotting function.
#'
#' @return A data frame with variables called year (int), era (chr), SSB (num), SSB_nofishing (num), scenario (int)
#' @export
#'
#' @examples
#' data <- dynamicB0plot_prep_SS(ss_mle)
#' dynamicB0plot(data)
dynamicB0plot_prep_SS <- function(ss_mle,
                                  scenarios = NULL) {

  if (check_scenarios(ss_mle,"SS","MLE")=="single scenario"){ss_mle <- list(ss_mle); warning("Assuming you are entering a single scenario, not a list of scenarios. Wrap ss_mle input inside a list() to avoid this warning.")}

  if (missing(scenarios)){scenarios <- 1:length(ss_mle)}

  data <- data.frame()

  for (scenario in scenarios) {

    tmp <- ss_mle[[scenario]]$Dynamic_Bzero |>
      dplyr::rename(year = Yr, era = Era, `SSB no fishing` = SSB_nofishing) |>
      tidyr::pivot_longer(-c(year, era), values_to = "value", names_to = "type") |>
      dplyr::mutate(scenario = scenario)

    data <- rbind(data, tmp)
  }

  return(data)
}


