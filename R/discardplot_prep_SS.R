# Copyright 2024 Fisheries Queensland

# This file is part of SSAND.
# SSAND is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
# SSAND is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
# You should have received a copy of the GNU General Public License along with SSAND. If not, see <https://www.gnu.org/licenses/>.

#' Discard plot
#'
#' @param ss_mle A list of outputs from r4ss::SS_output() with one element per scenario. Will automatically reformat as a list if a single r4ss::SS_output() output (i.e. one scenario) is entered.
#' @param scenarios A vector of scenarios to plot (numeric). Shows all scenarios if left blank. Can be overridden in the plotting function.
#'
#' @return A dataframe with year, fleet, obs, upper, lower, exp
#' @export
#'
#' @examples
#' data <- discardplot_prep_SS(ss_mle)
#' discardplot(data)
discardplot_prep_SS <- function(ss_mle,
                                scenarios = NULL) {

  if (length(ss_mle)>100){warning("Assuming you are entering a single scenario, not a list of scenarios.") ; ss_mle <- list(ss_mle)}
  if (missing(scenarios)){scenarios <- 1:length(ss_mle)}

  data <- data.frame()
  for (scenario in scenarios) {
    temp <- ss_mle[[scenario]]$discard |>
      dplyr::mutate(upper = Obs+Std_use,
             lower = Obs-Std_use,
             scenario = scenario,
             fleet = as.factor(Fleet)) |>
      dplyr::select(year = Yr, fleet, obs = Obs, upper, lower, exp = Exp,scenario)
    data <- rbind(data, temp)
  }
  rownames(data) <- NULL
  return(data)
}
