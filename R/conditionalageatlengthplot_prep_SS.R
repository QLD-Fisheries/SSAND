# Copyright 2024 Fisheries Queensland

# This file is part of SSAND.
# SSAND is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
# SSAND is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
# You should have received a copy of the GNU General Public License along with SSAND. If not, see <https://www.gnu.org/licenses/>.

#' Prepare Stock Synthesis data for conditional age-at-length plot
#'
#' @param ss_mle A list of outputs from r4ss::SS_output() with one element per scenario. Will automatically reformat as a list if a single r4ss::SS_output() output (i.e. one scenario) is entered.
#' @param scenarios A vector of scenarios to plot (numeric). Shows all scenarios if left blank. Can be overridden in the plotting function.
#' @param display_threshold The minimum value to be displayed on the graph (numeric). Observed or Pearson values below this will not be represented a bubble.
#' @param sex_code Specify which sex to show on the graph. Sex codes correspond to Stock Synthesis sex codes (1 is female, 2 is male, 3 and 0 are combined.)
#'
#' @return A data frame with year, bin, fleet, lbin_low, pearson
#' @export
#'
#' @examples
#' data <- conditionalageatlengthplot_prep_SS(ss_mle,sex_code=1)
#' conditionalageatlengthplot(data, show_fits=FALSE)
conditionalageatlengthplot_prep_SS <- function(ss_mle,
                                               scenarios = NULL,
                                               display_threshold = 0.001,
                                               sex_code = NULL) {
  if (check_scenarios(ss_mle,"SS","MLE")=="single scenario"){ss_mle <- list(ss_mle); warning("Assuming you are entering a single scenario, not a list of scenarios. Wrap ss_mle input inside a list() to avoid this warning.")}

  if (missing(scenarios)){scenarios <- 1:length(ss_mle)}

  if (missing(sex_code)) {warning("Please specify sex code to plot"); sex_code = ss_mle[[scenarios[[1]]]]$condbase$Sex[[1]]}

  data <- data.frame()
  for (scenario in scenarios) {

    tmp <- ss_mle[[scenario]]$condbase |>
      dplyr::filter(Sex==sex_code) |>
      dplyr::select(year=Yr, bin=Bin, fleet=Fleet, lbin_low=Lbin_lo, pearson=Pearson, obs=Obs, sex = Sex) |>
      dplyr::mutate(scenario = scenario)

    data <- rbind(data, tmp)
  }

  data <- data |>
    dplyr::mutate(obs = ifelse(obs<display_threshold,0,obs)) |>
    dplyr::mutate(pearson = ifelse(abs(pearson)<display_threshold, 0, pearson))

  return(data)
}

