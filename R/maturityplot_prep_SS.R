# Copyright 2024 Fisheries Queensland

# This file is part of SSAND.
# SSAND is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
# SSAND is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
# You should have received a copy of the GNU General Public License along with SSAND. If not, see <https://www.gnu.org/licenses/>.

#' Prepare Stock Synthesis data for maturity and fecundity
#'
#' @param ss_mle A list of outputs from r4ss::SS_output() with one element per scenario. Will automatically reformat as a list if a single r4ss::SS_output() output (i.e. one scenario) is entered.
#' @param scenarios A vector of scenarios to plot (numeric). Shows all scenarios if left blank. Can be overridden in the plotting function.
#'
#' @return A data frame with variables value, maturity, sex, scenario and type (length or age)
#' @export
#'
#' @examples
#' data <- maturityplot_prep_SS(ss_mle)
#' maturityplot(data)
maturityplot_prep_SS <- function(ss_mle,
                                 scenarios = NULL) {

  if (check_scenarios(ss_mle,"SS","MLE")=="single scenario"){ss_mle <- list(ss_mle); warning("Assuming you are entering a single scenario, not a list of scenarios. Wrap ss_mle input inside a list() to avoid this warning.")}
  if (missing(scenarios)){scenarios <- 1:length(ss_mle)}

  data <- data.frame()
  for (scenario in scenarios) {
    tmpLL <- ss_mle[[scenario]]$endgrowth |>
      dplyr::select(Len_Beg, Len_Mat, sex = Sex) |>
      dplyr::mutate(scenario = scenario) |>
      dplyr::rename(value = Len_Beg, maturity = Len_Mat) |>
      dplyr::mutate(type = "length1")

    tmpAL <- ss_mle[[scenario]]$endgrowth |>
      dplyr::select(Age_Beg, Len_Mat, sex = Sex) |>
      dplyr::mutate(scenario = scenario) |>
      dplyr::rename(value = Age_Beg, maturity = Len_Mat) |>
      dplyr::mutate(type = "age1")

    tmpAA <- ss_mle[[scenario]]$endgrowth |>
      dplyr::select(Age_Beg, Age_Mat, sex = Sex) |>
      dplyr::mutate(scenario = scenario) |>
      dplyr::rename(value = Age_Beg, maturity = Age_Mat) |>
      dplyr::mutate(type = "age2")

    tmpLA <- ss_mle[[scenario]]$endgrowth |>
      dplyr::select(Len_Beg, Age_Mat, sex = Sex) |>
      dplyr::mutate(scenario = scenario) |>
      dplyr::rename(value = Len_Beg, maturity = Age_Mat) |>
      dplyr::mutate(type = "length2")

    tmp <- rbind(tmpLA,tmpLL,tmpAL,tmpAA)
    data <- rbind(data, tmp)
  }

  return(data)

}
