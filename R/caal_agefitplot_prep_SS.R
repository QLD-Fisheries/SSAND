# Copyright 2024 Fisheries Queensland

# This file is part of SSAND.
# SSAND is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
# SSAND is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
# You should have received a copy of the GNU General Public License along with SSAND. If not, see <https://www.gnu.org/licenses/>.

#' Prepare Stock Synthesis data for plot to show age fits from conditional-age-at-length data
#'
#' @param ss_mle A list of outputs from r4ss::SS_output() with one element per scenario. Will automatically reformat as a list if a single r4ss::SS_output() output (i.e. one scenario) is entered.
#' @param scenarios A vector of scenarios to plot (numeric). Shows all scenarios if left blank. Can be overridden in the plotting function.
#' @param sex_code Specify which sex to show on the graph. Sex codes correspond to Stock Synthesis sex codes (1 is female, 2 is male, 3 and 0 are combined.)
#'
#' @return A data frame with year (int), bin (int), obs (num), exp (num), scenario (int), sex (int)
#' @export
#'
#' @examples
#' data <- caal_agefitplot_prep_SS(ss_mle)
#' caal_agefitplot(data, scenario=1,show_fits=FALSE)
caal_agefitplot_prep_SS <- function(ss_mle,
                                    sex_code = 1,
                                    scenarios=NULL) {

  if (check_scenarios(ss_mle,"SS","MLE")=="single scenario"){ss_mle <- list(ss_mle)}
  if (missing(scenarios)){scenarios <- 1:length(ss_mle)}
  if (missing(sex_code)) {warning("Please specify sex code to plot"); sex_code = ss_mle[[scenarios[[1]]]]$condbase$Sex[[1]]}


  data <- data.frame()
  for (scenario in scenarios) {

    tmp <- ss_mle[[scenario]]$condbase |>
      dplyr::mutate(Obs0 = round(Nsamp_in * (Obs - min(Obs)), 2),
                    Exp0 = Nsamp_in * (Exp - min(Obs))) |>
      dplyr::rename(year=Yr, bin=Bin) |>
      dplyr::group_by(year, bin, sex) |>
      dplyr::summarise(obs = sum(Obs0),
                       exp = sum(Exp0),
                       .groups = 'drop') |>
      dplyr::mutate(scenario = scenario)

    data <- rbind(data, tmp)
  }

  data <- data |> dplyr::filter(sex==sex_code)

  return(data)
}
