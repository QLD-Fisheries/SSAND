# Copyright 2024 Fisheries Queensland

# This file is part of SSAND.
# SSAND is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
# SSAND is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
# You should have received a copy of the GNU General Public License along with SSAND. If not, see <https://www.gnu.org/licenses/>.

#' Prepare SS data for Andre plot
#' Andre plots show mean age and standard deviation in conditional age-at-length data.
#' Left plots are mean age at length by size-class (obs. and exp.) with confidence intervals obtained by adding the appropriate number of standard errors of mean to the data.
#' Right plots in each pair are SE of mean age-at-length (obs. and exp.) with confidence intervals based on the chi-square distribution.
#'
#' @param ss_mle A list of outputs from r4ss::SS_output() with one element per scenario. Will automatically reformat as a list if a single r4ss::SS_output() output (i.e. one scenario) is entered.
#' @param scenarios A vector of scenarios to plot (numeric). Shows all scenarios if left blank. Can be overridden in the plotting function.
#' @param sex_code Specify which sex to show on the graph. Sex codes correspond to Stock Synthesis sex codes (1 is female, 2 is male, 3 and 0 are combined.)
#' @param CI Confident interval. Default is 0.95.
#'
#' @importFrom stats "qchisq"
#'
#' @return A data frame with columns "year" (num), "length" (num), "obs" (num), "pred" (num), "low" (num), "upp" (num), "label" (chr), "scenario" (int), "CI" (num)
#' @export
#'
#' @examples
#' data <- andreplot_prep_SS(ss_mle, sex_code=1)
#' andreplot(data)
andreplot_prep_SS <- function(ss_mle,
                              scenarios = NULL,
                              sex_code = # Copyright 2024 Fisheries Queensland

# This file is part of SSAND.
# SSAND is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
# SSAND is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
# You should have received a copy of the GNU General Public License along with SSAND. If not, see <https://www.gnu.org/licenses/>.

NULL,
                              CI = 0.95) {

  if (check_scenarios(ss_mle,"SS","MLE")=="single scenario"){ss_mle <- list(ss_mle); warning("Assuming you are entering a single scenario, not a list of scenarios. Wrap ss_mle input inside a list() to avoid this warning.")}
  if (missing(scenarios)){scenarios <- 1:length(ss_mle)}
  if (missing(sex_code)) {warning("Please specify sex code to plot"); sex_code = ss_mle[[scenarios[[1]]]]$condbase$Sex[[1]]}

  se <- round(qnorm(1-(1-CI)/2),2)
  conf <- CI
  upper <- 1-(1-CI)/2
  lower <- (1-CI)/2

  data <- data.frame()
  for (scenario in scenarios) {
    tmp <- ss_mle[[scenario]]$condbase |>
      dplyr::filter(Sex == sex_code) |>
      dplyr::group_by(Lbin_lo,Yr.S) |>
      dplyr::summarise(ObsV = sum(Bin * Obs / sum(Obs)),
                       ObsV2 = sum(Bin * Bin * Obs / sum(Obs)),
                       PredV = sum(Bin * Exp / sum(Exp)),
                       PredV2 = sum(Bin * Bin * Exp / sum(Exp)),
                       NN = dplyr::first(Nsamp_adj),
                       varn = sqrt(PredV2 - PredV^2) / sqrt(NN),
                       Obs2 = sqrt(max(0, ObsV2 - ObsV^2, na.rm = TRUE)) / sqrt(NN),
                       Low = ObsV - se * Obs2,
                       Upp = ObsV + se * Obs2,
                       .groups = 'drop') |>
      dplyr::mutate(
        Low2 = dplyr::if_else(NN > 1, Obs2 * sqrt(max((NN - 1),0) / qchisq(upper, max(NN,1))), NA_real_), # why are upper and lower swapped?
        Upp2 = dplyr::if_else(NN > 1, Obs2 * sqrt(max((NN - 1),0) / qchisq(lower, max(NN,1))), NA_real_),
        year = floor(Yr.S)) |>
      dplyr::mutate(scenario = scenario, CI = conf)

    data <- rbind(data,tmp)
  }

  data <- rbind(
    data |> dplyr::mutate(label = "Age") |> dplyr::select(year, length = Lbin_lo, obs = ObsV, pred=PredV, low=Low, upp=Upp, label, scenario, CI),
    data |> dplyr::mutate(label = "Standard deviation (age)") |>  dplyr::select(year, length = Lbin_lo, obs = Obs2, pred=varn, low=Low2, upp=Upp2, label, scenario, CI)
  )

  return(data)
}
