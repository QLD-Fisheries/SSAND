# Copyright 2024 Fisheries Queensland

# This file is part of SSAND.
# SSAND is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
# SSAND is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
# You should have received a copy of the GNU General Public License along with SSAND. If not, see <https://www.gnu.org/licenses/>.

#' Prepare Stock Synthesis data for spawningoutputplot()
#'
#' @param ss_mle A list of outputs from r4ss::SS_output() with one element per scenario. Will automatically reformat as a list if a single r4ss::SS_output() output (i.e. one scenario) is entered.
#' @param ss_mcmc A list of outputs from r4ss::SSgetMCMC() with one element per scenario. Only needed if MCMC was used. Will automatically reformat as a list if a single r4ss::SSgetMCMC() output (i.e. one scenario) is entered.
#' @param scenarios A vector of scenarios to plot (numeric). Shows all scenarios if left blank. Can be overridden in the plotting function.
#'
#' @return A dataframe with xvar (int), maturityxfecundity (num), x (chr), scenario (int)
#' @export
#'
#' @examples
#' data <- spawningoutputplot_prep_SS(ss_mle)
#' spawningoutputplot(data,xaxis="length")
spawningoutputplot_prep_SS <- function(ss_mle,
                                       ss_mcmc = NULL,
                                       scenarios = NULL) {

  if (missing(ss_mcmc)) {MCMC = FALSE} else {MCMC = TRUE}
  if (check_scenarios(ss_mle,"SS","MLE")=="single scenario"){ss_mle <- list(ss_mle); warning("Assuming you are entering a single scenario, not a list of scenarios. Wrap ss_mle input inside a list() to avoid this warning.")}
  if (MCMC && check_scenarios(ss_mcmc,"SS","MCMC")=="single scenario"){ss_mcmc <- list(ss_mcmc); warning("Assuming you are entering a single scenario, not a list of scenarios. Wrap ss_mcmc input inside a list() to avoid this warning.")}

  if (missing(scenarios)){scenarios <- 1:length(ss_mle)}

  data <- data.frame()
  for (scenario in scenarios) {
    age <- ss_mle[[scenario]]$endgrowth |>
      dplyr::select(xvar = Age_Beg, maturityxfecundity= `Mat*Fecund`) |>
      dplyr::mutate(x = "age", scenario = scenario)

    # We only want the top half of the age object (due to nature of ss_mle$endgrowth object)
    age <- age[1:(nrow(age)/2),]

    length <- ss_mle[[scenario]]$biology |>
      dplyr::select(xvar = Len_mean, maturityxfecundity = `Mat*Fec`) |>
      dplyr::mutate(x = "length", scenario = scenario)

    data <- rbind(data, age, length)
  }
  return(data)
}
