# Copyright 2024 Fisheries Queensland

# This file is part of SSAND.
# SSAND is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
# SSAND is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
# You should have received a copy of the GNU General Public License along with SSAND. If not, see <https://www.gnu.org/licenses/>.

#' Prepare Stock Synthesis  data for yield plot
#'
#' @param ss_mle A list of outputs from r4ss::SS_output() with one element per scenario. Will automatically reformat as a list if a single r4ss::SS_output() output (i.e. one scenario) is entered.
#' @param ss_mcmc A list of outputs from r4ss::SSgetMCMC() with one element per scenario. Only needed if MCMC was used. Will automatically reformat as a list if a single r4ss::SSgetMCMC() output (i.e. one scenario) is entered.
#' @param scenarios A vector of scenarios to plot (numeric). Shows all scenarios if left blank. Can be overridden in the plotting function.
#'
#' @return A dataframe with Final_bio (num), yield (num), data (chr), scenario (int)
#' @export
#'
#' @examples
#' data <- yieldplot_prep_SS(ss_mle)
#' yieldplot(data)
yieldplot_prep_SS <- function(ss_mle,
                              ss_mcmc,
                              scenarios = NULL) {

  if (missing(ss_mcmc)) {MCMC = FALSE} else {MCMC = TRUE}
  if (check_scenarios(ss_mle,"SS","MLE")=="single scenario"){ss_mle <- list(ss_mle); warning("Assuming you are entering a single scenario, not a list of scenarios. Wrap ss_mle input inside a list() to avoid this warning.")}
  if (MCMC && check_scenarios(ss_mcmc,"SS","MCMC")=="single scenario"){ss_mcmc <- list(ss_mcmc); warning("Assuming you are entering a single scenario, not a list of scenarios. Wrap ss_mcmc input inside a list() to avoid this warning.")}

  if (missing(scenarios)){scenarios <- 1:length(ss_mle)}


  if (!MCMC) {

    data <- data.frame()
    for (scenario in scenarios) {
      yield <- ss_mle[[scenario]]$equil_yield |>
        dplyr::select(Final_bio = Depletion, yield = Tot_Catch) |>
        dplyr::mutate(data = "data") |>
        dplyr::mutate(scenario = as.factor(scenario))
      rownames(yield) <- NULL

      which_val <- which(abs(yield[["Final_bio"]] -
                               ss_mle[[scenario]][["current_depletion"]]) == min(abs(yield[["Final_bio"]] -
                                                                                       ss_mle[[scenario]][["current_depletion"]])))[1]

      yield$data[which_val] <- "final"

      Bmsy <- ss_mle[[scenario]]$derived_quants |>
        dplyr::filter(Label == "B_MSY/SSB_unfished") |>
        dplyr::select(Value) |>
        dplyr::pull()

      msy <- ss_mle[[scenario]]$derived_quants |>
        dplyr::filter(Label == "Dead_Catch_MSY") |>
        dplyr::select(Value) |>
        dplyr::pull()

      yield <- yield |>
        rbind(data.frame(Final_bio = Bmsy,
                         yield = msy,
                         data = "msy",
                         scenario = as.factor(scenario)))
      data <- rbind(data, yield)
    }
  } else {
    warning("Functionality not yet built for Stock Synthesis MCMC yield curve.")
  }

  rownames(data) <- NULL
  return(data)
}

