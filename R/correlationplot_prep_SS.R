# Copyright 2024 Fisheries Queensland

# This file is part of SSAND.
# SSAND is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
# SSAND is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
# You should have received a copy of the GNU General Public License along with SSAND. If not, see <https://www.gnu.org/licenses/>.

#' Prepare Stock Synthesis data frame for correlationplot
#'
#' @param ss_mle A list of outputs from r4ss::SS_output() with one element per scenario. Will automatically reformat as a list if a single r4ss::SS_output() output (i.e. one scenario) is entered.
#' @param ss_mcmc A list of outputs from r4ss::SSgetMCMC() with one element per scenario. Only needed if MCMC was used. Will automatically reformat as a list if a single r4ss::SSgetMCMC() output (i.e. one scenario) is entered.
#' @param scenario The scenario to plot (numeric). Shows scenario 1 if left blank. Can be overridden in the plotting function.
#' @param parameters A vector of parameters to include on plot (character). Use SSAND::extract_SS_parameters() to find a list of parameter names used.
#'
#' @return A data frame for correlation plot with X1 (fac), X2 (fac) and value (num)
#' @export
#'
#' @examples
#' parameters <- extract_SS_parameters(ss_mle)[c(2,3,4,26),]
#' data <- correlationplot_prep_SS(ss_mle, scenario = 1, parameters = parameters)
#' correlationplot(data)
#'
#' parameters <- extract_SS_parameters(ss_mle)[c(2,3,4,26),]
#' data <- correlationplot_prep_SS(ss_mle, ss_mcmc, scenario = 1, parameters = parameters)
#' correlationplot(data)
correlationplot_prep_SS <- function(ss_mle,
                                    ss_mcmc = NULL,
                                    scenario = 1,
                                    parameters) {

  if (missing(ss_mcmc)) {MCMC = FALSE} else {MCMC = TRUE}
  if (check_scenarios(ss_mle,"SS","MLE")=="single scenario"){ss_mle <- list(ss_mle); warning("Assuming you are entering a single scenario, not a list of scenarios. Wrap ss_mle input inside a list() to avoid this warning.")}
  if (MCMC && check_scenarios(ss_mcmc,"SS","MCMC")=="single scenario"){ss_mcmc <- list(ss_mcmc); warning("Assuming you are entering a single scenario, not a list of scenarios. Wrap ss_mcmc input inside a list() to avoid this warning.")}

  if (MCMC & !"chain" %in% names(ss_mcmc[[scenario]])) {ss_mcmc[[scenario]] <- ss_mcmc[[scenario]] |> dplyr::mutate(chain=1)}


  if (MCMC==FALSE) {
    data <- ss_mle[[scenario]]$CoVar |>
      dplyr::filter(label.i %in% parameters,
                    label.j %in% parameters) |>
      dplyr::rename(X1 = label.i, X2 = label.j, value = corr) |>
      dplyr::select(X1, X2, value)

    lev <- c(setdiff(data$X2,data$X1), unique(data$X1))

    data <- data |>
      dplyr::mutate(X1 = factor(X1, levels = lev),
                    X2 = factor(X2, levels = lev)) |>
      dplyr::mutate(MCMC = FALSE)
  }

  if (MCMC) {
    data <- ss_mcmc[[scenario]] |>
      dplyr::select(iter=Iter, chain,parameters) |>
      dplyr::mutate(MCMC = TRUE) |>
      dplyr::mutate(chain = as.factor(chain)) |>
      dplyr::select(chain, iter, dplyr::everything())
  }


  return(data)
}
