# Copyright 2024 Fisheries Queensland

# This file is part of SSAND.
# SSAND is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
# SSAND is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
# You should have received a copy of the GNU General Public License along with SSAND. If not, see <https://www.gnu.org/licenses/>.

#' Prepare Stock Synthesis data for mcmc_rhatplot
#'
#' @param ss_mle A list of outputs from r4ss::SS_output() with one element per scenario. Will automatically reformat as a list if a single r4ss::SS_output() output (i.e. one scenario) is entered.
#' @param ss_mcmc A list of outputs from r4ss::SSgetMCMC() with one element per scenario. Only needed if MCMC was used. Will automatically reformat as a list if a single r4ss::SSgetMCMC() output (i.e. one scenario) is entered.
#' @param scenarios A vector of scenarios to plot (numeric). Shows all scenarios if left blank. Can be overridden in the plotting function.
#' @param parameters A vector of parameters to include on plot (character). Use SSAND::extract_SS_parameters() to find a list of parameter names used.
#'
#' @return A dataframe with variables  parameter (factor), rhat (num), scenario (factor), group (fct), xmax (num)
#' @export
#'
#' @examples
#' data <- mcmc_rhatplot_prep_SS(ss_mle, ss_mcmc,
#'            parameters = extract_SS_parameters(ss_mle)[c(2:10),])
#' mcmc_rhatplot(data)
mcmc_rhatplot_prep_SS <- function(ss_mle,
                                  ss_mcmc,
                                  scenarios = NULL,
                                  parameters = NULL) {

  if (check_scenarios(ss_mle,"SS","MLE")=="single scenario"){ss_mle <- list(ss_mle); warning("Assuming you are entering a single scenario, not a list of scenarios. Wrap ss_mle input inside a list() to avoid this warning.")}
  if (check_scenarios(ss_mcmc,"SS","MCMC")=="single scenario"){ss_mcmc <- list(ss_mcmc); warning("Assuming you are entering a single scenario, not a list of scenarios. Wrap ss_mcmc input inside a list() to avoid this warning.")}

  if (missing(parameters)) {
    parameters = extract_SS_parameters(ss_mle)[1:5,]
    warning("You have not specified the parameters to display so we've chosen a few. Use extract_SS_parameters(ss_mle) to see available options. ")
  }

  if (missing(scenarios)){scenarios <- 1:length(ss_mcmc)}

  data <- data.frame()
  for (scenario in scenarios) {
    for (parameter in 2:dim(ss_mcmc[[scenario]])[2]) {

      parameter <- names(ss_mcmc[[scenario]])[parameter]
      Rhat<- rstan::Rhat(ss_mcmc[[scenario]][,parameter])

      data <- rbind(data, data.frame(parameter=parameter,Rhat = Rhat, scenario = scenario))
    }
  }

  data <- data |>
    dplyr::filter(parameter %in% parameters) |>
    dplyr::mutate(group=dplyr::case_when(Rhat<= 1.05 ~ paste(as.character(expression(R)), as.character(expression("\u2264")), '1.05'),
                                         Rhat > 1.05 & Rhat < 1.1  ~ paste(as.character(expression(R)), as.character(expression("\u2264")), '1.1'),
                                         TRUE ~ paste0(as.character(expression(R)),' > 1.1')))  |>
    dplyr::mutate(group=factor(group, levels=c(paste(as.character(expression(R)), as.character(expression("\u2264")), '1.05'),
                                               paste(as.character(expression(R)), as.character(expression("\u2264")), '1.1'),
                                               paste0(as.character(expression(R)),' > 1.1')))) |>
    dplyr::mutate(parameter=as.factor(parameter)) |>
    dplyr::group_by(scenario) |>
    dplyr::mutate(xmax = max(1.1,max(Rhat))) |>
    dplyr::ungroup() |>
    dplyr::mutate(scenario = as.factor(scenario))
}
