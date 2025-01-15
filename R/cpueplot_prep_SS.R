# Copyright 2024 Fisheries Queensland

# This file is part of SSAND.
# SSAND is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
# SSAND is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
# You should have received a copy of the GNU General Public License along with SSAND. If not, see <https://www.gnu.org/licenses/>.

#' Prepare Stock Synthesis data for catch per unit effort (cpue) fit plot
#'
#' @param ss_mle A list of outputs from r4ss::SS_output() with one element per scenario. Will automatically reformat as a list if a single r4ss::SS_output() output (i.e. one scenario) is entered.
#' @param ss_mcmc A list of outputs from r4ss::SSgetMCMC() with one element per scenario. Only needed if MCMC was used. Will automatically reformat as a list if a single r4ss::SSgetMCMC() output (i.e. one scenario) is entered.
#' @param scenarios A vector of scenarios to plot (numeric). Shows all scenarios if left blank. Can be overridden in the plotting function.
#' @param month_override Set to TRUE to change month to 1 for each year. Useful for monthly models.
#'
#' @return A data frame with variables date, fleet, obs, exp, ub, lb, scenario
#' @export
#'
#' @importFrom stats "qlnorm"
#'
#' @examples
#' data <- cpueplot_prep_SS(ss_mle)
#' cpueplot(data)
cpueplot_prep_SS <- function(ss_mle,
                             ss_mcmc = NULL,
                             scenarios = NULL,
                             month_override = TRUE){

  if (missing(ss_mcmc)) {MCMC = FALSE} else {MCMC = TRUE}
  if (check_scenarios(ss_mle,"SS","MLE")=="single scenario"){ss_mle <- list(ss_mle); warning("Assuming you are entering a single scenario, not a list of scenarios. Wrap ss_mle input inside a list() to avoid this warning.")}
  if (MCMC && check_scenarios(ss_mcmc,"SS","MCMC")=="single scenario"){ss_mcmc <- list(ss_mcmc); warning("Assuming you are entering a single scenario, not a list of scenarios. Wrap ss_mcmc input inside a list() to avoid this warning.")}

  if (missing(scenarios)){scenarios <- 1:length(ss_mle)}



  if (!MCMC) {
    data <- data.frame()
    for (scenario in scenarios) {

      if (month_override) {ss_mle[[scenario]]$cpue$Month <- 1}

      if (any(ss_mle[[scenario]]$survey_error) != 0) {
        warning('Data preparation assumed error distribution is lognormal, but SS output indicates your fleet uses normal error or Students t-distrubtion. ')
      }

      temp <- ss_mle[[scenario]]$cpue |>
        dplyr::mutate(date = as.Date(paste0('01/',Month,'/',Yr), format = '%d/%m/%Y')) |>
        dplyr::mutate(ub = qlnorm(0.975, meanlog = log(Obs),sdlog = SE),
                      lb = qlnorm(0.025, meanlog = log(Obs),sdlog = SE)) |>
        dplyr::mutate(scenario = as.factor(scenario)) |>
        dplyr::select(date, fleet=Fleet, obs=Obs, exp=Exp, ub, lb, scenario)
      data <- rbind(data, temp)
    }
  } else {

    warning("A Stock Synthesis MCMC CPUE plot is currently not plausible. The MCMC output for Stock Synthesis models does not include CPUE time series, nor does it include vulnerable biomass.")
  }
  rownames(data) <- NULL
  return(data)
}


