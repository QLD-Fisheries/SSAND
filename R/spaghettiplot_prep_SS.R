# Copyright 2024 Fisheries Queensland

# This file is part of SSAND.
# SSAND is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
# SSAND is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
# You should have received a copy of the GNU General Public License along with SSAND. If not, see <https://www.gnu.org/licenses/>.

#' Prepare Stock Synthesis data for biomassplot()
#'
#' @param ss_mle A list of outputs from r4ss::SS_output() with one element per scenario. Will automatically reformat as a list if a single r4ss::SS_output() output (i.e. one scenario) is entered.
#' @param ss_mcmc A list of outputs from r4ss::SSgetMCMC() with one element per scenario. Only needed if MCMC was used. Will automatically reformat as a list if a single r4ss::SSgetMCMC() output (i.e. one scenario) is entered.
#' @param base_case The scenario number that pertains to the base case (numeric). If NA, no base case was selected.
#' @param biomass_type The type of biomass used. Options are "relative" or "absolute" (character).
#' @param scenarios A vector of scenarios to plot (numeric). Shows all scenarios if left blank. Can be overridden in the plotting function.
#' @param show_median Type of median shown. Default "trajectory" shows median trajectory based on biomass in final year.
#' "annual_biomass" shows the median of each year
#' @param period Optional: time period of stock assessment.
#'
#' @return A data frame with variables called year, scenario, base and value
#' @export
#' @examples
#' data <- spaghettiplot_prep_SS(ss_mle)
#' spaghettiplot(data)
#'
#' data <- spaghettiplot_prep_SS(ss_mle, ss_mcmc)
#' spaghettiplot(data)
spaghettiplot_prep_SS <- function(ss_mle,
                                  ss_mcmc = NULL,
                                  base_case = 1,
                                  biomass_type = "relative",
                                  scenarios = NULL,
                                  period = NULL,
                                  show_median = "trajectory" # trajectory, annual_biomass, parameters, none
) {

  if (missing(ss_mcmc)) {MCMC = FALSE} else {MCMC = TRUE}
  if (check_scenarios(ss_mle,"SS","MLE")=="single scenario"){ss_mle <- list(ss_mle); warning("Assuming you are entering a single scenario, not a list of scenarios. Wrap ss_mle input inside a list() to avoid this warning.")}
  if (MCMC && check_scenarios(ss_mcmc,"SS","MCMC")=="single scenario"){ss_mcmc <- list(ss_mcmc); warning("Assuming you are entering a single scenario, not a list of scenarios. Wrap ss_mcmc input inside a list() to avoid this warning.")}

  if (missing(period)) {period = (ss_mle[[1]]$startyr+1) : (ss_mle[[1]]$endyr+1)}

  if (missing(scenarios)){scenarios <- 1:length(ss_mle)}

  if (!MCMC) {
    data <- SSAND::biomassplot_prep_SS(ss_mle=ss_mle,
                                       scenarios=scenarios,
                                       biomass_type=biomass_type,
                                       period=period) |>
      dplyr::mutate(base = as.factor(ifelse(scenario==base_case,'Base','Alt'))) |>
      dplyr::select(year, scenario, value, base)

  } else {
    data <- SSAND::biomassplot_prep_SS(ss_mle=ss_mle,
                                       ss_mcmc=ss_mcmc,
                                       scenarios=scenarios,
                                       biomass_type=biomass_type,
                                       period=period) |>
      dplyr::filter(med == show_median, biomass_type == biomass_type) |>
      dplyr::mutate(base = as.factor(ifelse(scenario==base_case,'Base','Alt'))) |>
      dplyr::select(year, scenario, value, base)
  }

  if(is.na(base_case)){
    data_levels = "Alt"
  }else{
    data_levels = c("Base","Alt")
  }

  data <- data |>
    dplyr::mutate(year = as.integer(year),
                  scenario = as.factor(scenario),
                  value = as.numeric(value),
                  base = factor(base, levels = data_levels)
    )

  rownames(data) <- NULL
  return(data)
}
