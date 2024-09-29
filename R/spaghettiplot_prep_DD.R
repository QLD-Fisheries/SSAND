# Copyright 2024 Fisheries Queensland

# This file is part of SSAND.
# SSAND is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
# SSAND is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
# You should have received a copy of the GNU General Public License along with SSAND. If not, see <https://www.gnu.org/licenses/>.

#' Prepare DDUST data for biomassplot()
#'
#' @param dd_mle A list of outputs from DDUST::makefullreport() with one list element per scenario. Will automatically reformat as a list if a single DDUST::makefullreport() output (i.e. one scenario) is entered
#' @param dd_mcmc A list of model fits from tmbstan::tmbstan() with one list element per scenario. Only needed if MCMC was used.
#' @param dd_sim A list of outputs from SSAND::simulate_DD() with one list element per scenario. Only required if MCMC was used. Will automatically reformat as a list if a single DDUST::makefullreport() output (i.e. one scenario) is entered
#' @param base_case The scenario number that pertains to the base case (numeric). If NA, no base case was selected.
#' @param biomass_type The type of biomass used. Options are "relative" or "absolute" (character).
#' @param scenarios A vector of scenarios to plot (numeric). Shows all scenarios if left blank. Can be overridden in the plotting function.
#' @param show_median Type of median shown. Default "trajectory" shows median trajectory based on biomass in final year.
#' "annual_biomass" shows the median of each year
#'
#' @return A data frame with variables called year, scenario, base and value
#' @export
#'
#' @examples
#' data <- spaghettiplot_prep_DD(dd_mle)
#' spaghettiplot(data)
#'
#' \dontrun{
#' library(DDUST)
#' dd_sim <- simulate_DDUST(dd_mle,dd_mcmc)
#' data <- spaghettiplot_prep_DD(dd_mle, dd_mcmc, dd_sim)
#' spaghettiplot(data)
#' }
spaghettiplot_prep_DD <- function(dd_mle,
                                  dd_mcmc = NULL,
                                  dd_sim = NULL,
                                  base_case = 1,
                                  biomass_type = "relative",
                                  scenarios = NULL,
                                  show_median = "trajectory" # trajectory, annual_biomass, parameters, none
) {

  if (missing(dd_mcmc)) {MCMC = FALSE} else {MCMC = TRUE}

  if (check_scenarios(dd_mle,"DD","MLE")=="single scenario"){dd_mle <- list(dd_mle)}
  if (MCMC && check_scenarios(dd_mcmc,"DD","MCMC")=="single scenario"){dd_mcmc <- list(dd_mcmc)}
  if (MCMC && check_scenarios(dd_sim,"DD","SIM")=="single scenario"){dd_sim <- list(dd_sim)}


  if (missing(scenarios)){scenarios <- 1:length(dd_mle)}

  if (!MCMC) {
    data <- SSAND::biomassplot_prep_DD(dd_mle=dd_mle,
                                         scenarios=scenarios,
                                         biomass_type=biomass_type) |>
      dplyr::mutate(base = as.factor(ifelse(scenario==base_case,'Base','Alt'))) |>
      dplyr::select(year, scenario, value, base)

  } else {
    data <- SSAND::biomassplot_prep_DD(dd_mle=dd_mle,
                                         dd_mcmc=dd_mcmc,
                                         dd_sim=dd_sim,
                                         scenarios=scenarios,
                                         biomass_type=biomass_type) |>
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
