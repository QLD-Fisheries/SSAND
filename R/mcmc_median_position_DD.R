# Copyright 2024 Fisheries Queensland

# This file is part of SSAND.
# SSAND is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
# SSAND is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
# You should have received a copy of the GNU General Public License along with SSAND. If not, see <https://www.gnu.org/licenses/>.

#' Extract DDUST MCMC iteration position that produces median final biomass
#'
#' @param dd_mle A list of outputs from DDUST::makefullreport() with one list element per scenario. Will automatically reformat as a list if a single DDUST::makefullreport() output (i.e. one scenario) is entered
#' @param dd_mcmc A list of model fits from tmbstan::tmbstan() with one list element per scenario. Only needed if MCMC was used.
#' @param dd_sim A list of outputs from SSAND::simulate_DD() with one list element per scenario. Only required if MCMC was used. Will automatically reformat as a list if a single DDUST::makefullreport() output (i.e. one scenario) is entered
#' @param scenarios A vector of scenario numbers to be be included in median calculation (numeric). Function will return one median for each scenario.
#'
#' @return A vector of numbers with one entry per scenario entered, where each number is the corresponding MCMC run that produced the median final biomass ratio.
#' @export
#'
#' @examples
#' \dontrun{
#' library(DDUST)
#' dd_sim <- simulate_DDUST(dd_mle,dd_mcmc)
#' pos <- mcmc_median_position_DD(dd_mle, dd_mcmc, dd_sim)
#'
#' # Ensemble model
#' dd_mcmc_ens <- mcmc_ensemble_DD(dd_mcmc,dd_sim,scenarios=c(1,2))$dd_mcmc
#' dd_sim_ens <- mcmc_ensemble_DD(dd_mcmc,dd_sim,scenarios=c(1,2))$dd_sim
#' pos <- mcmc_median_position_DD(dd_mle,dd_mcmc_ens,dd_sim_ens)[1]
#' }
mcmc_median_position_DD <- function(dd_mle,
                                    dd_mcmc,
                                    dd_sim,
                                    scenarios = NULL){

  if (check_scenarios(dd_mle,"DD","MLE")=="single scenario"){dd_mle <- list(dd_mle)}
  if (check_scenarios(dd_mcmc,"DD","MCMC")=="single scenario"){dd_mcmc <- list(dd_mcmc)}
  if (check_scenarios(dd_sim,"DD","SIM")=="single scenario"){dd_sim <- list(dd_sim)}

  if (missing(scenarios)){scenarios <- 1:length(dd_mle)}
  if (check_scenarios(dd_mcmc,"DD","MCMC")=="ensemble") {scenarios <- 1}

  data <- c()
  for (scenario in scenarios) {

    startyr <- dd_mle[[scenario]]$data$first_year_catch
    endyr <- dd_mle[[scenario]]$data$last_year_catch

    # Extract fit_model and model for each scenario, convert fit_model to a matrix
    if (check_scenarios(dd_mcmc,"DD","MCMC")=="ensemble") {
      fit <- dd_mcmc[[1]]
    } else {
      fit <- as.matrix(dd_mcmc[[scenario]])
    }

    nyears <- dd_mle[[scenario]]$data$last_year_catch - dd_mle[[scenario]]$data$first_year_catch + 1


    # Simulate model for each iteration of MCMC
      sim <- dd_sim[[scenario]]

    # Find parameter set that produces median final B_ratio
    B_final <- sapply(1:nrow(fit), function(i){sim[[i]]$B_annual_ratio[nyears]})


    # If there are an odd number of MCMC runs:
    if (length(B_final) %% 2 == 1) {
      pos <- which(B_final == median(B_final[1:(length(B_final))]))
    }


    # If there are an even number of MCMC runs:
    if (length(B_final) %% 2 == 0) {
      pos <- which(B_final == median(B_final[1:(length(B_final)-1)]))
    }


    # In case multiple runs produce the same median (very unlikely!):
    pos <- pos[1]


    data[scenario] <- pos
  }
  return(data)
}
