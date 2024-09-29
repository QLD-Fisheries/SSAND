# Copyright 2024 Fisheries Queensland

# This file is part of SSAND.
# SSAND is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
# SSAND is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
# You should have received a copy of the GNU General Public License along with SSAND. If not, see <https://www.gnu.org/licenses/>.

#' MCMC ensemble preparation for DDUST
#'
#' @param dd_mcmc A list of model fits from tmbstan::tmbstan() with one list element per scenario. Only needed if MCMC was used.
#' @param dd_sim A list of outputs from DDUST::simulate_DD() with one list element per scenario. Only required if MCMC was used. Will automatically reformat as a list if a single DDUST::makefullreport() output (i.e. one scenario) is entered
#' @param scenarios A vector of scenarios to be included in the ensemble.
#' @param weighting A vector of integers to weight each scenario, one integer per scenario.
#' The MCMC iterations for each scenario are repeated the number of times indicated by the corresponding element in the weighting vector.
#' For example, for an ensemble with four scenarios, to double the weighting of the first scenario you would enter weighting=c(2,1,1,1).
#'
#' @return A list containing one dataframe of all specified dd_mcmc and dd_sim objects combined, limited to columns that are common between dd_mcmc objects, duplicated as per the specified weighting.
#' An extra column "ensemble" is appended, for datachecks in other functions.
#' @export
#'
#' @examples
#' \dontrun{
#' library(DDUST)
#' dd_sim <- simulate_DDUST(dd_mle,dd_mcmc)
#' dd_mcmc_ens <- mcmc_ensemble_DD(dd_mcmc,dd_sim,scenarios=c(1,2))$dd_mcmc
#' dd_sim_ens <- mcmc_ensemble_DD(dd_mcmc,dd_sim,scenarios=c(1,2))$dd_sim
#  data <- biomassplot_prep_DD(dd_mle[2],dd_mcmc_ens)
#' biomassplot(data)
#' }
mcmc_ensemble_DD <- function(dd_mcmc,
                             dd_sim,
                             scenarios = NULL,
                             weighting = NULL) {
  if (check_scenarios(dd_mcmc,"DD","MCMC")=="single scenario"){dd_mcmc <- list(dd_mcmc)}
  if (check_scenarios(dd_sim,"DD","SIM")=="single scenario"){dd_sim <- list(dd_sim)}

  if (missing(scenarios)) {scenarios <- 1:length(dd_mcmc)}

  # If no weighting provided, assume all scenarios bear equal weight
  if (missing(weighting)) {weighting = rep(1,length(scenarios))}

  # Check weighting is all integer
  if (!all(weighting==as.integer(weighting))) {
    warning("You have entered a non-intger value in the weighting vector. Please adjust weighting input to be a vector of integers.")
  }

  dd_mcmc_ens <- lapply(dd_mcmc, function(x) data.frame(as.matrix(x)))

  # Check the parameters are the same. Only do ensemble for matching parameters.
  names <- list()
  for (i in 1:length(scenarios)) {
    names[[i]]<- names(dd_mcmc_ens[[scenarios[i]]])
  }
  common_parameters <- Reduce(intersect,names)

  # Create vector for weighting
  weight_repeats <- rep(scenarios, times = weighting)

  # Transform MCMC list to data frame
  dd_mcmc_subset_common <- data.frame()
  for (i in weight_repeats) {
    tmp <- dd_mcmc_ens[[i]] |>
      dplyr::select(common_parameters)
    dd_mcmc_subset_common <- rbind(dd_mcmc_subset_common,tmp)
    rm(tmp)
  }

  # Transform sim list to combined list
  dd_sim_ens <- list()
  for (i in weight_repeats) {
    dd_sim_ens <- c(dd_sim_ens,dd_sim[[i]])
  }

  data <- list(dd_mcmc = list(dd_mcmc_subset_common),
               dd_sim = list(dd_sim_ens))
  return(data)
}

