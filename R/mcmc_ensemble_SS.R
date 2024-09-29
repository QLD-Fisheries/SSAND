# Copyright 2024 Fisheries Queensland

# This file is part of SSAND.
# SSAND is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
# SSAND is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
# You should have received a copy of the GNU General Public License along with SSAND. If not, see <https://www.gnu.org/licenses/>.

#' MCMC ensemble preparation for Stock Synthesis
#'
#' @param ss_mcmc A list of outputs from r4ss::SSgetMCMC() with one element per scenario. Only needed if MCMC was used. Will automatically reformat as a list if a single r4ss::SSgetMCMC() output (i.e. one scenario) is entered.
#' @param scenarios A vector of scenarios to be included in the ensemble.
#' @param weighting A vector of integers to weight each scenario, one integer per scenario.
#' The MCMC iterations for each scenario are repeated the number of times indicated by the corresponding element in the weighting vector.
#' For example, for an ensemble with four scenarios, to double the weighting of the first scenario you would enter weighting=c(2,1,1,1).
#'
#' @return A list containing one dataframe of all specified ss_mcmc objects combined, limited to columns that are common between ss_mcmc objects, duplicated as per the specified weighting.
#' An extra column "ensemble" is appended, for datachecks in other functions.
#' @export
#'
#' @examples
#' ss_mcmc_ens <- mcmc_ensemble_SS(ss_mcmc,scenarios=c(1,2))
#' ss_mcmc_ens <- mcmc_ensemble_SS(ss_mcmc,scenarios=c(1,2), weighting=c(3,1))
#' data <- biomassplot_prep_SS(ss_mle,ss_mcmc_ens)
#' biomassplot(data)
mcmc_ensemble_SS <- function(ss_mcmc,
                             scenarios = NULL,
                             weighting = NULL) {
  if (check_scenarios(ss_mcmc,"SS","MCMC")=="single scenario"){ss_mcmc <- list(ss_mcmc)}
  if (missing(scenarios)) {scenarios <- 1:length(ss_mcmc)}

  # If no weighting provided, assume all scenarios bear equal weight
  if (missing(weighting)) {weighting = rep(1,length(scenarios))}

  # Check weighting is all integer
  if (!all(weighting==as.integer(weighting))) {
    warning("You have entered a non-intger value in the weighting vector. Please adjust weighting input to be a vector of integers.")
  }

  # Check the parameters are the same. Only do ensemble for matching parameters.
  names <- list()
  for (i in 1:length(scenarios)) {
    names[[i]]<- names(ss_mcmc[[scenarios[i]]])
  }
  common_parameters <- Reduce(intersect,names)

  # Create vector for weighting
  weight_repeats <- rep(scenarios, times = weighting)

  # Transform MCMC list to data frame
  ss_mcmc_subset_common <- data.frame()
  for (i in weight_repeats) {
    tmp <- ss_mcmc[[i]] |>
      dplyr::select(common_parameters)
    ss_mcmc_subset_common <- rbind(ss_mcmc_subset_common,tmp)
    rm(tmp)
  }

  data <- list(ensemble = ss_mcmc_subset_common)
  return(data)
}


