# Copyright 2024 Fisheries Queensland

# This file is part of SSAND.
# SSAND is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
# SSAND is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
# You should have received a copy of the GNU General Public License along with SSAND. If not, see <https://www.gnu.org/licenses/>.

#' Extract Stock Synthesis MCMC iteration position that produces median final biomass
#'
#' @param ss_mle A list of outputs from r4ss::SS_output() with one element per scenario. Will automatically reformat as a list if a single r4ss::SS_output() output (i.e. one scenario) is entered.
#' @param ss_mcmc A list of outputs from r4ss::SSgetMCMC() with one element per scenario. Only needed if MCMC was used. Will automatically reformat as a list if a single r4ss::SSgetMCMC() output (i.e. one scenario) is entered.
#' @param scenarios A vector of scenario numbers to be be included in median calculation (numeric). Function will return one median for each scenario.
#'
#' @return A vector of numbers with one entry per scenario entered, where each number is the corresponding MCMC run that produced the median final biomass ratio.
#' @export
#'
#' @examples
#' pos <- mcmc_median_position_SS(ss_mle, ss_mcmc)
#'
#' # Ensemble model
#' ss_mcmc_ens <- mcmc_ensemble_SS(ss_mcmc)
#' pos <- mcmc_median_position_SS(ss_mle, ss_mcmc_ens)
mcmc_median_position_SS <- function(ss_mle,
                                    ss_mcmc,
                                    scenarios = NULL){


  if (missing(scenarios)){scenarios <- 1:length(ss_mcmc)}

  if (check_scenarios(ss_mle,"SS","MLE")=="single scenario"){ss_mle <- list(ss_mle)}
  if (check_scenarios(ss_mcmc,"SS","MCMC")=="single scenario"){ss_mcmc <- list(ss_mcmc)}

  data <- c()
  for (scenario in scenarios) {

    startyr <- ss_mle[[scenario]]$startyr
    endyr <- ss_mle[[scenario]]$endyr
    years <- startyr:endyr

    tmpdata <- ss_mcmc[[scenario]] |>
      dplyr::select(tidyselect::contains("Bratio_"))

    B_final <- tmpdata[,dim(tmpdata)[2]]

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
