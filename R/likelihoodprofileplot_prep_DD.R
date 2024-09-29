# Copyright 2024 Fisheries Queensland

# This file is part of SSAND.
# SSAND is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
# SSAND is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
# You should have received a copy of the GNU General Public License along with SSAND. If not, see <https://www.gnu.org/licenses/>.

#' Prepare DDUST data for likelihoodprofileplot()
#' View ?likelihoodprofileplot to view full examples for running likelihood profiles for Stock Synthesis or DDUST.
#'
#' @param null placeholder
#' @return placeholder
#' @export
#'
#' @examples
#' \dontrun{
#' # Run DDUST model:
#' data <- dd_mle[[1]]$data
#' parameters <- dd_mle[[1]]$parameters
#' map <- dd_mle[[1]]$map
#' dd_out <- DDUST::run_DDUST(data, parameters, map, MCMC = FALSE)
#'
#' # Set up parameter profile:
#' Rinit <- seq(11,12,by = 0.05)
#'
#' # Simulate model over every value of parameter
#' profile <- c()
#' for (i in 1:length(Rinit)){
#'   simulation_parameters <- parameters
#'   for (item in names(map)){
#'     simulation_parameters[item] <- NULL
#'   }
#'   simulation_parameters$Rinit <- Rinit[i]
#'   sim <- dd_out$dd_mle$model$simulate(unlist(simulation_parameters))
#'
#'   profile <- rbind(profile,
#'                    data.frame(x_vector = Rinit[i],
#'                               likelihood = sim$LL, component = 'Total'),
#'                    data.frame(x_vector = Rinit[i],
#'                               likelihood = sim$biomassLL, component = 'Biomass survey'),
#'                    data.frame(x_vector = Rinit[i],
#'                               likelihood = sim$cpueLL, component = 'Index'),
#'                    data.frame(x_vector = Rinit[i],
#'                               likelihood = sim$RecDevLL, component = 'Recruitment'),
#'                    data.frame(x_vector = Rinit[i],
#'                               likelihood = sim$penLL1, component = 'Catch penalty'),
#'                    data.frame(x_vector = Rinit[i],
#'                               likelihood = sim$penLL2, component = 'Recruits penalty'))
#' }
#' likelihoodprofileplot(profile)
#' }
likelihoodprofileplot_prep_DD <- function(null) {
  print("Run ?likelihoodprofileplot_prep_DD in your console for an example of how to set up a likelihood profile for DDUST")
}


