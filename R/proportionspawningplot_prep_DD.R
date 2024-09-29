# Copyright 2024 Fisheries Queensland

# This file is part of SSAND.
# SSAND is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
# SSAND is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
# You should have received a copy of the GNU General Public License along with SSAND. If not, see <https://www.gnu.org/licenses/>.

#' Prepare DDUST data for proportion spawning plot
#'
#' @param dd_mle A list of outputs from DDUST::makefullreport() with one list element per scenario. Will automatically reformat as a list if a single DDUST::makefullreport() output (i.e. one scenario) is entered
#' @param dd_mcmc A list of model fits from tmbstan::tmbstan() with one list element per scenario. Only needed if MCMC was used.
#' @param scenarios A vector of scenarios to plot (numeric). Shows all scenarios if left blank. Can be overridden in the plotting function.
#'
#' @return A data frame with month (factor), value (num), scenario (factor)
#' @export
#'
#' @examples
#' data <- proportionspawningplot_prep_DD(dd_mle)
#' proportionspawningplot(data)
proportionspawningplot_prep_DD <- function(dd_mle,
                                           dd_mcmc = NULL,
                                           scenarios = NULL
                                           ) {

  if (missing(dd_mcmc)) {MCMC = FALSE} else {MCMC = TRUE}

  if (check_scenarios(dd_mle,"DD","MLE")=="single scenario"){dd_mle <- list(dd_mle)}
  if (MCMC && check_scenarios(dd_mcmc,"DD","MCMC")=="single scenario"){dd_mcmc <- list(dd_mcmc)}

  if (missing(scenarios)){scenarios <- 1:length(dd_mle)}

  if (!MCMC) {
    data <- data.frame()
    for (scenario in scenarios) {
      proportion_spawning <- dd_mle[[scenario]]$data$proportion_spawning

      proportion_spawningdata <- data.frame(month = factor(month.abb, levels = c(month.abb)),
                           value = proportion_spawning,
                           scenario = as.factor(scenario))
      data <- rbind(data, proportion_spawningdata)
    }
  }

  if (MCMC) {
    warning("No functionality built for DDUST MCMC proportion spawning pattern plot yet")
  }
  rownames(data) <- NULL
  return(data)
}
