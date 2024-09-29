# Copyright 2024 Fisheries Queensland

# This file is part of SSAND.
# SSAND is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
# SSAND is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
# You should have received a copy of the GNU General Public License along with SSAND. If not, see <https://www.gnu.org/licenses/>.

#' Prepare DDUST data for mcmc_finalbiomassposteriorplot
#'
#' @param dd_mle A list of outputs from DDUST::makefullreport() with one list element per scenario. Will automatically reformat as a list if a single DDUST::makefullreport() output (i.e. one scenario) is entered
#' @param dd_mcmc A list of model fits from tmbstan::tmbstan() with one list element per scenario. Only needed if MCMC was used.
#' @param dd_sim A list of outputs from SSAND::simulate_DD() with one list element per scenario. Only required if MCMC was used. Will automatically reformat as a list if a single DDUST::makefullreport() output (i.e. one scenario) is entered
#' @param credible_interval The credible interval to use to create dashed lines in the final plot
#' @param end_year The final year of biomass to be plotted. Calculated by default, but option is included in case model is run into the future.
#' @param cut Set to TRUE to limit the modeled density to the range of biomass estimates produced by the MCMC. If FALSE, the plot may place density outside of the range of biomass values estimated by the MCMC.
#'
#' @return A list of 5 objects
#' @export
#'
#' @examples
#' \dontrun{
#' # MCMC model
#' library(DDUST)
#' dd_sim <- simulate_DDUST(dd_mle,dd_mcmc)
#' data <- mcmc_finalbiomassposterior_prep_DD(dd_mle,dd_mcmc,
#'                                               dd_sim,scenarios=1)
#' mcmc_finalbiomassposteriorplot(data)
#'
#' # Ensemble model:
#' library(DDUST)
#' dd_sim <- simulate_DDUST(dd_mle,dd_mcmc)
#'
#' dd_mcmc_ens <- mcmc_ensemble_DD(dd_mcmc,dd_sim,scenarios=c(1,2))$dd_mcmc
#' dd_sim_ens <- mcmc_ensemble_DD(dd_mcmc,dd_sim,scenarios=c(1,2))$dd_sim
#' data <- mcmc_finalbiomassposterior_prep_DD(dd_mle,dd_mcmc_ens,
#'                                               dd_sim_ens,scenarios=1)
#' mcmc_finalbiomassposteriorplot(data)
#' }
mcmc_finalbiomassposterior_prep_DD <- function(dd_mle,
                                               dd_mcmc,
                                               dd_sim,
                                               credible_interval = 0.95,
                                               end_year = NULL,
                                               cut = TRUE) {

  if (check_scenarios(dd_mle,"DD","MLE")=="single scenario"){dd_mle <- list(dd_mle)}
  if (check_scenarios(dd_mcmc,"DD","MCMC")=="single scenario"){dd_mcmc <- list(dd_mcmc)}
  if (check_scenarios(dd_sim,"DD","SIM")=="single scenario"){dd_sim <- list(dd_sim)}


  if (missing(end_year)) {end_year <- dd_mle[[1]]$data$last_year_catch}

  data <- mcmc_posteriordensityplot_prep_DD(dd_mle,
                                            dd_mcmc,
                                            dd_sim)[[1]] |>
    dplyr::filter(parameter == paste0("B[",end_year,"]"))


  if (cut) {
    density <- data.frame(x=density(data$value, from=min(data$value), to=max(data$value))$x*100,
                          y=density(data$value, from=min(data$value), to=max(data$value))$y)
  } else {
    density <- data.frame(x=density(data$value)$x*100,
                          y=density(data$value)$y)
  }

  quant_upper <- unname(quantile(data$value, probs=c(1-(1-credible_interval)/2))*100)
  quant_lower <- unname(quantile(data$value, probs=c((1-credible_interval)/2))*100)
  median <- unname(quantile(data$value, probs=c(0.5))*100)

  risk <- c(round(dim(data |> dplyr::filter(value<=0.2))[1] / dim(data)[1] * 100),
            round(dim(data |> dplyr::filter(value>=0.2 & value<=0.4))[1] / dim(data)[1] * 100),
            round(dim(data |> dplyr::filter(value>=0.4 & value<=0.6))[1] / dim(data)[1] * 100),
            round(dim(data |> dplyr::filter(value>=0.6))[1] / dim(data)[1] * 100)
  )

  data <- list(
    density = density,
    quant_lower = quant_lower,
    quant_upper = quant_upper,
    median = median,
    risk = risk,
    end_year = end_year,
    credible_interval = credible_interval
  )
  return(data)
}
