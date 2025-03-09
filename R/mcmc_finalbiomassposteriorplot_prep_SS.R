# Copyright 2024 Fisheries Queensland

# This file is part of SSAND.
# SSAND is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
# SSAND is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
# You should have received a copy of the GNU General Public License along with SSAND. If not, see <https://www.gnu.org/licenses/>.

#' Prepare Stock Synthesis data for mcmc_finalbiomassposteriorplot
#'
#' @param ss_mle A list of outputs from r4ss::SS_output() with one element per scenario, or a single representative scenario. Will automatically reformat as a list if a single r4ss::SS_output() output (i.e. one scenario) is entered.
#' @param ss_mcmc Output of mcmc_ensemble_SS(). A list containing one dataframe of all specified ss_mcmc objects combined, limited to columns that are common between ss_mcmc objects, duplicated as per the specified weighting.
#' An extra column "ensemble" is appended, for data checks in other functions.
#' @param credible_interval The credible interval to use to create dashed lines in the final plot
#' @param end_year The final year of biomass to be plotted. Calculated by default, but option is included in case model is run into the future.
#' @param cut Set to TRUE to limit the modeled density to the range of biomass estimates produced by the MCMC. If FALSE, the plot may place density outside of the range of biomass values estimated by the MCMC.
#'
#' @return A list of 5 objects
#' @export
#'
#' @examples
#' # MCMC model
#' data <- mcmc_finalbiomassposterior_prep_SS(ss_mle,ss_mcmc)
#' mcmc_finalbiomassposteriorplot(data)
#'
#' # Ensemble model
#' ss_mcmc_ens <- mcmc_ensemble_SS(ss_mcmc)
#' data <- mcmc_finalbiomassposterior_prep_SS(ss_mle,ss_mcmc_ens)
#' mcmc_finalbiomassposteriorplot(data)
mcmc_finalbiomassposterior_prep_SS <- function(ss_mle,
                                               ss_mcmc,
                                               credible_interval = 0.95,
                                               end_year = NULL,
                                               cut = TRUE) {

  if (check_scenarios(ss_mle,"SS","MLE")=="single scenario"){ss_mle <- list(ss_mle)}
  if (check_scenarios(ss_mcmc,"SS","MCMC")=="single scenario"){ss_mcmc <- list(ss_mcmc)}

  if (length(ss_mle)>1) {ss_mle <- list(ss_mle[[1]])}
  if (length(ss_mcmc)>1) {ss_mcmc <- list(ss_mcmc[[1]])}

  if (missing(end_year)) {end_year <- ss_mle[[1]]$endyr +1}

  data <- mcmc_posteriordensityplot_prep_SS(ss_mle[[1]],
                                            ss_mcmc,
                                            parameters = paste0("Bratio_",end_year),
                                            scenario = 1)[[1]]

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
  mode <- density$x[which.max(density$y)]

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
    mode = mode,
    risk = risk,
    end_year = end_year,
    credible_interval = credible_interval
  )
  return(data)
}
