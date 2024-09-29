# Copyright 2024 Fisheries Queensland

# This file is part of SSAND.
# SSAND is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
# SSAND is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
# You should have received a copy of the GNU General Public License along with SSAND. If not, see <https://www.gnu.org/licenses/>.

#' Prepare Stock Synthesis data for parameter summary table
#'
#' @param ss_mle A list of outputs from r4ss::SS_output() with one element per scenario. Will automatically reformat as a list if a single r4ss::SS_output() output (i.e. one scenario) is entered.
#' @param ss_mcmc A list of outputs from r4ss::SSgetMCMC() with one element per scenario. Only needed if MCMC was used. Will automatically reformat as a list if a single r4ss::SSgetMCMC() output (i.e. one scenario) is entered.
#' @param scenario The scenario to display (numeric). Shows scenario 1 if left blank. Can be overridden in the plotting function.
#' @param parameters A vector of parameters to include in tablet (character). Use SSAND::extract_SS_parameters() to find a list of parameter names used.
#' @param fields For MLE, a vector of column headers to include in the table (e.g. Value, Phase, Min, Max etc)
#'
#' @return A data frame that summarised parameter estimate, ready for use in parametertable()
#' @export
#'
#' @examples
#' # \rowcolors{2}{white}{light-gray}
#' # <<table_mcmc1, results='asis', echo=FALSE>>=
#' parameters <- extract_SS_parameters(ss_mle)[2:4,]
#' data <- parametertable_prep_SS(ss_mle=ss_mle, parameters=parameters, scenario=1)
#' parametertable(data, label="tab:param")
#' # @
#'
#' # \rowcolors{2}{white}{light-gray}
#' # <<table_mcmc1, results='asis', echo=FALSE>>=
#' parameters <- extract_SS_parameters(ss_mle)[2:4,]
#' data <- parametertable_prep_SS(ss_mcmc=ss_mcmc, parameters=parameters, scenario=1)
#' parametertable(data, label="tab:param")
#' # @
#'
#' # \rowcolors{2}{white}{light-gray}
#' # <<table_mcmc1, results='asis', echo=FALSE>>=
#' ss_mcmc_ens <- mcmc_ensemble_SS(ss_mcmc,scenarios=c(1,2))
#' parameters <- extract_SS_parameters(ss_mle)[2:4,]
#' data <- parametertable_prep_SS(ss_mcmc=ss_mcmc_ens, parameters=parameters, scenario=1)
#' parametertable(data, label="tab:param")
#' # @
parametertable_prep_SS <- function(ss_mle = NULL,
                                   ss_mcmc = NULL,
                                   scenario = 1,
                                   parameters,
                                   fields = c("Label", "Value", "Phase", "Min", "Max", "Init", "Parm_StDev")
) {
  if (!missing(ss_mle) & !missing(ss_mcmc)) {warning("Please only enter MLE or MCMC data, not both.")}

  if (!missing(ss_mle) && check_scenarios(ss_mle,"SS","MLE")=="single scenario"){ss_mle <- list(ss_mle)}
  if (!missing(ss_mcmc) && check_scenarios(ss_mcmc,"SS","MCMC")=="single scenario"){ss_mcmc <- list(ss_mcmc)}


  if (!missing(ss_mle)) {
    data <- ss_mle[[scenario]]$parameters |>
      dplyr::filter(Label %in% parameters) |>
      dplyr::select(dplyr::all_of(fields)) |>
      `rownames<-`(NULL) |>
      dplyr::rename("Symbol" = "Label")

    colnames(data) <- gsub("_", "", colnames(data))
  }

  if (!missing(ss_mcmc)) {
    data <- ss_mcmc[[scenario]] |>
      dplyr::select(dplyr::all_of(parameters)) |>
      apply(2,quantile, na.rm = TRUE, probs =c(0.5,0.025,0.975)) |>
      t() |>
      as.data.frame() |>
      tibble::rownames_to_column() |>
      `colnames<-`(c("Symbol", "MCMC median", "MCMC 2.5\\%", "MCMC 97.5\\%"))
  }
  return(data)
}
