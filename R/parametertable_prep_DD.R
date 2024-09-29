# Copyright 2024 Fisheries Queensland

# This file is part of SSAND.
# SSAND is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
# SSAND is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
# You should have received a copy of the GNU General Public License along with SSAND. If not, see <https://www.gnu.org/licenses/>.

#' Prepare DDUST data for parameter summary table
#'
#' @param dd_mle A list of outputs from DDUST::makefullreport() with one list element per scenario. Will automatically reformat as a list if a single DDUST::makefullreport() output (i.e. one scenario) is entered
#' @param dd_mcmc A list of model fits from tmbstan::tmbstan() with one list element per scenario. Only needed if MCMC was used.
#' @param scenario The scenario to display (numeric). Shows scenario 1 if left blank. Can be overridden in the plotting function.
#' @param parameters A vector of parameters to include on plot (character). See names(dd_mle) for options.
#'
#' @return A data frame that summarises parameter estimate, ready for use in parametertable()
#' @export
#'
#' @examples
#' # \rowcolors{2}{white}{light-gray}
#' # <<table_mcmc1, results='asis', echo=FALSE>>=
#' data <- parametertable_prep_DD(dd_mle=dd_mle)
#' parametertable(data, label="tab:param")
#' # @
#'
#' \dontrun{
#' library(DDUST)
#' dd_sim <- simulate_DDUST(dd_mle,dd_mcmc)
#' # \rowcolors{2}{white}{light-gray}
#' # <<table_mcmc1, results='asis', echo=FALSE>>=
#' data <- parametertable_prep_DD(dd_mcmc=dd_mcmc)
#' parametertable(data, label="tab:param")
#' # @
#'
#' # \rowcolors{2}{white}{light-gray}
#' # <<table_mcmc1, results='asis', echo=FALSE>>=
#' dd_mcmc_ens <- mcmc_ensemble_DD(dd_mcmc,dd_sim,scenarios=c(1,2))$dd_mcmc
#' data <- parametertable_prep_DD(dd_mcmc=dd_mcmc_ens)
#' parametertable(data, label="tab:param")
#' # @
#' }
parametertable_prep_DD <- function(dd_mle = NULL,
                                   dd_mcmc = NULL,
                                   scenario = 1,
                                   parameters = NULL
) {
  if (!missing(dd_mle) & !missing(dd_mcmc)) {warning("Please only enter MLE or MCMC data, not both.")}

  if (!missing(dd_mle) && check_scenarios(dd_mle,"DD","MLE")=="single scenario"){dd_mle <- list(dd_mle)}
  if (!missing(dd_mcmc) && check_scenarios(dd_mcmc,"DD","MCMC")=="single scenario"){dd_mcmc <- list(dd_mcmc)}

  if (!missing(dd_mle)) {

    if (missing(parameters)) {parameters <- sapply(dd_mle, function(x) {x$parameters})}
    name <- setdiff(rownames(parameters),c("log_R_star","zeta"))

    data <- data.frame(scenario = integer(), name = character(), value = numeric(), ub = numeric(), lb = numeric())

    for (i in 1:ncol(parameters)){
      sd <- unlist(sapply(1:length(name), function(j) {dd_mle[[i]][paste0(name[j],'_sd')]}))
      value <- unlist(sapply(1:length(name), function(j) {dd_mle[[i]][name[j]]}))

      data <- rbind(data, data.frame(scenario = rep(i,length(name)),
                                     name = name,
                                     value = value,
                                     sd = sd,
                                     ub = value + 1.96*sd,
                                     lb = value - 1.96*sd
      ))
      if ('xi' %in% name){
        xi_vector <- data[which(data$name %in% 'xi' & data$scenario == i),]

        ub = (1 + exp(xi_vector$ub))/(5 + exp(xi_vector$ub))
        value = (1 + exp(xi_vector$value))/(5 + exp(xi_vector$value))
        sd = (ub - value)/1.96

        data <- rbind(data, data.frame(scenario = i,
                                       name = 'h',
                                       value = (1 + exp(xi_vector$value))/(5 + exp(xi_vector$value)),
                                       sd = sd,
                                       ub = (1 + exp(xi_vector$ub))/(5 + exp(xi_vector$ub)),
                                       lb = (1 + exp(xi_vector$lb))/(5 + exp(xi_vector$lb))
        ))
      }
    }

    scenario_val <- scenario

    data <- data |>
      dplyr::filter(scenario == scenario_val) |>
      dplyr::filter(!sd==0) |>
      dplyr::mutate(sd = round(sd,3)) |>
      dplyr::select(Symbol = name, Estimate = value, `Standard deviation`=sd) |>
      `rownames<-`(NULL)
  }

  if (!missing(dd_mcmc)) {

    data <- data.frame(scenario = integer(), name = character(), value = numeric(), ub = numeric(), lb = numeric())

      name <- setdiff(names(dd_mcmc[[scenario]]),c("lp__",paste0('log_R_star[',1:100,']'), paste0('log_R_star.',1:100,'.'), paste0('zeta[',1:100,']')))
      par <- data.frame(as.matrix(dd_mcmc[[scenario]]))

    # calculate credible interval
    data <- apply(par,2,quantile, na.rm = TRUE, probs = c(0.025, 0.5, 0.975)) |>
      t() |>
      as.data.frame()  |>
      tibble::rownames_to_column() |>
      dplyr::filter(rowname %in% name) |>
      `colnames<-`(c("Symbol", "MCMC median", "MCMC 2.5\\%", "MCMC 97.5\\%"))
  }
  return(data)
}
