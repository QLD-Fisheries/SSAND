# Copyright 2024 Fisheries Queensland

# This file is part of SSAND.
# SSAND is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
# SSAND is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
# You should have received a copy of the GNU General Public License along with SSAND. If not, see <https://www.gnu.org/licenses/>.

#' Prepare data fro mcmc_rhatplot
#'
#' @param dd_mle A list of outputs from DDUST::makefullreport() with one list element per scenario. Will automatically reformat as a list if a single DDUST::makefullreport() output (i.e. one scenario) is entered
#' @param dd_mcmc A list of model fits from tmbstan::tmbstan() with one list element per scenario. Only needed if MCMC was used.
#' @param scenarios A vector of scenarios to plot (numeric). Shows all scenarios if left blank. Can be overridden in the plotting function.
#'
#' @return A dataframe with variables parameter (numeric), group (factor), scenario (factor)
#' @export
#'
#' @examples
#' \dontrun{
#' library(DDUST)
#' data <- mcmc_rhatplot_prep_DD(dd_mle,dd_mcmc)
#' mcmc_rhatplot(data)
#' }
mcmc_rhatplot_prep_DD <- function(dd_mle,
                                  dd_mcmc,
                                  scenarios = NULL) {

  if (check_scenarios(dd_mle,"DD","MLE")=="single scenario"){dd_mle <- list(dd_mle); warning("Assuming you are entering a single scenario, not a list of scenarios. Wrap dd_mle input inside a list() to avoid this warning.")}
  if (check_scenarios(dd_mcmc,"DD","MCMC")=="single scenario"){dd_mcmc <- list(dd_mcmc); warning("Assuming you are entering a single scenario, not a list of scenarios. Wrap dd_mcmc input inside a list() to avoid this warning.")}

  if (missing(scenarios)){scenarios <- 1:length(dd_mle)}

  rhat_data <- list()

  for (scenario in scenarios){
    # fit <- base::summary(dd_mcmc[[scenario]])[1] |>
    fit <- summary(dd_mcmc[[scenario]])[1] |>
      as.data.frame() |>
      dplyr::select(Rhat = summary.Rhat)

    row_names <- rownames(fit)

    rhat <- fit |>
      dplyr::mutate(parameter=as.character(row_names))

    rownames(rhat) <- NULL

    if(dd_mle[[scenario]]$data$rec_dev_type == "random"){
      rec_dev <- rhat |>
        dplyr::filter(grepl('log_R_star', parameter)) |>
        dplyr::summarise(Rhat=mean(Rhat), parameter='log_R_star', scenario= scenario)

      rhat2 <- rhat |>
        dplyr::mutate(scenario = scenario) |>
        dplyr::filter(!grepl('log_R_star', parameter)) |>
        rbind(rec_dev)
    }else{
      rec_dev <- rhat |> dplyr::filter(grepl('zeta', parameter)) |>
        dplyr::summarise(Rhat=mean(Rhat), parameter='zeta', scenario= scenario)
      rhat2 <- rhat |> dplyr::filter(!grepl('zeta', parameter)) |> rbind(rec_dev)
    }
    rhat_data[[scenario]] <- rhat2
  }
  rhat_data_all <- as.data.frame(do.call(rbind, rhat_data)) |>
    dplyr::mutate(group=dplyr::case_when(Rhat<= 1.05 ~ paste(as.character(expression(R)), as.character(expression("\u2264")), '1.05'),
                                  Rhat > 1.05 & Rhat < 1.1  ~ paste(as.character(expression(R)), as.character(expression("\u2264")), '1.1'),
                                  TRUE ~ paste0(as.character(expression(R)),' > 1.1'))) |>
    dplyr::mutate(scenario=factor(scenario, levels = scenarios)) |>
    dplyr::mutate(parameter = dplyr::case_when(parameter=='Rinit' ~ 'R[init]',
                                        parameter == 'q1' ~ 'q[1]',
                                        parameter == 'q2' ~ 'q[2]',
                                        parameter == 'lsigmaR_sq' ~ 'log(sigma[R]^2))',
                                        parameter == 'lsigmaI_sq' ~ 'log(sigma[I]^2))',
                                        parameter == 'lp__' ~ 'LL',
                                        parameter == 'k' ~ 'kappa',
                                        parameter == 'log_R_star' ~ 'log(R)^*',
                                        parameter == 'zeta' ~ 'zeta',
                                        TRUE ~ parameter)) |>
    dplyr::mutate(group=factor(group, levels=c(paste(as.character(expression(R)), as.character(expression("\u2264")), '1.05'),
                                               paste(as.character(expression(R)), as.character(expression("\u2264")), '1.1'),
                                               paste0(as.character(expression(R)),' > 1.1'))))
  param_names <- rhat_data_all |>
    dplyr::select(parameter) |>
    unique() |>
    `rownames<-`( NULL )

  param_names <- param_names$parameter

  seq <- c(1:length(param_names))

  data <- rhat_data_all |>
    dplyr::mutate(parameter=factor(parameter, levels=param_names)) |>
    dplyr::group_by(scenario) |>
    dplyr::mutate(xmax = max(1.1,max(Rhat))) |>
    dplyr::ungroup() |>
    dplyr::mutate(scenario = as.factor(scenario))

  return(data)
}
