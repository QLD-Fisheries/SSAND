# Copyright 2024 Fisheries Queensland

# This file is part of SSAND.
# SSAND is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
# SSAND is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
# You should have received a copy of the GNU General Public License along with SSAND. If not, see <https://www.gnu.org/licenses/>.

#' Prepare DDUST data frame for correlationplot
#'
#' @param dd_sdr The SD report from a model run. Extracted from model output using TMB::sdreport() as per the example.
#' @param dd_mle A list of outputs from DDUST::makefullreport() with one list element per scenario. Will automatically reformat as a list if a single DDUST::makefullreport() output (i.e. one scenario) is entered
#' @param dd_mcmc A list of model fits from tmbstan::tmbstan() with one list element per scenario. Only needed if MCMC was used.
#' @param dd_sim A list of outputs from SSAND::simulate_DD() with one list element per scenario. Only required if MCMC was used. Will automatically reformat as a list if a single DDUST::makefullreport() output (i.e. one scenario) is entered
#' @param other_quantities A vector of other derived quantities to display on plot (character).
#' @param scenario The scenarios to plot (numeric). Shows scenario 1 if left blank.
#'
#' @return A data frame for correlation plot with X1 (fac), X2 (fac) and value (num)
#' @export
#'
#' @examples
#' \dontrun{
#' library(DDUST)
#' dd_sdr <- TMB::sdreport(dd_mle[[1]]$model)
#' data <- correlationplot_prep_DD(dd_sdr)
#' correlationplot(data)
#'
#' data <- correlationplot_prep_DD(dd_sdr=dd_sdr, dd_mle=dd_mle, dd_mcmc=dd_mcmc, dd_sim=dd_sim)
#' correlationplot(data, sample=10)
#' }
correlationplot_prep_DD <- function(dd_sdr,
                                    dd_mle= NULL,
                                    dd_mcmc = NULL,
                                    dd_sim = NULL,
                                    other_quantities = NULL,
                                    scenario = 1){

  if (missing(dd_mcmc)) {MCMC = FALSE} else {MCMC = TRUE}
  if (check_scenarios(dd_mle,"DD","MLE")=="single scenario"){dd_mle <- list(dd_mle)}
  if (MCMC && check_scenarios(dd_mcmc,"DD","MCMC")=="single scenario"){dd_mcmc <- list(dd_mcmc)}
  if (MCMC && check_scenarios(dd_sim,"DD","SIM")=="single scenario"){dd_sim <- list(dd_sim)}

  if (!MCMC) {

    covariancematrix <- dd_sdr$cov.fixed

    V <- covariancematrix
    H <- cov2cor(V)
    H[upper.tri(H,diag = TRUE)] <- NA

    data <- H |> as.data.frame()
    colnames(data) <- rownames(data) # need unique column names

    data <- data |>
      tibble::rownames_to_column("X1") |>
      dplyr::mutate(X1 = factor(X1, levels = X1)) |>
      tidyr::pivot_longer(-X1, names_to = "X2", values_to = "value")

    data <- data |>
      dplyr::mutate(X2 = factor(X2, levels = levels(data$X1))) |>
      dplyr::mutate(MCMC = FALSE)
  }

  if (MCMC) {
    model <- dd_mle[[scenario]]$model
    fit_matrix <- as.matrix(dd_mcmc[[scenario]])
    nyears <- dd_mle[[scenario]]$data$last_year_catch-dd_mle[[scenario]]$data$first_year_catch+1
    numChains <- dd_mcmc[[scenario]]@sim$chains

    # Extract derived quantities of interest
    sim <- dd_sim[[scenario]]


    B_final <- sapply(1:nrow(fit_matrix), function(i){sim[[i]]$B_annual_ratio[nyears]})
    pos <- mcmc_median_position_SS(ss_mle,ss_mcmc)[scenario]

    penLL2 <- sapply(1:nrow(fit_matrix),function(i){sim[[i]]$penLL2})

    # create data frame with all outputs for plotting
    data <- data.frame(fit_matrix) |>
      dplyr::select(!contains('log_R_star')) |>
      dplyr::select(!contains('zeta')) |>
      dplyr::select(!"lp__")

    parameters <- setdiff(colnames(fit_matrix),c('lp__', paste0('log_R_star[',1:100,']'), paste0('zeta[',1:100,']')))

    for (par in parameters){
      if (!par %in% names(model$par)){ # add in parameter if it was fixed
        data[[par]] <- as.numeric(dd_mle[[scenario]]$parameters[par])
      }
    }

    for (val in other_quantities){
      data[[val]] <- as.numeric(sapply(1:nrow(fit_matrix),function(i){sim[[i]][val]}))
    }

    if ('xi' %in% names(data)){
      mcmc_df_trace = data |>
        dplyr::mutate(h = (1+exp(xi))/(5+exp(xi)), B_final = B_final) |>
        dplyr::mutate(chain = sort(rep(1:numChains,nrow(fit_matrix)/numChains)),
                      iter = rep(1:(nrow(fit_matrix)/numChains), numChains)) |>
        tidyr::pivot_longer(!c('iter','chain'),names_to = "parameter", values_to = "value") |>
        dplyr::mutate(chain = as.factor(chain))
    } else {
      mcmc_df_trace = data |>
        dplyr::mutate(B_final = B_final) |>
        dplyr::mutate(chain = sort(rep(1:numChains,nrow(fit_matrix)/numChains)),
                      iter = rep(1:(nrow(fit_matrix)/numChains), numChains)) |>
        tidyr::pivot_longer(!c('iter','chain'),names_to = "parameter", values_to = "value") |>
        dplyr::mutate(chain = as.factor(chain))
    }

    mcmc_df_trace <- mcmc_df_trace |>
      dplyr::mutate(parameter = dplyr::case_when(parameter=='Rinit' ~ 'R[init]',
                                                 parameter=='k' ~ 'kappa',
                                                 parameter=='q1' ~ 'q[1]',
                                                 parameter=='q2' ~ 'q[2]',
                                                 parameter=='lsigmaR_sq' ~ 'log(sigma[R]^2)',
                                                 parameter=='lsigmaI_sq' ~ 'log(sigma[I]^2)',
                                                 parameter=='B_final' ~ 'B[final]',
                                                 parameter=='penLL1' ~ 'penalty[1]',
                                                 parameter=='penLL2' ~ 'penalty[2]',
                                                 TRUE ~ parameter))
    data <- mcmc_df_trace |>
      tidyr::pivot_wider(names_from = parameter, values_from=value) |>
      dplyr::mutate(MCMC = TRUE)
  }



  return(data)
}
