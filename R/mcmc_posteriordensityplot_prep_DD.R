# Copyright 2024 Fisheries Queensland

# This file is part of SSAND.
# SSAND is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
# SSAND is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
# You should have received a copy of the GNU General Public License along with SSAND. If not, see <https://www.gnu.org/licenses/>.

#' Posterior DDUST density plot for MCMC run
#'
#' @param dd_mle A list of outputs from DDUST::makefullreport() with one list element per scenario. Will automatically reformat as a list if a single DDUST::makefullreport() output (i.e. one scenario) is entered
#' @param dd_mcmc A list of model fits from tmbstan::tmbstan() with one list element per scenario. Only needed if MCMC was used.
#' @param dd_sim A list of outputs from SSAND::simulate_DD() with one list element per scenario. Only required if MCMC was used. Will automatically reformat as a list if a single DDUST::makefullreport() output (i.e. one scenario) is entered
#' @param scenario A single number representing the scenario to plot (numeric). Shows first scenario if left blank. Can be overridden in the plotting function.
#' @param other_quantities Other derived quantities from dd_mle to plot, see names(dd_mle) for options
#'
#' @return Posterior density plot
#' @export
#'
#' @examples
#' \dontrun{
#' library(DDUST)
#' dd_sim <- simulate_DDUST(dd_mle,dd_mcmc)
#' data <- mcmc_posteriordensityplot_prep_DD(dd_mle, dd_mcmc, dd_sim, scenario = 1)
#' mcmc_posteriordensityplot(data)
#' }
mcmc_posteriordensityplot_prep_DD <- function(dd_mle,
                                              dd_mcmc,
                                              dd_sim,
                                              scenario = 1,
                                              other_quantities=c()) {

  if (check_scenarios(dd_mle,"DD","MLE")=="single scenario"){dd_mle <- list(dd_mle)}
  if (check_scenarios(dd_mcmc,"DD","MCMC")=="single scenario"){dd_mcmc <- list(dd_mcmc)}
  if (check_scenarios(dd_sim,"DD","SIM")=="single scenario"){dd_sim <- list(dd_sim)}

  if (length(scenario)> 1) {warning("Please enter only one scenario.")}

  pos <- mcmc_median_position_DD(dd_mle,dd_mcmc,dd_sim)[scenario]

  dd_mle <- dd_mle[[scenario]]

  dd_mcmc <- dd_mcmc[[scenario]]

  sim <- dd_sim[[scenario]]

  if (missing(dd_sim)){
    stop('Please provide a MCMC simulation: \n dd_sim <- SSAND::simulate_DD(dd_mle, dd_mcmc)')
  }

  model <- dd_mle$model

  if (check_scenarios(dd_mcmc,"DD","MCMC")=="ensemble") {
    fit_matrix <- dd_mcmc
    numChains <- 1
  } else {
    fit_matrix <- as.matrix(dd_mcmc)
    numChains <- dd_mcmc@sim$chains
  }

  nyears <- dd_mle$data$last_year_catch-dd_mle$data$first_year_catch+1

  # Extract dd_sim quantities of interest
  B_final <- sapply(1:nrow(fit_matrix), function(i){sim[[i]]$B_annual_ratio[nyears]})
  penLL2 <- sapply(1:nrow(fit_matrix),function(i){sim[[i]]$penLL2})

  # create data frame with all outputs for plotting
  data <- data.frame(fit_matrix) |>
    dplyr::select(!dplyr::contains('log_R_star')) |>
    dplyr::select(!dplyr::contains('zeta'))

  parameters <- setdiff(colnames(fit_matrix),c("lp__",paste0('log_R_star[',1:100,']'), paste0('log_R_star.',1:100,'.'), paste0('zeta[',1:100,']')))

  for (par in parameters){
    if (!par %in% names(model$par)){ # add in parameter if it was fixed
      data[[par]] <- as.numeric(dd_mle$parameters[par])
    }
  }

  for (val in other_quantities){
    data[[val]] <- as.numeric(sapply(1:nrow(fit_matrix),function(i){sim[[i]][val]}))
  }

  # get optimised and median trajectory values
  opt_df <- data.frame(parameter = character(), value = numeric())
  med_traj_df <- data.frame(parameter = character(), value = numeric())


  for (par in parameters){
    opt_df <- rbind(opt_df,data.frame(parameter = par,value = dd_mle[[par]]))
    if (par %in% parameters) {
      med_traj_df <- rbind(med_traj_df,data.frame(parameter = par,value = fit_matrix[pos,][par][[1]]))
    } else {
      value = sim[[pos]][par]
      med_traj_df <- rbind(med_traj_df,data.frame(parameter = par,value = unlist(sim[[pos]][par])))
    }
  }

  # B_final
  max_year <- dd_mle$data$last_year_catch

  opt_df <- rbind(opt_df,data.frame(parameter = paste0('B_',max_year), value = dplyr::last(dd_mle$B_annual_ratio)))
  med_traj_df <- rbind(med_traj_df,data.frame(parameter = paste0('B_',max_year), value = dplyr::last(sim[[pos]]$B_annual_ratio)))
  # LL
  opt_df <- rbind(opt_df, data.frame(parameter = 'LL', value = -dd_mle$LL))
  med_traj_df <- rbind(med_traj_df,data.frame(parameter = 'LL', value = -dplyr::last(sim[[pos]]$LL)))
  # h
  if ('xi' %in% unique(opt_df$parameter)){
    opt_df <- rbind(opt_df,data.frame(parameter = 'h',
                                      value = (1+exp(opt_df$value[opt_df$parameter=='xi']))/(5+exp(opt_df$value[opt_df$parameter=='xi']))))
  }
  if ('xi' %in% unique(med_traj_df$parameter)){
    med_traj_df<- rbind(med_traj_df,data.frame(parameter = 'h',
                                               value = (1+exp(med_traj_df$value[med_traj_df$parameter=='xi']))/(5+exp(med_traj_df$value[med_traj_df$parameter=='xi']))))
  }
  med_traj_df <- med_traj_df |> dplyr::mutate(parameter = dplyr::case_when(parameter=='Rinit' ~ 'R[0]',
                                                                           parameter=='k' ~ 'kappa',
                                                                           parameter=='q1' ~ 'q[1]',
                                                                           parameter=='q2' ~ 'q[2]',
                                                                           parameter=='lsigmaR_sq' ~ 'log(sigma[R]^2)',
                                                                           parameter=='lsigmaI_sq' ~ 'log(sigma[I]^2)',
                                                                           parameter== paste0('B_',max_year) ~ paste0('B[',max_year,']'),
                                                                           parameter=='penLL1' ~ 'penalty[1]',
                                                                           parameter=='penLL2' ~ 'penalty[2]',
                                                                           TRUE ~ parameter))
  opt_df <- opt_df |> dplyr::mutate(parameter = dplyr::case_when(parameter=='Rinit' ~ 'R[0]',
                                                                 parameter=='k' ~ 'kappa',
                                                                 parameter=='q1' ~ 'q[1]',
                                                                 parameter=='q2' ~ 'q[2]',
                                                                 parameter=='lsigmaR_sq' ~ 'log(sigma[R]^2)',
                                                                 parameter=='lsigmaI_sq' ~ 'log(sigma[I]^2)',
                                                                 parameter== paste0('B_',max_year) ~ paste0('B[',max_year,']'),
                                                                 parameter=='penLL1' ~ 'penalty[1]',
                                                                 parameter=='penLL2' ~ 'penalty[2]',
                                                                 TRUE ~ parameter))

  if ('xi' %in% names(data)){
    mcmc_df_trace = data |>
      dplyr::mutate(h = (1+exp(xi))/(5+exp(xi)), "B_{max_year}" := B_final) |>
      dplyr::mutate(chain = sort(rep(1:numChains,nrow(fit_matrix)/numChains)),
                    iter = rep(1:(nrow(fit_matrix)/numChains), numChains)) |>
      tidyr::pivot_longer(!c('iter','chain'),names_to = "parameter", values_to = "value") |>
      dplyr::mutate(chain = as.factor(chain))
  } else {
    mcmc_df_trace = data |>
      dplyr::mutate("B_{max_year}" :=  B_final) |>
      dplyr::mutate(chain = sort(rep(1:numChains,nrow(fit_matrix)/numChains)),
                    iter = rep(1:(nrow(fit_matrix)/numChains), numChains)) |>
      tidyr::pivot_longer(!c('iter','chain'),names_to = "parameter", values_to = "value") |>
      dplyr::mutate(chain = as.factor(chain))
  }

  # median of each parameter
  med_par_df <- mcmc_df_trace |>
    dplyr::group_by(parameter) |>
    dplyr::summarise(value=median(value)) |>
    dplyr::mutate(parameter = dplyr::case_when(parameter=='Rinit' ~ 'R[0]',
                                               parameter=='k' ~ 'kappa',
                                               parameter=='q1' ~ 'q[1]',
                                               parameter=='q2' ~ 'q[2]',
                                               parameter=='lsigmaR_sq' ~ 'log(sigma[R]^2)',
                                               parameter=='lsigmaI_sq' ~ 'log(sigma[I]^2)',
                                               parameter== paste0('B_',max_year) ~ paste0('B[',max_year,']'),
                                               parameter=='penLL1' ~ 'penalty[1]',
                                               parameter=='penLL2' ~ 'penalty[2]',
                                               parameter=='lp__' ~ 'LL',
                                               TRUE ~ parameter))

  mcmc_df_trace <- mcmc_df_trace |> dplyr::mutate(parameter = dplyr::case_when(parameter=='Rinit' ~ 'R[0]',
                                                                               parameter=='k' ~ 'kappa',
                                                                               parameter=='q1' ~ 'q[1]',
                                                                               parameter=='q2' ~ 'q[2]',
                                                                               parameter=='lsigmaR_sq' ~ 'log(sigma[R]^2)',
                                                                               parameter=='lsigmaI_sq' ~ 'log(sigma[I]^2)',
                                                                               parameter== paste0('B_',max_year) ~ paste0('B[',max_year,']'),
                                                                               parameter=='penLL1' ~ 'penalty[1]',
                                                                               parameter=='penLL2' ~ 'penalty[2]',
                                                                               parameter=='lp__' ~ 'LL',
                                                                               TRUE ~ parameter))

  rownames(mcmc_df_trace) <- NULL
  rownames(opt_df) <- NULL
  rownames(med_par_df) <- NULL
  rownames(med_traj_df) <- NULL
  data <- list(mcmc_df_trace,opt_df,med_par_df,med_traj_df)
  return(data)
}

