# Copyright 2024 Fisheries Queensland

# This file is part of SSAND.
# SSAND is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
# SSAND is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
# You should have received a copy of the GNU General Public License along with SSAND. If not, see <https://www.gnu.org/licenses/>.

#' Prepare DDUST data for sensitivityplot()
#'
#' @param dd_mle A list of outputs from DDUST::makefullreport() with one list element per scenario. Will automatically reformat as a list if a single DDUST::makefullreport() output (i.e. one scenario) is entered
#' @param dd_mcmc A list of model fits from tmbstan::tmbstan() with one list element per scenario. Only needed if MCMC was used.
#' @param dd_sim A list of outputs from SSAND::simulate_DD() with one list element per scenario. Only required if MCMC was used. Will automatically reformat as a list if a single DDUST::makefullreport() output (i.e. one scenario) is entered
#' @param parameters A vector of parameters to include on plot (character). See names(dd_mle) for options.
#' @param scenarios A list of scenarios to show - shows all scenarios if left blank
#' @param show_B_ratio Set to TRUE to show a panel for final biomass ratio (logical).
#' @param show_B_abs Set to TRUE to show a panel for final absolute biomass (logical).
#' @param show_F_final Set to TRUE to show final annual fishing mortality (logical).
#' @param show_LL Set to TRUE to show negative log likelhood or objective function (logical).
#'
#' @return A data frame with scenario (int), name (fac), value (num), ub (num), lb (num) and fixed (boolean).
#' @export
#'
#' @examples
#' data <- sensitivityplot_prep_DD(dd_mle)
#' sensitivityplot(data)
#'
#' \dontrun{
#' library(DDUST)
#' dd_sim <- simulate_DDUST(dd_mle,dd_mcmc)
#' data <- sensitivityplot_prep_DD(dd_mle, dd_mcmc, dd_sim)
#' sensitivityplot(data)
#' }
sensitivityplot_prep_DD <- function(dd_mle,
                                    dd_mcmc = NULL,
                                    dd_sim = NULL,
                                    scenarios = NULL,
                                    parameters = NULL,
                                    show_B_ratio = TRUE,
                                    show_B_abs = FALSE,
                                    show_F_final = FALSE,
                                    show_LL = TRUE
){

  if (missing(dd_mcmc)) {MCMC = FALSE} else {MCMC = TRUE}

  if (check_scenarios(dd_mle,"DD","MLE")=="single scenario"){dd_mle <- list(dd_mle)}
  if (MCMC && check_scenarios(dd_mcmc,"DD","MCMC")=="single scenario"){dd_mcmc <- list(dd_mcmc)}
  if (MCMC && check_scenarios(dd_sim,"DD","SIM")=="single scenario"){dd_sim <- list(dd_sim)}

  if (missing(scenarios)){scenarios <- 1:length(dd_mle)}

  if (!MCMC) {
    par <- sapply(dd_mle, function(x) {x$parameters})
    name <- setdiff(rownames(par),c("log_R_star","zeta"))

    data <- data.frame(scenario = integer(), name = character(), value = numeric(), ub = numeric(), lb = numeric())

    for (i in 1:ncol(par)){
      sd <- unlist(sapply(1:length(name), function(j) {dd_mle[[i]][paste0(name[j],'_sd')]}))
      value <- unlist(sapply(1:length(name), function(j) {dd_mle[[i]][name[j]]}))

      data <- rbind(data, data.frame(scenario = rep(i,length(name)),
                                     name = name,
                                     value = value,
                                     ub = value + 1.96*sd,
                                     lb = value - 1.96*sd))
      if ('xi' %in% name){
        xi_vector <- data[which(data$name %in% 'xi' & data$scenario == i),]
        data <- rbind(data, data.frame(scenario = i,
                                       name = 'h',
                                       value = (1 + exp(xi_vector$value))/(5 + exp(xi_vector$value)),
                                       ub = (1 + exp(xi_vector$ub))/(5 + exp(xi_vector$ub)),
                                       lb = (1 + exp(xi_vector$lb))/(5 + exp(xi_vector$lb))))
      }
    }
    data <- data |> dplyr::mutate(fixed = dplyr::case_when(value==lb ~ TRUE,
                                                           TRUE ~ FALSE))

    dq <- data.frame(scenario = integer(), name = character(), value = numeric(), ub = numeric(), lb = numeric())
    for (i in 1:length(dd_mle)) {
      b_ratio <- data.frame(scenario = i,
                            name = "B_annual_ratio",
                            value = dd_mle[[i]]$B_annual_ratio[length(dd_mle[[i]]$B_annual_ratio)],
                            ub = dd_mle[[i]]$B_annual_ratio[length(dd_mle[[i]]$B_annual_ratio)] + 1.96*dd_mle[[i]]$B_annual_ratio_sd[length(dd_mle[[i]]$B_annual_ratio_sd)],
                            lb = dd_mle[[i]]$B_annual_ratio[length(dd_mle[[i]]$B_annual_ratio)] - 1.96*dd_mle[[i]]$B_annual_ratio_sd[length(dd_mle[[i]]$B_annual_ratio_sd)])

      b_abs <- data.frame(scenario = i,
                          name = "B_annual",
                          value = dd_mle[[i]]$B_annual[length(dd_mle[[i]]$B_annual)],
                          ub = dd_mle[[i]]$B_annual[length(dd_mle[[i]]$B_annual)] + 1.96*dd_mle[[i]]$B_annual[length(dd_mle[[i]]$B_annual_sd)],
                          lb = dd_mle[[i]]$B_annual[length(dd_mle[[i]]$B_annual)] - 1.96*dd_mle[[i]]$B_annual[length(dd_mle[[i]]$B_annual_sd)])


      F_final <- data.frame(scenario = i,
                            name = "F_annual",
                            value = dd_mle[[i]]$F_annual[length(dd_mle[[i]]$F_annual)],
                            ub = dd_mle[[i]]$F_annual[length(dd_mle[[i]]$F_annual)] + 1.96*dd_mle[[i]]$F_annual[length(dd_mle[[i]]$F_annual_sd)],
                            lb = dd_mle[[i]]$F_annual[length(dd_mle[[i]]$F_annual)] - 1.96*dd_mle[[i]]$F_annual[length(dd_mle[[i]]$F_annual_sd)])

      LL <- data.frame(scenario = i,
                       name = "LL",
                       value = dd_mle[[i]]$LL,
                       ub = dd_mle[[i]]$LL + 1.96*dd_mle[[i]]$LL_sd,
                       lb = dd_mle[[i]]$LL - 1.96*dd_mle[[i]]$LL_sd)

      dq <- dq |> rbind(b_ratio, b_abs, F_final, LL)
    }

    dq$fixed <- FALSE

    if(show_B_ratio) {data <- data |> rbind(dq |> dplyr::filter(name=="B_annual_ratio"))}
    if(show_B_abs)   {data <- data |> rbind(dq |> dplyr::filter(name=="B_annual"))}
    if(show_F_final) {data <- data |> rbind(dq |> dplyr::filter(name=="F_annual"))}
    if(show_LL)      {data <- data |> rbind(dq |> dplyr::filter(name=="LL"))}

  } else { # start MCMC

    data <- data.frame(scenario = integer(), name = character(), value = numeric(), ub = numeric(), lb = numeric())
    name <- setdiff(names(dd_mle[[1]]$parameters),c("log_R_star","zeta"))


    for (scenario in scenarios) {

      if (missing(parameters)) {parameters <- data.frame(as.matrix(dd_mcmc[[scenario]]))}

      # calculate credible interval
      par_quantiles <- apply(parameters,2,quantile, na.rm = TRUE, probs = c(0.025, 0.5, 0.975)) |>
        as.data.frame() |>
        dplyr::select(!dplyr::contains('log_R_star')) |>
        dplyr::select(!lp__)

      for (i in 1:length(name)){
        if (name[i] %in% colnames(par_quantiles)){
          mcmc_results <- data.frame(scenario = scenario,
                                     name = name[i],
                                     value = par_quantiles[2,name[i]],
                                     ub = par_quantiles[3,name[i]],
                                     lb = par_quantiles[1,name[i]])
        } else {
          mcmc_results <- data.frame(scenario = scenario,
                                     name = name[i],
                                     value = as.numeric(dd_mle[[scenario]]$parameters[name[i]]),
                                     ub = as.numeric(dd_mle[[scenario]]$parameters[name[i]]),
                                     lb = as.numeric(dd_mle[[scenario]]$parameters[name[i]]))
        }
        data <- rbind(data, mcmc_results)
      }

      nyears <- dd_mle[[scenario]]$data$last_year_catch - dd_mle[[scenario]]$data$first_year_catch + 1

      if (show_B_ratio) {
        B_ratio <- sapply(1:nrow(as.matrix(dd_mcmc[[scenario]])), function(i){dd_sim[[scenario]][[i]]$B_annual_ratio[nyears]}) |>
          quantile(na.rm=TRUE, probs=c(0.025,0.5,0.975))
        B_ratio_df <- data.frame(scenario = scenario, name = "B_annual_ratio", value = B_ratio[2], ub = B_ratio[3], lb = B_ratio[1])
        data <- rbind(data, B_ratio_df)
      }

      if (show_B_abs) {
        B_abs <- sapply(1:nrow(as.matrix(dd_mcmc[[scenario]])), function(i){dd_sim[[scenario]][[i]]$B_annual[nyears]}) |>
          quantile(na.rm=TRUE, probs=c(0.025,0.5,0.975))
        B_abs_df <- data.frame(scenario = scenario, name = "B_annual", value = B_abs[2], ub = B_abs[3], lb = B_abs[1])
        data <- rbind(data, B_abs_df)
      }

      if (show_F_final) {
        F_final <- sapply(1:nrow(as.matrix(dd_mcmc[[scenario]])), function(i){dd_sim[[scenario]][[i]]$F_annual[nyears]}) |>
          quantile(na.rm=TRUE, probs=c(0.025,0.5,0.975))
        F_final_df <- data.frame(scenario = scenario, name = "F_annual", value = F_final[2], ub = F_final[3], lb = F_final[1])
        data <- rbind(data, F_final_df)
      }

      if (show_LL) {
        LL <- sapply(1:nrow(as.matrix(dd_mcmc[[scenario]])), function(i){dd_sim[[scenario]][[i]]$LL}) |>
          quantile(na.rm=TRUE, probs=c(0.025,0.5,0.975))
        LL_df <- data.frame(scenario = scenario, name = "LL", value = LL[2], ub = LL[3], lb = LL[1])
        data <- rbind(data, LL_df)
      }
    }
  } # end MCMC

  if (!missing(parameters)) {data <- data |> dplyr::filter(name %in% parameters)}

  data <- data |>
    dplyr::mutate(fixed = dplyr::case_when(value==lb ~ TRUE, TRUE ~ FALSE)) |>
    dplyr::filter(scenario %in% scenarios)

  rownames(data) <- NULL
  return(data)
}
