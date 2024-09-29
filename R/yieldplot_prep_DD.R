# Copyright 2024 Fisheries Queensland

# This file is part of SSAND.
# SSAND is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
# SSAND is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
# You should have received a copy of the GNU General Public License along with SSAND. If not, see <https://www.gnu.org/licenses/>.

#' Prepare DDUST data for yield plot
#'
#' @param dd_mle A list of outputs from DDUST::makefullreport() with one list element per scenario. Will automatically reformat as a list if a single DDUST::makefullreport() output (i.e. one scenario) is entered
#' @param dd_mcmc A list of model fits from tmbstan::tmbstan() with one list element per scenario. Only needed if MCMC was used.
#' @param dd_sim A list of outputs from SSAND::simulate_DD() with one list element per scenario. Only required if MCMC was used. Will automatically reformat as a list if a single DDUST::makefullreport() output (i.e. one scenario) is entered
#' @param scenarios A vector of scenarios to plot (numeric). Shows all scenarios if left blank. Can be overridden in the plotting function.
#'
#' @return A dataframe with Final_bio (num), yield (num), data (chr), scenario (int)
#' @export
#'
#' @examples
#' data <- yieldplot_prep_DD(dd_mle)
#' yieldplot(data)
yieldplot_prep_DD <- function(dd_mle,
                              dd_mcmc = NULL,
                              dd_sim = NULL,
                              scenarios = NULL) {

  if (missing(dd_mcmc)) {MCMC = FALSE} else {MCMC = TRUE}
  if (check_scenarios(dd_mle,"DD","MLE")=="single scenario"){dd_mle <- list(dd_mle)}
  if (MCMC && check_scenarios(dd_mcmc,"DD","MCMC")=="single scenario"){dd_mcmc <- list(dd_mcmc)}
  if (MCMC && check_scenarios(dd_sim,"DD","SIM")=="single scenario"){dd_sim <- list(dd_sim)}

  if (missing(scenarios)){scenarios <- 1:length(dd_mle)}

  if (!MCMC) {

    data <- data.frame()
    for (scenario in scenarios) {

      yieldcurve <- data.frame(Final_bio = dd_mle[[scenario]]$B_values,
                               yield = dd_mle[[scenario]]$yield_values,
                               data = "data")

      yieldmsy   <- data.frame(Final_bio = dd_mle[[scenario]]$Bmsy,
                               yield = dd_mle[[scenario]]$msy,
                               data = "msy")

      # Calculate vertical value for current biomass line
      B_current <- dplyr::last(dd_mle[[scenario]]$B_annual_ratio)
      yieldcurve_sorted <- yieldcurve[order(yieldcurve$Final_bio), ]

      below <- if (any(yieldcurve_sorted$Final_bio < B_current)) {
        yieldcurve_sorted[max(which(yieldcurve_sorted$Final_bio < B_current)), ]
      } else {data.frame(Final_bio=NA,yield=NA,data=NA)}

      above <- if (any(yieldcurve_sorted$Final_bio > B_current)) {
        yieldcurve_sorted[min(which(yieldcurve_sorted$Final_bio > B_current)), ]
      } else {data.frame(Final_bio=NA,yield=NA,data=NA)}

      if (below$Final_bio==B_current | is.na(above$Final_bio)) {yieldfinal <- below |> dplyr::mutate(data="final")}
      if (above$Final_bio==B_current | is.na(below$Final_bio)) {yieldfinal <- above |> dplyr::mutate(data="final")}

      if (is.na(below$Final_bio) & is.na(above$Final_bio)) {yieldfinal <- data.frame()} # Should never happen

      if (!is.na(below$Final_bio) & !is.na(above$Final_bio)) {
        yieldfinal <- data.frame(
          Final_bio = B_current,
          yield = below$yield + (B_current - below$Final_bio) * (above$yield - below$yield) / (above$Final_bio - below$Final_bio),
          data = "final")
      }

      yield <- rbind(yieldcurve, yieldmsy, yieldfinal) |>
        dplyr::mutate(scenario = scenario)

      data <- rbind(data, yield)
    }
  } else { # if MCMC
    warning("Functionality not yet built for DDUST MCMC yield curve.")

    # if (missing(dd_sim)){
    #   stop('Please provide a list of MCMC simulations: \n dd_sim <- SSAND::simulate_DD(dd_mle, dd_mcmc)')
    # }
    #
    # data_all <- data.frame(Final_bio_prop = numeric(),
    #                        F_mort = numeric(),
    #                        msy = numeric(),
    #                        data = character(),
    #                        scenario = character())
    #
    # for (scenario in scenarios){
    #   yieldcurve <- data.frame(Final_bio_prop=numeric(),
    #                            F_mort=numeric(),
    #                            msy=numeric(),
    #                            data = character())
    #
    #   model <- dd_mle[[scenario]]$model
    #
    #   fit_matrix <- as.matrix(dd_mcmc[[scenario]])
    #
    #   # sim <- sapply(1:nrow(fit_matrix), function(i){list(model$simulate(fit_matrix[i,1:ncol(fit_matrix)-1]))})
    #   sim <- dd_sim[[scenario]]
    #
    #   #Find median MCMC run based on median final year biomass
    #   nyears <- dd_mle[[scenario]]$data$last_year_catch - dd_mle[[scenario]]$data$first_year_catch + 1
    #   B_final <- sapply(1:nrow(fit_matrix), function(i){sim[[i]]$B_annual_ratio[nyears]})
    #   pos <- mcmc_median_position_DD(dd_mle,dd_mcmc,dd_sim)[scenario]
    #   F_final <- dplyr::last(colSums(matrix(sim[[pos]]$F_timestep, nrow = 12/dd_mle[[scenario]]$data$Number_months_per_timestep)))
    #
    #   if (is.null(dd_mle[[1]]$msy_run)){
    #     model_msy <- list()
    #     yieldcurve_msy <- list()
    #     dd_mle_base_all_msy_opt <- list()
    #     dd_mle_target60_all_opt <- list()
    #
    #     # only plot median for now
    #
    #     # Fishing mortality pattern
    #     F_timestep_allyears <- sapply(1:nrow(fit_matrix), function(i){sim[[i]]$F_timestep})
    #     F_timestep <- matrix(as.numeric(F_timestep_allyears[(nrow(F_timestep_allyears)-(5*12/dd_mle[[scenario]]$data$Number_months_per_timestep - 1)):
    #                                                           nrow(F_timestep_allyears),pos]),nrow = 12/dd_mle[[scenario]]$data$Number_months_per_timestep)
    #     F_pattern_raw <- rowMeans(F_timestep)
    #     F_pattern <- F_pattern_raw/sum(F_pattern_raw)
    #
    #     # Catchability pattern
    #     if (length(sim[[1]]$log_q)==1){
    #       log_q <- sapply(1:nrow(fit_matrix), function(i){sim[[i]]$log_q})
    #     } else {
    #       log_q <- sapply(1:nrow(fit_matrix), function(i){sim[[i]]$log_q})[length(sim[[1]]$log_q),]
    #     }
    #
    #     if ("q1" %in% names(dd_mcmc[[scenario]])){
    #       q1 <- fit_matrix[,"q1"]
    #       q2 <- fit_matrix[,"q2"]
    #     } else {
    #       q1 <- rep(0,nrow(fit_matrix))
    #       q2 <- rep(0,nrow(fit_matrix))
    #     }
    #
    #     months <- as.vector(2*pi*dd_mle[[scenario]]$data$m_seq/(12/dd_mle[[scenario]]$data$Number_months_per_timestep))
    #
    #     temp_q_dt <- sapply(1:length(log_q), function(i) {exp(log_q[i]+q1[i]*cos(months)+q2[i]*sin(months))})
    #     q_dt <- temp_q_dt[seq(1,12,by=dd_mle[[scenario]]$data$Number_months_per_timestep),]
    #
    #     # Build data
    #     Number_months_per_timestep <- dd_mle[[scenario]]$data$Number_months_per_timestep
    #     R0scalar <- dd_mle[[scenario]]$data$R0scalar
    #     Ps <- dd_mle[[scenario]]$data$Ps
    #     wtrec <- dd_mle[[scenario]]$data$wtrec
    #     smoothness <- dd_mle[[scenario]]$data$smoothness
    #     m_seq <- dd_mle[[scenario]]$data$m_seq
    #     eq_iter <- dd_mle[[scenario]]$data$eq_iter
    #     rho <- dd_mle[[scenario]]$data$rho
    #     M <- dd_mle[[scenario]]$M
    #     years_to_run <- as.numeric(100)
    #
    #     if (!('Rinit' %in% names(dd_mcmc[[scenario]]))){
    #       Rinit <- dd_mle[[scenario]]$Rinit
    #     } else {
    #       Rinit <- as.numeric(fit_matrix[pos,"Rinit"])
    #     }
    #
    #     if (!('xi' %in% names(dd_mcmc[[scenario]]))){
    #       xi <- dd_mle[[scenario]]$xi
    #     } else {
    #       xi <- as.numeric(fit_matrix[pos,"xi"])
    #     }
    #
    #     if (!('k' %in% names(dd_mcmc[[scenario]]))){
    #       k <- dd_mle[[scenario]]$k
    #     } else {
    #       k <- as.numeric(fit_matrix[pos,"k"])
    #     }
    #
    #     if (!('mu' %in% names(dd_mcmc[[scenario]]))){
    #       mu <- dd_mle[[scenario]]$mu
    #     } else {
    #       mu <- as.numeric(fit_matrix[pos,"mu"])
    #     }
    #
    #     if (!('M' %in% names(dd_mcmc[[scenario]]))){
    #       M <- dd_mle[[scenario]]$M
    #     } else {
    #       M <- as.numeric(fit_matrix[pos,"M"])
    #     }
    #
    #     data <- list(Number_months_per_timestep = Number_months_per_timestep,
    #                  R0scalar = R0scalar,
    #                  Ps = Ps,
    #                  wtrec = wtrec,
    #                  smoothness = smoothness,
    #                  m_seq = m_seq,
    #                  eq_iter = eq_iter,
    #                  rho = rho,
    #                  Rinit = Rinit,
    #                  xi = xi,
    #                  M = M,
    #                  k = k,
    #                  mu = mu,
    #                  q_dt = ifelse(Number_months_per_timestep==12,q_dt[pos],q_dt[,pos]),
    #                  F_pattern =  F_pattern,
    #                  years_to_run = years_to_run)
    #
    #     parameters <- list(F_mort = 1)
    #
    #     # model_msy <- TMB::MakeADFun(data = c(model = "DDUST_msy",data),
    #     #                             parameters,
    #     #                             DLL = "DDUST_TMBExports")
    #
    #     F_values <- c(F_final, seq(0,3,by=0.01))
    #     data_values <- c("final", rep("data", 301))
    #   }else{
    #
    #     data <- dd_mle[[scenario]]$msy_run$data
    #     parameters <- list(F_mort = 1)
    #
    #     # model_msy <- TMB::MakeADFun(data = c(model = "DDUST_msy",data),
    #     #                             parameters,
    #     #                             DLL = "DDUST_TMBExports")
    #     temp_Fmsy = dd_mle[[scenario]]$msy_run$F_mort
    #     temp_msy = dd_mle[[scenario]]$msy_run$msy
    #     F_values <- c(F_final, seq(0,1,0.05)*temp_Fmsy)
    #     data_values <- c("final", rep("data", 20), "msy")
    #
    #
    #   }
    #   # run first part of curve
    #   for (i in 1:length(F_values)){
    #     temp = model_msy$simulate(par=list(F_mort = F_values[i]))
    #     yieldcurve <- rbind(yieldcurve, data.frame(Final_bio=temp$Final_bio_prop,
    #                                                F_mort=temp$F_mort, yield=temp$msy, data=data_values[i]))
    #   }
    #
    #   if(is.null(dd_mle[[1]]$msy_run)){
    #     temp_Fmsy = last(yieldcurve$F_mort)
    #     temp_msy = last(yieldcurve$yield)
    #   }
    #   # run second part of curve
    #   inc = 1
    #   while(temp$msy>(temp_msy/1e5)){
    #     inc = inc+0.03
    #     temp_Fmsy = temp_Fmsy * inc # this will give bigger and bigger increments as F gets increasingly large
    #     temp = model_msy$simulate(par=list(F_mort = temp_Fmsy))
    #     if(is.nan(temp$msy) | inc>3){
    #       break
    #     }
    #     yieldcurve <- rbind(yieldcurve,
    #                         data.frame(Final_bio=temp$Final_bio_prop,
    #                                    F_mort=temp$F_mort, yield=temp$msy, data="data"))
    #   }
    #   data_all <- rbind(data_all, yieldcurve |> dplyr::mutate(scenario = scenario))
    # }
    # data_all <- data_all |>
    #   dplyr::mutate(scenario = factor(scenario, levels = scenarios)) |>
    #   dplyr::select(-F_mort)
    # data <- data_all

  }
  rownames(data) <- NULL
  return(data)
}
