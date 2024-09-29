# Copyright 2024 Fisheries Queensland

# This file is part of SSAND.
# SSAND is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
# SSAND is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
# You should have received a copy of the GNU General Public License along with SSAND. If not, see <https://www.gnu.org/licenses/>.

#' Prepare DDUST data for stock-recruit curve plot
#'
#' @param dd_mle A list of outputs from DDUST::makefullreport() with one list element per scenario. Will automatically reformat as a list if a single DDUST::makefullreport() output (i.e. one scenario) is entered
#' @param dd_mcmc A list of model fits from tmbstan::tmbstan() with one list element per scenario. Only needed if MCMC was used.
#' @param scenarios A vector of scenarios to plot (numeric). Shows all scenarios if left blank. Can be overridden in the plotting function.
#'
#' @return A data frame with variables year (int), spawn_bio (num), pred_rec (num), dev (num), exp_recr (num), bias_adjusted (num), scenario (factor)
#' @export
#'
#' @examples
#' data <- srplot_prep_DD(dd_mle)
#' srplot(data)
srplot_prep_DD <- function(dd_mle,
                           dd_mcmc=NULL,
                           scenarios = NULL
){

  if (missing(dd_mcmc)) {MCMC = FALSE} else {MCMC = TRUE}

  if (check_scenarios(dd_mle,"DD","MLE")=="single scenario"){dd_mle <- list(dd_mle)}
  if (MCMC && check_scenarios(dd_mcmc,"DD","MCMC")=="single scenario"){dd_mcmc <- list(dd_mcmc)}
  if (MCMC && check_scenarios(dd_sim,"DD","SIM")=="single scenario"){dd_sim <- list(dd_sim)}

  if (missing(scenarios)){scenarios <- 1:length(dd_mle)}


  if (!MCMC) {
    data <- data.frame()
    for (scenario in scenarios) {
      h = dd_mle[[scenario]]$h
      R0 = exp(dd_mle[[scenario]]$Rinit)
      SB = dd_mle[[scenario]]$B_annual
      SB_eq = dd_mle[[scenario]]$equilibrium.Spawners[length(dd_mle[[scenario]]$equilibrium.Spawners)]
      alpha = SB_eq*(1-h)/(4*h*R0)
      beta = (5*h-1)/(4*h*R0)
      R = SB/(alpha + beta*SB)
      year = dd_mle[[scenario]]$data$first_year_catch:dd_mle[[scenario]]$data$last_year_catch

      if (dd_mle[[scenario]]$data$rec_dev_type=='random') {
        recdevs = c(rep(0, length(dd_mle[[scenario]]$data$first_year_catch:(dd_mle[[scenario]]$data$first_year_rec_devs))-1), dd_mle[[scenario]]$RecDev)
      } else {
        recdevs = c(rep(0, length(dd_mle[[scenario]]$data$first_year_catch:(dd_mle[[scenario]]$data$first_year_rec_devs))-1), dd_mle[[scenario]]$zeta_matrix)
      }

      pred_recr = R*(1+recdevs)
      SR_data <- data.frame(year = year,
                            spawn_bio=SB/SB[1],
                            pred_recr=pred_recr/R[1],
                            dev = recdevs,
                            exp_recr=R/R[1],
                            bias_adjusted=R/R[1],
                            scenario = as.factor(scenario))
      data <- rbind(data,SR_data)
    }
  }

  if (MCMC) {

    warning("DDUST MCMC functionality not yet built for SR curve")

    # SR_data <- data.frame(iteration=as.numeric(),
    #                       spawn_bio=as.numeric(),
    #                       year=as.numeric(),
    #                       pred_recr=as.numeric(),
    #                       dev=as.numeric(),
    #                       exp_recr=as.numeric(),
    #                       scenario=as.numeric())
    #
    # SR_data_mcmc <- data.frame(iteration=as.numeric(),
    #                            spawn_bio=as.numeric(),
    #                            year=as.numeric(),
    #                            pred_recr=as.numeric(),
    #                            dev=as.numeric(),
    #                            exp_recr=as.numeric(),
    #                            scenario=as.numeric())
    # p <- list()
    #
    # for (scenario in scenarios){
    #
    #   fit <- as.matrix(dd_mcmc[[scenario]])
    #   nyears <- dd_mle[[scenario]]$data$last_year_catch - dd_mle[[scenario]]$data$first_year_catch + 1
    #
    #   # find parameter set that results in median final B_annual_ratio
    #   sim <- simulation_list[[scenario]]
    #   B_final <- sapply(1:nrow(fit), function(i){sim[[i]]$B_annual_ratio[nyears]})
    #   pos <- which(B_final == median(B_final[1:(length(B_final)-1)]))[1] #if more than 1 run, just take the first one
    #
    #   #median fit
    #   med_fit <- fit[pos,]
    #
    #   # extract s-r curve for median fit
    #   if (!('Rinit' %in% names(dd_mcmc[[scenario]]))){
    #     med_R0 <- exp(dd_mle[[scenario]]$Rinit)*dd_mle[[scenario]]$data$R0scalar
    #   } else {
    #     med_R0 <- exp(fit[pos,'Rinit'])*dd_mle[[scenario]]$data$R0scalar
    #   }
    #
    #   if (!('xi' %in% names(dd_mcmc[[scenario]]))){
    #     med_h <- (1+exp(dd_mle[[scenario]]$xi))/(5+exp(dd_mle[[scenario]]$xi))
    #   } else {
    #     med_h <- (1+exp(fit[pos,'xi']))/(5+exp(fit[pos,'xi']))
    #   }
    #
    #   med_SB <- sim[[pos]]$SpawnB_annual
    #   med_SB_eq <- sim[[pos]]$biomass_wup[nyears]*0.5
    #   med_alpha <- med_SB_eq*(1-med_h)/(4*med_h*med_R0)
    #   med_beta <- (5*med_h-1)/(4*med_h*med_R0)
    #   med_R <- med_SB/(med_alpha + med_beta*med_SB)
    #   med_SB_norm <- med_SB/med_SB[1]
    #   med_R_norm <- med_R/med_R[1]
    #   year <- dd_mle[[scenario]]$data$first_year_catch:dd_mle[[scenario]]$data$last_year_catch
    #   no_rec_year <- rep(0, length(dd_mle[[scenario]]$data$first_year_catch:(dd_mle[[scenario]]$data$first_year_rec_devs)))
    #
    #   if (dd_mle[[scenario]]$model_name=='REDDUST') {
    #     no_rec_year <- rep(0, length(dd_mle[[scenario]]$data$first_year_catch:(dd_mle[[scenario]]$data$first_year_rec_devs))-1)
    #     med_recdevs <- med_R*(1+c(no_rec_year,sim[[pos]]$RecDev))/med_R[1]
    #     recdevs = c(med_recdevs) |> unlist()
    #   } else {
    #     no_rec_year <- rep(0, length(dd_mle[[scenario]]$data$first_year_catch:(dd_mle[[scenario]]$data$first_year_rec_devs)))
    #     med_recdevs <- med_R*(1+c(no_rec_year,fit[pos,grep("zeta", colnames(fit))]))/med_R[1]
    #     recdevs = c(med_recdevs) |> unlist()
    #   }
    #
    #   SR_data <- data.frame(iteration='med',
    #                         spawn_bio=med_SB_norm,
    #                         year=year,
    #                         pred_recr=recdevs/recdevs[1],
    #                         exp_recr=med_R_norm,
    #                         scenario=scenario)
    #
    #   SR_data2 <- SR_data[order(SR_data$spawn_bio),] # not sure if this is necessary
    #
    #   # All other mcmc iterations
    #   if (!('Rinit' %in% names(dd_mcmc[[scenario]]))){
    #     R0 <- rep(exp(dd_mle[[scenario]]$Rinit)*dd_mle[[scenario]]$data$R0scalar,nrow(fit))
    #   } else {
    #     R0 <- exp(fit[,'Rinit'])*dd_mle[[scenario]]$data$R0scalar
    #   }
    #
    #   if (!('xi' %in% names(dd_mcmc[[scenario]]))){
    #     h <- rep((1+exp(dd_mle[[scenario]]$xi))/(5+exp(dd_mle[[scenario]]$xi)),nrow(fit))
    #   } else {
    #     h <- (1+exp(fit[,'xi']))/(5+exp(fit[,'xi']))
    #   }
    #
    #   SB <- sapply(1:nrow(fit), function(i){sim[[i]]$SpawnB_annual})
    #   # names(SB) <- paste0(1:length(sim))
    #   SB_eq <- sapply(1:nrow(fit), function(i){sim[[i]]$biomass_wup[nyears]})*0.5
    #   # names(SB_eq) <- paste0(1:length(sim))
    #
    #   alpha <- SB_eq*(1-h)/(4*h*R0)
    #   beta <- (5*h-1)/(4*h*R0)
    #   R <- matrix(data=NA, nrow = nrow(SB), ncol = ncol(SB))
    #   for (i in 1:ncol(SB)){
    #     R[,i] <- SB[,i]/(alpha[i] + beta[i]*SB[,i])
    #   }
    #   # names(R) <- paste0('iter',1:length(sim))
    #   year <- dd_mle[[scenario]]$data$first_year_catch:dd_mle[[scenario]]$data$last_year_catch
    #   SB_norm <- SB/matrix(SB[1,],nrow=nrow(SB),ncol=ncol(SB), byrow=TRUE)
    #   exp_recr <- R/matrix(R[1,],nrow=nrow(R),ncol=ncol(R), byrow=TRUE)
    #
    #   spawn_bio <- SB_norm |>
    #     as.data.frame() |>
    #     tidyr::pivot_longer(everything(), names_to = 'iteration')  |>
    #     dplyr::mutate(year=sort(rep(year,length(sim))))
    #
    #   exp_recr2 <- exp_recr |>
    #     as.data.frame() |>
    #     tidyr::pivot_longer(everything(),names_to = 'iteration') |>
    #     dplyr::mutate(year=sort(rep(year,length(sim))))
    #
    #   #formatting things
    #   temp <- cbind(spawn_bio,pred_recr=NA,exp_recr2$value,scenario=scenario)
    #
    #   temp2 <- temp |>
    #     as.data.frame() |>
    #     dplyr::rename(exp_recr=`exp_recr2$value`,spawn_bio=value)
    #
    #   SR_data_mcmc <- rbind(SR_data_mcmc,temp2)
    #
    #   SR_data_mcmc2 <- SR_data_mcmc[order(SR_data_mcmc$spawn_bio),]
    #
    #   SR_data <- SR_data |>
    #     dplyr::mutate(scenario = as.factor(scenario), iteration = as.factor(iteration))
    #
    #   samples <- seq(1,nrow(SR_data_mcmc),by=round(nrow(SR_data_mcmc)/nsamples))
    #   SR_data_mcmc_subset <- SR_data_mcmc[samples,]
    # }
    # return(SR_data_mcmc_subset)
  }
  rownames(data) <- NULL
  return(data)
}
