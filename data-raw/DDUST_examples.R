# Copyright 2024 Fisheries Queensland

# This file is part of SSAND.
# SSAND is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
# SSAND is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
# You should have received a copy of the GNU General Public License along with SSAND. If not, see <https://www.gnu.org/licenses/>.

# 📚 Libraries ----
suppressPackageStartupMessages({
  library(DDUST)     #DDUST
  library(dplyr)     #tidyverse
  library(ggplot2)   #plots
  library(cowplot)   #plots
  library(ggrepel)   #spaghetti plot
  library(tmbstan)   #MCMC
}
)

# 👀 Model Overview ----
# Set up 3 model scenarios
param_scenarios <- data.frame(
  model_index = c(1,2,3),
  N_m = c(1,6,12))

# 🏃 Run models ----

# Setup
dd_out <- list()

for (i in 1:length(param_scenarios$model_index)){
  data <- list()
  data$Number_months_per_timestep <- param_scenarios$N_m[i]
  data$proportion_spawning <- rep(1/12,12)
  data$weight_at_recruitment <- c(1,2)
  data$weight_inf <- c(5)
  data$last_year_catch <- 2023
  data$first_year_catch <- 1950
  data$first_year_rec_devs <- 1950
  data$month_sequence <- as.numeric(1:12)
  data$calculate_rho <- 0
  data$rho_input <- 0.606
  data$use_recruit_penalty <- 1
  data$prior_mean_xi <- log(3)
  data$prior_mean_mu <- 0
  data$prior_mean_k <- 5
  data$prior_sd_xi <- 1
  data$prior_sd_mu <- 1
  data$prior_sd_k <- 1
  data$use_cpue_sd <- 0
  data$rec_dev_type <- "random"
  data$ctch <- c(seq(0,1000,length.out=round(888*0.5)),seq(1000,5,length.out=round(888*0.5)))
  data$cpue <- rnorm(888,30,5)-c(seq(0,15,length.out=round(888*0.75)),seq(15,5,length.out=round(888*0.25)))
  data$cpue_sd <- t(matrix(rep(0,888)))

  # add noise to catch
  set.seed(123)
  data$ctch <- data$ctch + rnorm(length(data$ctch),0, data$ctch/3)

  # Adjust catch and cpue according to timestep. Sum catch and mean cpue.
  data$ctch <- rowSums(matrix(data$ctch,ncol=param_scenarios$N_m[i], byrow=T))
  data$cpue <- t(matrix(rowSums(matrix(data$cpue,ncol=param_scenarios$N_m[i], byrow=T))/param_scenarios$N_m[i]))

  # settings
  data$projection_years <- 50
  data$target_relative_biomass <- 0.6
  data$F_initial <- 0.05

  parameters <- list()
  parameters$Rinit <- 15
  parameters$xi <- log(0.5)
  parameters$M <- 1.2
  parameters$k <- 1.5
  parameters$mu <- 3.5
  parameters$q1 <- 0.15
  parameters$q2 <- 0.01
  parameters$lsigmaR_sq <- log(0.36^2)
  parameters$lsigmaI_sq <- log(0.2^2)
  parameters$zeta <- rnorm(2023 - 1950, 0, 0.36)
  parameters$log_R_star <- as.numeric(rep(19,74))

  data <- check_data(data, silent = TRUE)
  parameters <- check_parameters(parameters, rec_dev_type = data$rec_dev_type)

  if (param_scenarios$N_m[i]==12){
    map <- list(M=factor(NA),
               lsigmaI_sq=factor(NA),
               k=factor(NA),
               mu=factor(NA),
               q1=factor(NA),
               q2=factor(NA))
  } else {
    map <- list(M=factor(NA),
                lsigmaI_sq=factor(NA))
  }

  map$zeta <- rep(factor(NA),73)

  dd_out[[i]] <- run_DDUST(data, parameters, map, MCMC = FALSE)

}

dd_out_transposed <- purrr::list_transpose(dd_out)
dd_mle <- dd_out_transposed$dd_mle

usethis::use_data(dd_mle, overwrite = TRUE)
