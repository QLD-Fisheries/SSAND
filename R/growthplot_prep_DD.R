# Copyright 2024 Fisheries Queensland

# This file is part of SSAND.
# SSAND is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
# SSAND is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
# You should have received a copy of the GNU General Public License along with SSAND. If not, see <https://www.gnu.org/licenses/>.

#' Prepare DDUST data for growth plot
#'
#' @param dd_mle A list of outputs from DDUST::makefullreport() with one list element per scenario. Will automatically reformat as a list if a single DDUST::makefullreport() output (i.e. one scenario) is entered
#' @param type 'length' for Von Bert model or 'weight' for Schnute model.
#' @param age_rec Age of recruitment, in the unit of measurement used in the model.
#' @param age_max Maximum age to plot, in the unit of measurement used in the model.
#' @param W_r_minus1 Weight at the age one timestep before recruitment. This can be left as NA to use the value used in the model.
#' @param W_r Weight at recruitment. This can be left as NA to use the value used in the model.
#' @param rho A parameter of the Schnute growth equation. This can be left as NA to use the value used in the model.
#' @param t0 Von Bert growth parameter (annual)
#' @param kappa Von Bert growth parameter (annual)
#' @param Linf Von Bert growth parameter (in cm)
#' @param scenarios A vector of scenarios to plot (numeric). Shows all scenarios if left blank. Can be overridden in the plotting function.
#'
#' @return A data frame with variables age (numeric), value (numeric), upper (numeric), and lower (numeric) and Sex (integer)
#' @export
#'
#' @examples
#' data <- growthplot_prep_DD(dd_mle)
#' growthplot(data)
growthplot_prep_DD <- function(dd_mle,
                               type = 'weight',
                               age_rec=2,
                               age_max=10*12,
                               W_r_minus1=NA,
                               W_r=NA,
                               rho=NA,
                               t0=NA,
                               kappa=NA,
                               Linf=NA,
                               scenarios = 1
                               ) {

  if (check_scenarios(dd_mle,"DD","MLE")=="single scenario"){dd_mle <- list(dd_mle)}
  if (missing(scenarios)){scenarios <- 1:length(dd_mle)}

  data <- data.frame()
  for (scenario in scenarios) {
    if (type=='weight') {

      #if weight-at-age parameters not provided - take them from dd_mle
      if (is.na(W_r_minus1)) {W_r_minus1 = dd_mle[[scenario]]$data$weight_at_recruitment[[1]]}
      if (is.na(W_r)) {W_r = dd_mle[[scenario]]$data$weight_at_recruitment[[2]]}
      if (is.na(rho)) {rho = dd_mle[[scenario]]$rho}

      age_vector <- seq(0,age_max,by=12/dd_mle[[scenario]]$data$Number_months_per_timestep)
      weight_vector <- W_r_minus1 + (W_r-W_r_minus1)*(1-rho^(1+age_vector-age_rec))/(1-rho) #Schnute growth curve (Eq 5.14 of Quinn & Deriso)
      tmp <- data.frame(age=age_vector, value=weight_vector, lower=NA, upper=NA, sex=1)
      tmp$scenario <- scenario

    } else if (type=='length'){

      if (is.na(t0)) {t0=0}
      if (is.na(Linf) + is.na(kappa) > 0) {stop('Provide growth parameters')}

      age_vector <- seq(0,age_max/12*dd_mle[[scenario]]$data$Number_months_per_timestep,by=dd_mle[[scenario]]$data$Number_months_per_timestep/12)
      length_vector <- Linf*(1-exp(-kappa*(age_vector-t0)))
      tmp <- data.frame(age=age_vector, value=length_vector, lower=NA, upper=NA, sex=1)
      tmp$scenario <- scenario
    } else {
      stop('Check type input')
    }
    data <- rbind(data, tmp)
  }
  rownames(data) <- NULL
  return(data)
}
