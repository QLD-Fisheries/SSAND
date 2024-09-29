# Copyright 2024 Fisheries Queensland

# This file is part of SSAND.
# SSAND is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
# SSAND is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
# You should have received a copy of the GNU General Public License along with SSAND. If not, see <https://www.gnu.org/licenses/>.

#' Prepare DDUST data for phase plot
#'
#' @param dd_mle A list of outputs from DDUST::makefullreport() with one list element per scenario. Will automatically reformat as a list if a single DDUST::makefullreport() output (i.e. one scenario) is entered
#' @param scenarios A vector of scenarios to plot (numeric). Shows all scenarios if left blank. Can be overridden in the plotting function.
#'
#' @return A data frame that contains year (int), Bratio (num), F_ (num), scenario (factor), B_MSY (num), F_max (num)
#' @export
#'
#' @examples
#' data <- phaseplot_prep_DD(dd_mle)
#' phaseplot(data)
phaseplot_prep_DD <- function(dd_mle,
                              scenarios = NULL) {

  if (check_scenarios(dd_mle,"DD","MLE")=="single scenario"){dd_mle <- list(dd_mle)}

  if (missing(scenarios)){scenarios <- 1:length(dd_mle)}


  data <- data.frame()
  for (scenario in scenarios) {
    year = as.integer(dd_mle[[scenario]]$data$first_year_catch:(dd_mle[[scenario]]$data$first_year_catch-1+length(dd_mle[[scenario]]$B_annual)))
    Bratio = dd_mle[[scenario]]$B_annual_ratio
    F_MSY = dd_mle[[scenario]]$Fmsy
    F_ = dd_mle[[scenario]]$F_annual/F_MSY
    B_MSY = dd_mle[[scenario]]$Bmsy

    F_max <- round(max(c(F_,0.01), na.rm=TRUE),2)
    if (F_max > 0.4){
      F_max <- max(F_max,1.2)+0.2
    } else {
      F_max <- 1.1*F_max
    }

    phase <- data.frame(year,
                        Bratio,
                        F_,
                        scenario = as.factor(scenario),
                        B_MSY,
                        F_max,
                        row.names=NULL) # prevents warning "row names were found from a short variable"

    data <- rbind(data, phase, row.names = NULL)
  }


  # if (MCMC) {
  #   data <- data.frame(year=numeric(),
  #                      Bratio=numeric(),
  #                      F_=numeric(),
  #                      scenario=character(),
  #                      B_MSY=numeric(),
  #                      F_max=numeric())
  #
  #   for (scenario in scenarios) {
  #     MSY   = dd_mle[[scenario]]$msy_run$msy[[1]]
  #     B_MSY = dd_mle[[scenario]]$msy_run$Final_bio_prop[[1]]
  #     F_MSY = dd_mle[[scenario]]$msy_run$F_mort[[1]]
  #
  #     model <- dd_mle[[scenario]]$model
  #     fit_matrix <- as.matrix(dd_mcmc[[scenario]])
  #     nyears <- dd_mle[[scenario]]$data$last_year_catch-dd_mle[[scenario]]$data$first_year_catch+1
  #     numChains <- dd_mcmc[[scenario]]@sim$chains
  #
  #     # Extract derived quantities of interest
  #     sim <- dd_sim[[scenario]]
  #     B_final <- sapply(1:nrow(fit_matrix), function(i){sim[[i]]$B_annual_ratio[nyears]})
  #     pos <- mcmc_median_position_DD(dd_mle,dd_mcmc,dd_sim)[scenario]
  #
  #     year = as.integer(dd_mle[[scenario]]$data$first_year_catch:(dd_mle[[scenario]]$data$first_year_catch-1+length(dd_mle[[scenario]]$B_annual)))
  #     Bratio = sim[[pos]]$B_annual_ratio
  #     F_ = colSums(matrix(sim[[pos]]$F_timestep, nrow = 12/dd_mle[[scenario]]$data$Number_months_per_timestep))/F_MSY
  #
  #     F_max <- round(max(c(F_,0.01), na.rm=TRUE),2)
  #     if (F_max > 0.4){
  #       F_max <- max(F_max,1.2)+0.2
  #     } else {
  #       F_max <- 1.1*F_max
  #     }
  #
  #     data <- rbind(data,data.frame(year = year,
  #                                   Bratio = Bratio,
  #                                   F_ = F_,
  #                                   scenario = as.factor(scenario),
  #                                   B_MSY = B_MSY,
  #                                   F_max = F_max))
  #   }
  # }
  rownames(data) <- NULL
  return(data)
}
