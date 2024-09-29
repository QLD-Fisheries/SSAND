# Copyright 2024 Fisheries Queensland

# This file is part of SSAND.
# SSAND is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
# SSAND is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
# You should have received a copy of the GNU General Public License along with SSAND. If not, see <https://www.gnu.org/licenses/>.

#' Prepare DDUST data for SSAND::weightplot()
#'
#' @param dd_mle A list of outputs from DDUST::makefullreport() with one list element per scenario. Will automatically reformat as a list if a single DDUST::makefullreport() output (i.e. one scenario) is entered
#' @param scenarios A vector of scenarios to plot (numeric). Shows all scenarios if left blank. Can be overridden in the plotting function.
#' @param age_max Maximum age to plot, in the unit of measurement used in the model.
#' @param age_rec Age of recruitment, in the unit of measurement used in the model.
#' @param sex Indicate which sex to plot. Enter 1 for female or 2 for male.
#'
#' @return A dataframe with xvar (int), weight (num), scenario (int), sex (character)
#' @export
#'
#' @examples
#' data <- weightplot_prep_DD(dd_mle, age_max=10, age_rec=2)
#' weightplot(data)
weightplot_prep_DD <- function(dd_mle,
                               scenarios = NULL,
                               age_max = NULL,
                               age_rec = NULL,
                               sex = "female") {

  if (check_scenarios(dd_mle,"DD","MLE")=="single scenario"){dd_mle <- list(dd_mle)}

  if (missing(scenarios)){scenarios <- 1:length(dd_mle)}

  if (missing(age_max)) {stop("Please enter a maximum age to plot, in the unit of measurement used in the model.")}
  if (missing(age_rec)) {stop("Please enter the age of recruitment, in the unit of measurement used in the model.")}

  # Convert SS notation
  sex <- dplyr::case_when(
    sex == "female" ~ "female",
    sex == "male" ~ "male",
    sex == 1 ~ "female",
    sex == 2 ~ "male",
    sex == 3 ~ "combined", # by SS - sex ratio preserved
    sex == 0 ~ "combined"  # by user
  )

  data <- data.frame()
  for (scenario in scenarios) {
    W_r_minus1 = dd_mle[[scenario]]$data$weight_at_recruitment[[1]]
    W_r = dd_mle[[scenario]]$data$weight_at_recruitment[[2]]
    rho = dd_mle[[scenario]]$rho

    age_vector <- seq(0,age_max)
    weight_vector <- W_r_minus1 + (W_r-W_r_minus1)*(1-rho^(1+age_vector-age_rec))/(1-rho) #Schnute growth curve (Eq 5.14 of Quinn & Deriso)
    tmp <- data.frame(xvar=age_vector, weight=weight_vector, scenario=scenario, sex=sex)
    tmp$scenario <- scenario
    data <- rbind(data, tmp)
  }
  rownames(data) <- NULL
  return(data)
}

