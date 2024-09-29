# Copyright 2024 Fisheries Queensland

# This file is part of SSAND.
# SSAND is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
# SSAND is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
# You should have received a copy of the GNU General Public License along with SSAND. If not, see <https://www.gnu.org/licenses/>.

#' Prepare DDUST data for catchplot()
#'
#' @param dd_mle A list of outputs from DDUST::makefullreport() with one list element per scenario. Will automatically reformat as a list if a single DDUST::makefullreport() output (i.e. one scenario) is entered
#' @param scenarios A vector of scenarios to plot (numeric). Shows all scenarios if left blank. Can be overridden in the plotting function.
#'
#' @return A dataframe with date, value, sector, scenario
#' @export
#'
#' @examples
#' data <- catchplot_prep_DD(dd_mle)
#' catchplot(data)
catchplot_prep_DD <- function(dd_mle,
                              scenarios = NULL){

  if (check_scenarios(dd_mle,"DD","MLE")=="single scenario"){dd_mle <- list(dd_mle)}
  if (missing(scenarios)){scenarios <- 1:length(dd_mle)}

  data <- data.frame()
  for (scenario in scenarios) {
    tmp <- data.frame(year = sort(rep(dd_mle[[scenario]]$data$first_year_catch:dd_mle[[scenario]]$data$last_year_catch,12/dd_mle[[scenario]]$data$Number_months_per_timestep)),
                      month = seq(1,12,dd_mle[[scenario]]$data$Number_months_per_timestep),
                      value = dd_mle[[scenario]]$data$ctch/1000,
                      fleet = as.factor(1))
    tmp <- dplyr::mutate(tmp, date = as.Date(paste0('01/',month,'/',year), format = '%d/%m/%Y'))
    tmp <- dplyr::select(tmp, !c(year, month))
    tmp <- dplyr::mutate(tmp, scenario = scenario)

    data <- rbind(data, tmp)
  }
  rownames(data) <- NULL
  return(data)
}
