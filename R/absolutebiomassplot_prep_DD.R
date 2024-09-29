# Copyright 2024 Fisheries Queensland

# This file is part of SSAND.
# SSAND is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
# SSAND is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
# You should have received a copy of the GNU General Public License along with SSAND. If not, see <https://www.gnu.org/licenses/>.

#' Prepare DDUST data for absolute biomass plot
#'
#' @param dd_mle A list of outputs from DDUST::makefullreport() with one list element per scenario. Will automatically reformat as a list if a single DDUST::makefullreport() output (i.e. one scenario) is entered
#' @param dd_mcmc Placeholder. No functionality for MCMC build yet. A list of model fits from tmbstan::tmbstan() with one list element per scenario. Only needed if MCMC was used.
#' @param dd_sim Placeholder. No functionality for MCMC build yet. A list of outputs from SSAND::simulate_DD() with one list element per scenario. Only required if MCMC was used. Will automatically reformat as a list if a single DDUST::makefullreport() output (i.e. one scenario) is entered
#' @param scenarios A vector of scenarios to plot (numeric). Shows all scenarios if left blank. Can be overridden in the plotting function.
#'
#' @return A data frame with date, fleet, obs, exp, ub, lb, scenario
#' @export
#'
#' @examples
#' \dontrun{
#' data <- absolutebiomassplot_prep_DD(dd_mle)
#' absolutebiomassplot(data)
#' }
absolutebiomassplot_prep_DD <- function (dd_mle,
                                         dd_mcmc = NULL,
                                         dd_sim = NULL,
                                         scenarios = NULL) {
  if (missing(dd_mcmc)) {MCMC = FALSE} else {MCMC = TRUE}

  if (check_scenarios(dd_mle, "DD", "MLE") == "single scenario") {dd_mle <- list(dd_mle)}
  if (MCMC && check_scenarios(dd_mcmc, "DD", "MCMC") == "single scenario") {dd_mcmc <- list(dd_mcmc)}
  if (MCMC && check_scenarios(dd_mcmc, "DD", "SIM") == "single scenario") {dd_sim <- list(dd_sim)}
  if (missing(scenarios)) {scenarios <- 1:length(dd_mle)}

  if (!MCMC) {
    data.scenarios <- data.frame()
    for (scenario in scenarios) {
      numfleet <- nrow(dd_mle[[scenario]]$data$absolute_biomass)
      Nm <- dd_mle[[scenario]]$data$Number_months_per_timestep
      year_month_df <- data.frame(year = dd_mle[[scenario]]$data$first_year_catch,
                                  month = 1)
      year_month_df <- tidyr::complete(year_month_df,
                                       year = dd_mle[[scenario]]$data$first_year_catch:dd_mle[[scenario]]$data$last_year_catch,
                                       month = seq(1, 12, Nm))
      data <- data.frame()

      for (i in 1:numfleet) {
        sd <- dd_mle[[scenario]]$data$absolute_biomass_sd
        temp <- data.frame(year = sort(rep(dd_mle[[scenario]]$data$first_year_catch:dd_mle[[scenario]]$data$last_year_catch, 12/Nm)),
                           month = seq(1, 12, Nm),
                           fleet = i,
                           obs = dd_mle[[scenario]]$data$absolute_biomass[i, ],
                           exp = matrix(dd_mle[[scenario]]$B_annual, nrow = numfleet)[i, ],
                           sd = as.numeric(sd)) |>
          dplyr::mutate(ub = obs + 1.96 * sd,
                        lb = obs - 1.96 * sd) |>
          dplyr::select(-sd)

        temp <- dplyr::right_join(temp, year_month_df,
                                  by = dplyr::join_by(year, month))
        temp <- dplyr::mutate(temp, date = as.Date(paste0("01/",
                                                          month, "/", year), format = "%d/%m/%Y"), obs = ifelse(obs ==
                                                                                                                  0, NA, obs))
        temp <- dplyr::mutate(temp, ub = ifelse(is.na(obs),
                                                NA, ub), lb = ifelse(is.na(obs), NA, lb))
        minDate <- min(temp$date[!is.na(temp$obs)])
        maxDate <- max(temp$date[!is.na(temp$obs)])
        temp <- dplyr::filter(temp, date >= minDate,
                              date <= maxDate)
        data <- rbind(data, temp)
      }
      data <- cbind(data, scenario = scenario)
      data.scenarios <- rbind(data.scenarios, data)
    }
    data <- dplyr::select(data.scenarios, c(date, fleet, obs, exp, ub, lb, scenario))
  }
  return(data)
}
