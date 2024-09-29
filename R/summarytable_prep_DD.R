# Copyright 2024 Fisheries Queensland

# This file is part of SSAND.
# SSAND is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
# SSAND is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
# You should have received a copy of the GNU General Public License along with SSAND. If not, see <https://www.gnu.org/licenses/>.

#' Prepare DDUST data for summary table
#'
#' @param dd_mle A list of outputs from DDUST::makefullreport() with one list element per scenario. Will automatically reformat as a list if a single DDUST::makefullreport() output (i.e. one scenario) is entered
#' @param dd_mcmc A list of model fits from tmbstan::tmbstan() with one list element per scenario. Only needed if MCMC was used.
#' @param dd_sim A list of outputs from SSAND::simulate_DD() with one list element per scenario. Only required if MCMC was used. Will automatically reformat as a list if a single DDUST::makefullreport() output (i.e. one scenario) is entered
#' @param credible_interval The credible interval to use to create dashed lines in the final plot
#' @param end_year The final year of biomass to be plotted. Calculated by default, but option is included in case model is run into the future.
#'
#' @return A data frame that summarised biomass and fishing mortality estimates, ready for use in summarytable()
#' @export
#'
#' @examples
#' \dontrun{
#' library(DDUST)
#' dd_sim <- simulate_DDUST(dd_mle,dd_mcmc)
#'
#' # <<table_summary, results='asis', echo=FALSE>>=
#' dd_mcmc_ens <- mcmc_ensemble_DD(dd_mcmc,dd_sim,scenarios=c(1,2))$dd_mcmc
#' dd_sim_ens <- mcmc_ensemble_DD(dd_mcmc,dd_sim,scenarios=c(1,2))$dd_sim
#' data <- summarytable_prep_DD(dd_mle,dd_mcmc_ens,dd_sim_ens)
#' summarytable(data, label="tab:summary")
#' # @
#' }
summarytable_prep_DD <- function(dd_mle,
                                 dd_mcmc,
                                 dd_sim,
                                 end_year = NULL,
                                 credible_interval = 0.95) {

  if (check_scenarios(dd_mle,"DD","MLE")=="single scenario"){dd_mle <- list(dd_mle)}
  if (check_scenarios(dd_mcmc,"DD","MCMC")=="single scenario"){dd_mcmc <- list(dd_mcmc)}
  if (check_scenarios(dd_sim,"DD","SIM")=="single scenario"){dd_sim <- list(dd_sim)}

  if (missing(end_year)) {end_year <- dd_mle$data$last_year_catch}

  data <- report_values_DD(dd_mle,dd_mcmc,dd_sim,
                           F_relative_to_MSY = TRUE)

  # Prepare table
  indicator <- c(
    "\\rowcolor{light-gray} Biomass ratio (relative to unfished)",
    "\\rowcolor{white} \\hspace{5 mm}Range (95\\% credible interval)",
    "\\rowcolor{white} \\hspace{5 mm}Probability below 20\\%",
    "\\rowcolor{white} \\hspace{5 mm}Probability between 20\\% and 40\\%",
    "\\rowcolor{white} \\hspace{5 mm}Probability between 40\\% and 60\\%",
    "\\rowcolor{white} \\hspace{5 mm}Probability above 60\\%",
    "\\rowcolor{light-gray} Fishing pressure ratio (relative to MSY)",
    "\\rowcolor{white} \\hspace{5 mm}Range (95\\% credible interval)",
    "\\rowcolor{white} \\hspace{5 mm}Probability exceeds $F_{MSY}$"
  )

  value <- c(
    "",
    paste0(data$biomass_summary_lower,"--",data$biomass_summary_upper,"\\%"),
    paste0(data$biomass_risk20,"\\%"),
    paste0(data$biomass_risk20_40,"\\%"),
    paste0(data$biomass_risk40_60,"\\%"),
    paste0(data$biomass_risk60,"\\%"),
    "",
    paste0(data$F_summary_lower,"--",data$F_summary_upper),
    paste0(data$F_summary_risk,"\\%")
  )

  data <- data.frame(Indicator = indicator, Value = value)

  return(data)
}
