# Copyright 2024 Fisheries Queensland

# This file is part of SSAND.
# SSAND is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
# SSAND is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
# You should have received a copy of the GNU General Public License along with SSAND. If not, see <https://www.gnu.org/licenses/>.

#' Prepare Stock Synthesis data for summary table
#'
#' @param ss_mle A list of outputs from r4ss::SS_output() with one element per scenario. Will automatically reformat as a list if a single r4ss::SS_output() output (i.e. one scenario) is entered.
#' @param ss_mcmc A list of outputs from r4ss::SSgetMCMC() with one element per scenario. Only needed if MCMC was used. Will automatically reformat as a list if a single r4ss::SSgetMCMC() output (i.e. one scenario) is entered.
#' @param credible_interval The credible interval to use to create dashed lines in the final plot
#' @param end_year The final year of biomass to be plotted. Calculated by default, but option is included in case model is run into the future.
#' @param number_F_years Number of recent years to be included in the fishing pressure summary (e.g. number_F_years=5 will take the average of the five most recent years of the model)
#'
#' @return A data frame that summarised parameter estimate, ready for use in parametertable()
#' @export
#'
#' @examples
#' # <<table_summary, results='asis', echo=FALSE>>=
#' data <- summarytable_prep_SS(ss_mle[[1]],ss_mcmc[[1]])
#' summarytable(data, label="tab:summary")
#' # @
#'
#' # <<table_summary, results='asis', echo=FALSE>>=
#' ss_mcmc_ens <- mcmc_ensemble_SS(ss_mcmc,scenarios=c(1,2))
#' data <- summarytable_prep_SS(ss_mle,ss_mcmc_ens)
#' summarytable(data, label="tab:summary")
#' # @
summarytable_prep_SS <- function(ss_mle,
                                 ss_mcmc,
                                 end_year = NULL,
                                 credible_interval = 0.95,
                                 number_F_years = 5) {

  if (check_scenarios(ss_mcmc,"SS","MCMC")=="single scenario"){ ss_mcmc <- list(ss_mcmc)}

  if (check_scenarios(ss_mle,"SS","MLE")=="single scenario"){ ss_mle <- list(ss_mle) }

  if (missing(end_year)) {
    end_year <- ss_mle[[1]]$endyr+1
  }

  data <- report_values_SS(ss_mle,ss_mcmc,
                           credible_interval = credible_interval,
                           number_F_years = number_F_years)

  indicator <- c(
    "\\rowcolor{light-gray} Biomass ratio (relative to unfished)",
    "\\rowcolor{white} \\hspace{5 mm}Range (95\\% credible interval)",
    "\\rowcolor{white} \\hspace{5 mm}Probability below 20\\%",
    "\\rowcolor{white} \\hspace{5 mm}Probability between 20\\% and 40\\%",
    "\\rowcolor{white} \\hspace{5 mm}Probability between 40\\% and 60\\%",
    "\\rowcolor{white} \\hspace{5 mm}Probability above 60\\%",
    "\\rowcolor{light-gray} Fishing pressure ratio (relative to MSY)",
    "\\rowcolor{white} \\hspace{5 mm}Range (95\\% credible interval)"
  )

  value <- c(
    "",
    paste0(data$biomass_summary_lower,"--",data$biomass_summary_upper,"\\%"),
    paste0(data$biomass_risk20,"\\%"),
    paste0(data$biomass_risk20_40,"\\%"),
    paste0(data$biomass_risk40_60,"\\%"),
    paste0(data$biomass_risk60,"\\%"),
    "",
    paste0(data$F_summary_lower,"--",data$F_summary_upper)
  )

  data <- data.frame(Indicator = indicator, Value = value)

  if (
    (!(is.null(ss_mle[[1]]$F_report_basis)) && ss_mle[[1]]$F_report_basis == "(F)/(Fmsy);_with_F=Exploit(bio)") ||  # The old variable name
    (!(is.null(ss_mle[[1]]$F_std_basis))    && ss_mle[[1]]$F_std_basis    == "(F)/(Fmsy);_with_F=Exploit(bio)")
  ) {
    data <- data |>
      rbind(
        data.frame(Indicator = "\\hspace{5 mm}Probability exceeds $F_{MSY}$", Value = paste0(data$F_summary_risk,"\\%"))
      )
  }

  return(data)
}
