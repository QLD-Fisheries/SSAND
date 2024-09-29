#' Prepare Stock Synthesis data for likelihood table
#'
#' @param ss_mle A list of outputs from r4ss::SS_output() with one element per scenario. Will automatically reformat as a list if a single r4ss::SS_output() output (i.e. one scenario) is entered.
#' @param scenarios A vector of scenarios to plot (numeric). Shows all scenarios if left blank. Can be overridden in the plotting function.
#' @param components A vector of likelihood components to show. Options are "TOTAL", "Catch", "Equil_catch", "Survey", "Discard", "Length_comp", "Age_comp", "Recruitment", "InitEQ_Regime", "Forecast_Recruitment", "Parm_priors", "Parm_softbounds", "Parm_devs", "Crash_Pen"
#'
#' @return A data frame that summarises likelihood components
#' @export
#'
#' @examples
#' dat <- likelihoodtable_prep_SS(ss_mle)
#' dat <- likelihoodtable_prep_SS(ss_mle,
#'   components = c("TOTAL", "Survey", "Length_comp","Age_comp","Recruitment"))
likelihoodtable_prep_SS <- function(ss_mle,
                                    scenarios = NULL,
                                    components = NULL) {

  if (missing(scenarios)){scenarios <- 1:length(ss_mle)}

  data <- data.frame()
  for (scenario in scenarios) {

    tmp <- ss_mle[[scenario]]$likelihoods_used |>
      dplyr::select(values) |>
      tibble::rownames_to_column("component") |>
      dplyr::mutate(scenario = scenario)

    data <- rbind(data,tmp)
  }

  if (!missing(components)) {data <- data |> dplyr::filter(component %in% components)}

  data <- data |>
    tidyr::pivot_wider(names_from = "component", values_from = "values")

  return(data)
}
