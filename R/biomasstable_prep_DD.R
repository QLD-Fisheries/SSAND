#' Prepare DDUST data for biomass table
#'
#' @param dd_mle A list of outputs from DDUST::makefullreport() with one list element per scenario. Will automatically reformat as a list if a single DDUST::makefullreport() output (i.e. one scenario) is entered
#' @param dd_mcmc A list of model fits from tmbstan::tmbstan() with one list element per scenario. Only needed if MCMC was used.
#' @param dd_sim A list of outputs from DDUST::simulate_DD() with one list element per scenario. Only required if MCMC was used. Will automatically reformat as a list if a single DDUST::makefullreport() output (i.e. one scenario) is entered
#' @param credible_interval The credible interval to use to create dashed lines in the final plot
#' @param end_year The final year of biomass to be plotted. Calculated by default, but option is included in case model is run into the future.
#' @param scenarios A vector of scenarios to plot (numeric). Shows all scenarios if left blank. Can be overridden in the plotting function.
#' @param biomass_type The type of biomass used. Options are "relative" or "absolute" (character).
#' @param period Optional: time period of stock assessment.
#'
#' @return A data frame that summarised biomass estimate, ready for use in biomasstable()
#' @export
#'
#' @examples
#' \dontrun{
#' library(DDUST)
#' dd_sim <- simulate_DDUST(dd_mle,dd_mcmc)
#'
#' # <<table_biomass, results='asis', echo=FALSE>>=
#' data <- biomasstable_prep_DD(dd_mle,dd_mcmc,dd_sim)
#' biomasstable(data, label="tab:biomass")
#' # @
#' }
biomasstable_prep_DD <- function(dd_mle,
                                 dd_mcmc,
                                 dd_sim,
                                 end_year = NULL,
                                 credible_interval = 0.95,
                                 scenarios = NULL,
                                 period = NULL,
                                 biomass_type = "relative") {

  if (missing(end_year)) {end_year <- dd_mle[[1]]$data$last_year_catch}
  if (missing(scenarios)){scenarios <- 1:length(dd_mle)}
  if (missing(period)) {period = dd_mle[[1]]$data$first_year_catch : end_year}

  if (check_scenarios(dd_mle,"DD","MLE")=="single scenario"){dd_mle <- list(dd_mle)}
  if (check_scenarios(dd_mcmc,"DD","MCMC")=="single scenario"){dd_mcmc <- list(dd_mcmc)}
  if (check_scenarios(dd_sim,"DD","SIM")=="single scenario"){dd_sim <- list(dd_sim)}

  MLE <- biomassplot_prep_DD(dd_mle,
                             scenarios = scenarios,
                             biomass_type = biomass_type) |>
    dplyr::filter(year==end_year) |>
    dplyr::select(scenario,median=value,lower,upper)



  MCMC <- biomassplot_prep_DD(dd_mle,
                              dd_mcmc,
                              dd_sim,
                              scenarios = scenarios,
                              intervals = c(credible_interval,0.1), # needs to be at least 1 element long
                              biomass_type = biomass_type) |>
    dplyr::filter(year==end_year,
                  med=="MCMC") |>
    dplyr::group_by(scenario) |>
    dplyr::summarise(median = quantile(value,0.5),
                     lower = quantile(value,(1-credible_interval)/2),
                     upper = quantile(value,1-(1-credible_interval)/2))

  data <- MLE |>
    dplyr::left_join(MCMC, by = "scenario")

  names(data) <- c("",
                   paste0("$B_{",end_year,"}\\%$"),
                   paste0("$B_{",end_year,",lower}\\%$"),
                   paste0("$B_{",end_year,",upper}\\%$"),
                   paste0("$B_{",end_year,"}\\%$"),
                   paste0("$B_{",end_year,",lower}\\%$"),
                   paste0("$B_{",end_year,",upper}\\%$"))

  data <- list(data,period[1],end_year)

  return(data)
}
