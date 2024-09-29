#' Prepare Stock Synthesis data for biomass table
#'
#' @param ss_mle A list of outputs from r4ss::SS_output() with one element per scenario. Will automatically reformat as a list if a single r4ss::SS_output() output (i.e. one scenario) is entered.
#' @param ss_mcmc A list of outputs from r4ss::SSgetMCMC() with one element per scenario. Only needed if MCMC was used. Will automatically reformat as a list if a single r4ss::SSgetMCMC() output (i.e. one scenario) is entered.
#' @param credible_interval The credible interval to use to create dashed lines in the final plot
#' @param end_year The final year of biomass to be plotted. Calculated by default, but option is included in case model is run into the future.
#' @param scenarios A vector of scenarios to plot (numeric). Shows all scenarios if left blank. Can be overridden in the plotting function.
#' @param period Optional: time period of stock assessment.
#' @param biomass_type The type of biomass used. Options are "relative" or "absolute" (character).
#' @param biomass_definition The definition of biomass used. Options are "spawning" or "vulnerable" (character).
#' @param selectivity_fleet Fleet number used for the selectivity applied to calculate vulnerable biomass.
#'
#' @return A data frame that summarises biomass estimate, ready for use in biomasstable()
#' @export
#'
#' @examples
#' # <<table_biomass, results='asis', echo=FALSE>>=
#' data <- biomasstable_prep_SS(ss_mle,ss_mcmc)
#' biomasstable(data, label="tab:biomass")
#' # @
biomasstable_prep_SS <- function(ss_mle,
                                 ss_mcmc,
                                 end_year = NULL,
                                 credible_interval = 0.95,
                                 scenarios = NULL,
                                 period = NULL,
                                 biomass_type = "relative",
                                 biomass_definition = "spawning",
                                 selectivity_fleet = 2) {

  if (missing(end_year)) {end_year <- ss_mle[[1]]$endyr+1}
  if (missing(scenarios)){scenarios <- 1:length(ss_mle)}
  if (missing(period)) {period = (ss_mle[[1]]$startyr+1) : end_year}

  MLE <- biomassplot_prep_SS(ss_mle,
                             scenarios = scenarios,
                             intervals = c(credible_interval,0.1), # needs to be at least 1 element long
                             period = period,
                             biomass_type = biomass_type,
                             biomass_definition = biomass_definition,
                             selectivity_fleet = selectivity_fleet) |>
    dplyr::filter(year==end_year) |>
    dplyr::select(scenario,median=value,lower,upper)



  MCMC <- biomassplot_prep_SS(ss_mle,
                              ss_mcmc,
                              scenarios = scenarios,
                              intervals = c(credible_interval,0.1), # needs to be at least 1 element long
                              period = period,
                              biomass_type = biomass_type,
                              biomass_definition = biomass_definition,
                              selectivity_fleet = selectivity_fleet) |>
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
