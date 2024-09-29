# Copyright 2024 Fisheries Queensland

# This file is part of SSAND.
# SSAND is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
# SSAND is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
# You should have received a copy of the GNU General Public License along with SSAND. If not, see <https://www.gnu.org/licenses/>.

#' Prepare Stock Synthesis data for dataplot()
#'
#' @param ss_mle A single model output from r4ss::SS_output()
#' @param scenarios A vector of scenarios to plot (numeric). Shows all scenarios if left blank. Can be overridden in the plotting function.
#' @param financial_year Set to TRUE if the assessment was based on financial year (logical).
#'
#' @return A data object with variables called yr (int/num), typename (factor), size (numeric), fleetnames (factor)
#' @export
#'
#' @examples
#' data <- dataplot_prep_SS(ss_mle)
#' dataplot(data)
dataplot_prep_SS <- function(ss_mle,
                             scenarios = NULL,
                             financial_year=FALSE) {

  if (check_scenarios(ss_mle,"SS","MLE")=="single scenario"){ss_mle <- list(ss_mle); warning("Assuming you are entering a single scenario, not a list of scenarios. Wrap ss_mle input inside a list() to avoid this warning.")}
  if (missing(scenarios)){scenarios <- 1:length(ss_mle)}


  data <- data.frame()
  for (scenario in scenarios) {

    # Catch
    if (nrow(ss_mle[[scenario]]$catch)>0) {
      data_catch <- ss_mle[[scenario]]$catch |>
        dplyr::select(fleet=Fleet,year=Yr,Obs) |>
        dplyr::group_by(fleet) |>
        dplyr::mutate(size = Obs/max(Obs),
                      typename = "Retained catch",
                      scenario = scenario) |>
        dplyr::ungroup() |>
        dplyr::select(year,typename,size,fleet,scenario) |>
        dplyr::filter(size>0)

      data <- rbind(data,data_catch)
    }

    # Abundance indices
    if (nrow(ss_mle[[scenario]]$cpue)>0) {
      data_cpue <- ss_mle[[scenario]]$cpue |>
        dplyr::select(fleet=Fleet,year=Yr,Obs) |>
        dplyr::mutate(size = 1,
                      typename = "Abundance indices",
                      scenario = scenario) |>
        dplyr::select(year,typename,size,fleet,scenario)

      data <- rbind(data,data_cpue)
    }

    # Length compositions
    if (nrow(ss_mle[[scenario]]$lendbase)>0) {
      data_length <- ss_mle[[scenario]]$lendbase |>
        dplyr::select(fleet=Fleet,year=Yr,Nsamp_adj) |>
        dplyr::group_by(fleet, year) |>
        dplyr::summarise(Nsamp_adj = dplyr::first(Nsamp_adj), .groups='drop') |>
        dplyr::mutate(size = Nsamp_adj / max(Nsamp_adj),
                      typename = "Length compositions",
                      scenario = scenario) |>
        dplyr::select(year,typename,size,fleet,scenario)
      data <- rbind(data,data_length)
    }

    # Age compositions
    if (nrow(ss_mle[[scenario]]$agedbase)>0) {
      data_age <- ss_mle[[scenario]]$agedbase |>
        dplyr::select(fleet=Fleet,year=Yr,Nsamp_adj) |>
        dplyr::group_by(fleet, year) |>
        dplyr::summarise(Nsamp_adj = dplyr::first(Nsamp_adj), .groups='drop') |>
        dplyr::mutate(size = Nsamp_adj / max(Nsamp_adj),
                      typename = "Age compositions",
                      scenario = scenario) |>
        dplyr::select(year,typename,size,fleet,scenario)
      data <- rbind(data,data_age)
    }

    # Conditional age-at-length compositions
    if (nrow(ss_mle[[scenario]]$condbase)>0) {
      data_caal <- ss_mle[[scenario]]$condbase |>
        dplyr::group_by(Fleet,Yr,Sexes,Lbin_lo,Lbin_hi) |>
        dplyr::summarise(Nsamp_adj=dplyr::first(Nsamp_adj), .groups = 'drop') |>
        dplyr::group_by(Fleet,Yr) |>
        dplyr::summarise(Nsamp_adj= sum(Nsamp_adj), .groups = 'drop')|>
        dplyr::mutate(size = Nsamp_adj / max(Nsamp_adj),
                      typename = "Conditional age-at-length compositions",
                      scenario = scenario) |>
        dplyr::select(year=Yr,typename,size,fleet=Fleet,scenario)
      data <- rbind(data,data_caal)
    }

    # Ghost conditional age-at-length compositions
    if (nrow(ss_mle[[scenario]]$ghostcondbase)>0) {
      data_ghostcondbase <- ss_mle[[scenario]]$ghostcondbase |>
        dplyr::group_by(Fleet,Yr,Sexes,Lbin_lo,Lbin_hi) |>
        dplyr::summarise(Nsamp_adj=dplyr::first(Nsamp_adj), .groups = 'drop') |>
        dplyr::group_by(Fleet,Yr) |>
        dplyr::summarise(Nsamp_adj= sum(Nsamp_adj), .groups = 'drop')|>
        dplyr::mutate(size = Nsamp_adj / max(Nsamp_adj),
                      typename = "Ghost conditional age-at-length compositions",
                      scenario = scenario) |>
        dplyr::select(year=Yr,typename,size,fleet=Fleet,scenario)
      data <- rbind(data,data_ghostcondbase)
    }

    # Mean length-at-age
    if (nrow(ss_mle[[scenario]]$ladbase)>0) {
      data_ladbase <- ss_mle[[scenario]]$ladbase |>
        dplyr::select(fleet=Fleet,year=Yr,Nsamp_adj) |>
        dplyr::group_by(fleet, year) |>
        dplyr::summarise(Nsamp_adj = dplyr::first(Nsamp_adj), .groups='drop') |>
        dplyr::mutate(size = Nsamp_adj / max(Nsamp_adj),
                      typename = "Mean length-at-age",
                      scenario = scenario) |>
        dplyr::select(year,typename,size,fleet,scenario)
      data <- rbind(data,data_ladbase)
    }

    # Mean weight-at-age
    if (nrow(ss_mle[[scenario]]$wadbase)>0) {
      data_wadbase <- ss_mle[[scenario]]$wadbase |>
        dplyr::select(fleet=Fleet,year=Yr,Nsamp_adj) |>
        dplyr::group_by(fleet, year) |>
        dplyr::summarise(Nsamp_adj = dplyr::first(Nsamp_adj), .groups='drop') |>
        dplyr::mutate(size = Nsamp_adj / max(Nsamp_adj),
                      typename = "Mean weight-at-age",
                      scenario = scenario) |>
        dplyr::select(year,typename,size,fleet,scenario)
      data <- rbind(data,data_wadbase)
    }

    # Tag releases
    if (!is.null(ss_mle[[scenario]]$tagrelease)) {
      data_tagrelease <- ss_mle[[scenario]]$tagrelease |>
        dplyr::select(fleet=Fleet,year=Yr,Nrelease) |>
        dplyr::group_by(fleet, year) |>
        dplyr::summarise(Nrelease = dplyr::first(Nrelease), .groups='drop') |>
        dplyr::mutate(size = Nrelease / max(Nrelease),
                      typename = "Tag releases",
                      scenario = scenario) |>
        dplyr::select(year,typename,size,fleet,scenario)
      data <- rbind(data,data_tagrelease)
    }

    # Tag recaptures
    if (nrow(ss_mle[[scenario]]$tagdbase1)>0) {
      data_tagdbase1 <- ss_mle[[scenario]]$tagdbase1 |>
        dplyr::select(fleet=Fleet,year=Yr,Obs) |>
        dplyr::group_by(fleet, year) |>
        dplyr::summarise(Obs = dplyr::first(Obs), .groups='drop') |>
        dplyr::mutate(size = Obs / max(Obs),
                      typename = "Tag recaptures",
                      scenario = scenario) |>
        dplyr::select(year,typename,size,fleet,scenario)
      data <- rbind(data,data_tagdbase1)
    }

    # Discards
    if (nrow(ss_mle[[scenario]]$discard)>0) {
      data_discard <- ss_mle[[scenario]]$discard |>
        dplyr::select(fleet=Fleet,year=Yr,Obs) |>
        dplyr::mutate(size = 1,
                      typename = "Discards",
                      scenario = scenario) |>
        dplyr::select(year,typename,size,fleet,scenario)

      data <- rbind(data,data_discard)
    }

    # Ghost age compositions
    if (nrow(ss_mle[[scenario]]$ghostagedbase)>0) {
      data_ghostagedbase <- ss_mle[[scenario]]$ghostagedbase |>
        dplyr::select(fleet=Fleet,year=Yr) |>
        dplyr::mutate(size = 1,
                      typename = "Ghost age compositions",
                      scenario = scenario) |>
        dplyr::select(year,typename,size,fleet,scenario)

      data <- rbind(data,data_ghostagedbase)
    }

    # Ghost length compositions
    if (nrow(ss_mle[[scenario]]$ghostlendbase)>0) {
      data_ghostlendbase <- ss_mle[[scenario]]$ghostlendbase |>
        dplyr::select(fleet=Fleet,year=Yr) |>
        dplyr::mutate(size = 1,
                      typename = "Ghost length compositions",
                      scenario = scenario) |>
        dplyr::select(year,typename,size,fleet,scenario)

      data <- rbind(data,data_ghostlendbase)
    }
  }
  # data_all <- rbind(data_all,data)

  data <- data |>
    dplyr::mutate(typename = factor(typename,
                                    levels = c("Retained catch", "Abundance indices",
                                               "Length compositions", "Size compositions",
                                               "Age compositions", "Conditional age-at-length compositions",
                                               "Ghost age compositions", "Ghost conditional age-at-length compositions",
                                               "Ghost length compositions", "Mean length-at-age",
                                               "Mean weight-at-age", "Mean body weight", "Discards",
                                               "Tag releases", "Tag recaptures")))

  if (financial_year==T) {data$year <- data$year + 1}

  data <- data |>
    dplyr::mutate(scenario = scenario)|>
    dplyr::mutate(fleet = as.factor(fleet))

  # data <- data_all
  rownames(data) <- NULL
  return(data)
}
