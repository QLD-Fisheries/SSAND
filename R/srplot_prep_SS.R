# Copyright 2024 Fisheries Queensland

# This file is part of SSAND.
# SSAND is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
# SSAND is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
# You should have received a copy of the GNU General Public License along with SSAND. If not, see <https://www.gnu.org/licenses/>.

#' Prepare Stock Synthesis data for stock-recruit curve plot
#'
#' @param ss_mle A list of outputs from r4ss::SS_output() with one element per scenario. Will automatically reformat as a list if a single r4ss::SS_output() output (i.e. one scenario) is entered.
#' @param ss_mcmc A list of outputs from r4ss::SSgetMCMC() with one element per scenario. Only needed if MCMC was used. Will automatically reformat as a list if a single r4ss::SSgetMCMC() output (i.e. one scenario) is entered.
#' @param scenarios A vector of scenarios to plot (numeric). Shows all scenarios if left blank. Can be overridden in the plotting function.
#'
#' @return A data frame with variables year (int), spawn_bio (num), pred_rec (num), dev (num), exp_recr (num), bias_adjusted (num), scenario (factor)
#' @export
#'
#' @examples
#' data <- srplot_prep_SS(ss_mle)
#' srplot(data)
srplot_prep_SS <- function(ss_mle,
                           ss_mcmc = NULL,
                           scenarios = NULL) {

  if (missing(ss_mcmc)) {MCMC = FALSE} else {MCMC = TRUE}
  if (check_scenarios(ss_mle,"SS","MLE")=="single scenario"){ss_mle <- list(ss_mle); warning("Assuming you are entering a single scenario, not a list of scenarios. Wrap ss_mle input inside a list() to avoid this warning.")}
  if (MCMC && check_scenarios(ss_mcmc,"SS","MCMC")=="single scenario"){ss_mcmc <- list(ss_mcmc); warning("Assuming you are entering a single scenario, not a list of scenarios. Wrap ss_mcmc input inside a list() to avoid this warning.")}

  if (missing(scenarios)){scenarios <- 1:length(ss_mle)}


  if (!MCMC) {
    data <- data.frame()
    for (scenario in scenarios) {

      nsexes <- ss_mle[[scenario]]$nsexes
      if (nsexes == 1) bioscale <- 0.5 else bioscale <- 1

      minyr <- min(ss_mle[[scenario]]$recruit$Yr)

      recruit <- ss_mle[[scenario]]$recruit |>
        dplyr::filter(era %in% c("Fixed","Main","Fore",NA) & Yr >= minyr) |>
        dplyr::mutate(spawn_bio = bioscale * SpawnBio)

      timeseries <- ss_mle[[scenario]]$timeseries |>
        dplyr::mutate(SpawnBio = bioscale * SpawnBio)

      # only add lines for environmentally dependent recruitment if it differs
      # from expected recruitment without environmental link
      show_env <- TRUE & any(recruit$with_env != recruit$exp_recr)

      # store virgin and initial values
      B0 <- sum(timeseries$SpawnBio[timeseries$Era == "VIRG"], na.rm = TRUE)
      B1 <- sum(timeseries$SpawnBio[timeseries$Era == "INIT"], na.rm = TRUE)
      R0 <- sum(timeseries$Recruit_0[timeseries$Era == "VIRG"], na.rm = TRUE)
      R1 <- sum(timeseries$Recruit_0[timeseries$Era == "INIT"], na.rm = TRUE)

      # work around for issue with Shepherd function producing 0 values in equilibrium
      # use first non-zero value for each
      if (B0 == 0) {
        B0 <- head(recruit$spawn_bio[recruit$spawn_bio != 0], 1)
      }
      if (R0 == 0) {
        R0 <- head(recruit$exp_recr[recruit$exp_recr != 0], 1)
      }
      if (B0 == B1 & R0 == R1) {
        init <- FALSE
      }

      recruit <- recruit |>
        dplyr::mutate(exp_recr = exp_recr/R0,
                      bias_adjusted = bias_adjusted/R0,
                      pred_recr = pred_recr/R0,
                      spawn_bio = spawn_bio/B0)



      recruit <- recruit |>
        dplyr::filter(!era == "Fore") |>
        dplyr::select(year=Yr, spawn_bio, pred_recr, exp_recr, dev, bias_adjusted) |>
        dplyr::mutate(scenario = scenario)

      data <- rbind(data, recruit)
    }
  }


  if (MCMC) {

    warning("Stock Synthesis MCMC functionality not yet built for SR curve")
    #
    #     data_all <- NULL
    #     for(scenario in scenarios) {
    #
    #       # h value (1 = estimated, !1 = fixed)
    #       col = scenario_list$AssessNr[i]
    #       h <- fullparamlist[fullparamlist$Label == "SR_BH_steep", eval(col)]
    #
    #       # MCMC output
    #       mcmc_ss_mle <- SSgetMCMC(dir = scenario_list$path[i])
    #
    #       # take 1000 samples from MCMC outout
    #       set.seed(42)
    #       sampdata <- mcmc_ss_mle[sample(nrow(mcmc_ss_mle), size=1000), ]
    #
    #       tmpdata <- sampdata |>
    #         select(contains("Bratio_"))
    #
    #       # find median value of Biomass in the last year of assessment
    #       pos <- which(tmpdata[, ncol(tmpdata)] == median(tmpdata[1:(nrow(tmpdata)-1), ncol(tmpdata)]))
    #
    #       tmpssb <- sampdata |>
    #         dplyr::select(tidyr::starts_with("SSB_")) |>
    #         dplyr::select(!c("SSB_Initial", "SSB_unfished", "SSB_Btgt", "SSB_SPR","SSB_MSY"))
    #       tmpssb[, 2:ncol(tmpssb)] <- tmpssb[, 2:ncol(tmpssb)]/tmpssb$SSB_Virgin
    #
    #       tmpssb <- tmpssb  |>
    #         dplyr::select(!SSB_Virgin) |>
    #         dplyr::mutate(
    #           Iter = dplyr::row_number(),
    #           assessnr = scenario_list$AssessNr[i],
    #           scenario = scenario_list$scenario[i],
    #           med = ifelse(Iter==pos,"Median","MCMC")) |>
    #         tidyr::pivot_longer(cols=-c(Iter,assessnr, scenario, med),
    #                             names_to = "Label", values_to = "spawn_bio") |>
    #         tidyr::separate(Label, c("Label", "Year"), sep="_") |>
    #         dplyr::select(!Label) |>
    #         dplyr::mutate(Year = as.integer(Year))
    #
    #       tmppredrec <- sampdata |>
    #         dplyr::select(tidyr::starts_with("Recr_")) |>
    #         dplyr::select(!c("Recr_Initial", "Recr_unfished"))
    #       tmppredrec[, 2:ncol(tmppredrec)] <- tmppredrec[, 2:ncol(tmppredrec)]/tmppredrec$Recr_Virgin
    #
    #       tmppredrec <- tmppredrec  |>
    #         dplyr::select(!Recr_Virgin) |>
    #         dplyr::mutate(
    #           Iter = dplyr::row_number(),
    #           assessnr = scenario_list$AssessNr[i],
    #           scenario = scenario_list$scenario[i],
    #           med = ifelse(Iter==pos,"Median","MCMC")) |>
    #         tidyr::pivot_longer(cols=-c(Iter,assessnr, scenario, med),
    #                             names_to = "Label", values_to = "pred_recr") |>
    #         tidyr::separate(Label, c("Label", "Year"), sep="_") |>
    #         dplyr::select(!Label) |>
    #         dplyr::mutate(Year = as.integer(Year))
    #
    #       # if h is fixed, add SR_BH_steep in the sample data
    #       if(h!=1) {
    #         sampdata$SR_BH_steep <- h
    #       }
    #
    #       tmpexprec <- sampdata |>
    #         dplyr::select(tidyr::starts_with("SSB_"), Recr_unfished, SR_BH_steep) |>
    #         dplyr::select(!c("SSB_Virgin", "SSB_Initial", "SSB_Btgt", "SSB_SPR","SSB_MSY")) |>
    #         dplyr::mutate(
    #           Iter = dplyr::row_number(),
    #           assessnr = scenario_list$AssessNr[i],
    #           scenario = scenario_list$scenario[i],
    #           med = ifelse(Iter==pos,"Median","MCMC")) |>
    #         tidyr::pivot_longer(cols=-c(Iter,assessnr, scenario, med, SSB_unfished,
    #                                     Recr_unfished, SR_BH_steep),
    #                             names_to = "Label", values_to = "SBy") |>
    #         tidyr::separate(Label, c("Label", "Year"), sep="_") |>
    #         # calculate expected recruitment using BH function
    #         dplyr::mutate(Year = as.integer(Year),
    #                       exp_recr = (4*SR_BH_steep *Recr_unfished*SBy/(SSB_unfished*(1-SR_BH_steep) + SBy*(5*SR_BH_steep-1))),
    #                       exp_recr = exp_recr/Recr_unfished)  |>
    #         dplyr::select(!c("Label", "SSB_unfished","Recr_unfished", "SR_BH_steep", "SBy") )
    #
    #
    #       data <- tmpssb |>
    #         dplyr::left_join(tmppredrec) |>
    #         dplyr::left_join(tmpexprec)
    #
    #       data_all <- bind_rows(data_all, data)
    #
    #     }
    #     return(data_all)
  }
  rownames(data) <- NULL
  return(data)
}
