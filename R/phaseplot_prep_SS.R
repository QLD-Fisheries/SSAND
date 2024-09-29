# Copyright 2024 Fisheries Queensland

# This file is part of SSAND.
# SSAND is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
# SSAND is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
# You should have received a copy of the GNU General Public License along with SSAND. If not, see <https://www.gnu.org/licenses/>.

#' Prepare Stock Synthesis data for phase plot
#'
#' @param ss_mle A list of outputs from r4ss::SS_output() with one element per scenario. Will automatically reformat as a list if a single r4ss::SS_output() output (i.e. one scenario) is entered.
#' @param scenarios A vector of scenarios to plot (numeric). Shows all scenarios if left blank. Can be overridden in the plotting function.
#' @param financial_year Set to TRUE if the assessment was based on financial year (logical).
#'
#' @return A data frame that contains year (int), Bratio (num), F_ (num), scenario (factor), B_MSY (num), F_max (num)
#' @export
#'
#' @examples
#' data <- phaseplot_prep_SS(ss_mle)
#' phaseplot(data)
phaseplot_prep_SS <- function(ss_mle,
                              scenarios = NULL,
                              financial_year = FALSE) {

  if (check_scenarios(ss_mle,"SS","MLE")=="single scenario"){ss_mle <- list(ss_mle); warning("Assuming you are entering a single scenario, not a list of scenarios. Wrap ss_mle input inside a list() to avoid this warning.")}

  if (missing(scenarios)){scenarios <- 1:length(ss_mle)}

    data <- data.frame()
    for (scenario in scenarios) {
      replist <- ss_mle[[scenario]]$derived_quants
      startyr <- ss_mle[[scenario]]$startyr
      endyr <-  ss_mle[[scenario]]$endyr

      yrs <- c(startyr:endyr, endyr+1)

      F_year_Bmsy <- replist[match(paste("F_", yrs, sep = ""),
                                   replist[["Label"]]), ]

      Bratio_year_unfished <- replist[match(paste("Bratio_", yrs, sep = ""),
                                            replist[["Label"]]), ]

      PhaseDataTemp <- data.frame(year = yrs,
                                  Bratio_year = Bratio_year_unfished$Value,
                                  F_year = F_year_Bmsy$Value)

      PhaseDataTemp$Bratio_year[which(PhaseDataTemp$year==yrs[1])] <- 1

      # Update financial year representation to be year+1 for year--year+1
      if (financial_year==TRUE) {PhaseDataTemp$year <- PhaseDataTemp$year + 1}

      PhaseData <- data.frame(year = PhaseDataTemp$year - 1,
                              Bratio = PhaseDataTemp$Bratio_year)

      PhaseData$F_ <- c(0, PhaseDataTemp$F_year[1:(nrow(PhaseDataTemp)-1)])

      PhaseData$scenario <- as.factor(scenario)
      PhaseData$B_MSY <- ss_mle[[scenario]]$derived_quants$Value[replist$Label == "B_MSY/SSB_unfished"]

      F_max <- round(max(c(PhaseData$F_,0.01), na.rm=TRUE),2)

      if (F_max > 0.4){
        PhaseData$F_max <- max(F_max,1.2)+0.2
      } else {
        PhaseData$F_max <- 1.1*F_max
      }

      data <- rbind(data, PhaseData)
    }

  return(data)
}

