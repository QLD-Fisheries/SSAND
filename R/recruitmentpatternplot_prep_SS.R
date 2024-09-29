# Copyright 2024 Fisheries Queensland

# This file is part of SSAND.
# SSAND is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
# SSAND is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
# You should have received a copy of the GNU General Public License along with SSAND. If not, see <https://www.gnu.org/licenses/>.

#' Prepare Stock Synthesis data for recruitment pattern plot
#'
#' @param ss_mle list of outputs from r4ss::SS_output(), one element per scenario
#' @param scenarios A vector of scenarios to plot (numeric). Shows all scenarios if left blank. Can be overridden in the plotting function.
#'
#' @return A dataframe with recruitment (num), months (num), monthnames (factor), scenario (factor), format (character)
#' @export
#'
#' @examples
#' data <- recruitmentpatternplot_prep_SS(ss_mle)
#' recruitmentpatternplot(data)
recruitmentpatternplot_prep_SS <- function(ss_mle,
                                           scenarios = NULL) {

  if (check_scenarios(ss_mle,"SS","MLE")=="single scenario"){ss_mle <- list(ss_mle); warning("Assuming you are entering a single scenario, not a list of scenarios. Wrap ss_mle input inside a list() to avoid this warning.")}

  if (missing(scenarios)){scenarios <- 1:length(ss_mle)}
  data <- data.frame()

  for (scenario in scenarios) {
    tmp <- data.frame(recruitment=NA, months=NA, monthnames=NA, scenario=NA, area=NA, GP=NA, format = NA)

    if (nrow(ss_mle[[scenario]]$RecrDistpars)>0) {
      tmp <- ss_mle[[scenario]]$RecrDistpars |>
        dplyr::mutate(GP = stringr::str_extract(Label, "(?<=GP_)\\d+"),
                      area = stringr::str_extract(Label, "(?<=area_)\\d+"),
                      months = as.numeric(stringr::str_extract(Label, "(?<=month_)\\d+"))) |>
        dplyr::mutate(recruitment = 1 / (1 + exp(-1*Value))) |>
        dplyr::mutate(monthnames = month.name[months]) |>
        dplyr::mutate(scenario = scenario) |>
        dplyr::select(recruitment, months, monthnames, scenario, area, GP) |>
        dplyr::filter(!is.na(months)) |>
        dplyr::mutate(format = NA) |>
        dplyr::mutate(recruitment = recruitment/sum(recruitment))
    }

    if (!nrow(tmp)==1) {
      tmp$format = "points"
    }

    if (nrow(tmp)==1) {
      tmp$format = "no points"
      tmp2 <- tmp |> dplyr::mutate(months=12, monthnames="December")
      tmp <- rbind(tmp,tmp2)
    }
    data <- rbind(data,tmp)
  }

  data <- data |> dplyr::filter(!is.na(scenario))
  rownames(data) <- NULL
  return(data)
}

