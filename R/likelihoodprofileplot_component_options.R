# Copyright 2024 Fisheries Queensland

# This file is part of SSAND.
# SSAND is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
# SSAND is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
# You should have received a copy of the GNU General Public License along with SSAND. If not, see <https://www.gnu.org/licenses/>.

#' Options for component argument of likelihoodprofileplot_prep_SS
#'
#' @param profile_input The likelihood profile object after running: r4ss::profile(), r4ss::SSgetoutput(), then r4ss::SSsummarize(). See ?likelihoodprofileplot for details.
#'
#' @return A vector of options that can be used in the component argument of likelihoodprofileplot_prep_SS()
#' @export
#'
#' @examples
#' \dontrun{
#' likelihoodprofileplot_component_options(profile_input)
#' data <- likelihoodprofileplot_prep_SS(profile_input)
#' likelihoodprofileplot(data)
#' }
likelihoodprofileplot_component_options <- function(profile_input) {
  options <- profile_input$likelihoods |>
    dplyr::select(Label) |>
    dplyr::pull()

  component_translate <- data.frame(
    component_code = c("TOTAL",
                       "Catch", "Equil_catch", "Survey", "Discard", "Mean_body_wt",
                       "Length_comp", "Age_comp", "Size_at_age", "SizeFreq",
                       "Morphcomp", "Tag_comp", "Tag_negbin", "Recruitment",
                       "InitEQ_Regime", "Forecast_Recruitment", "Parm_priors",
                       "Parm_softbounds", "Parm_devs", "F_Ballpark", "Crash_Pen"),
    component_simple = c("Total", "Catch", "Equilibrium catch",
                         "Index data", "Discard", "Mean body weight", "Length data",
                         "Age data", "Size-at-age data", "Generalized size data",
                         "Morph composition data", "Tag recapture distribution",
                         "Tag recapture total", "Recruitment", "Initital equilibrium recruitment",
                         "Forecast recruitment", "Priors", "Soft bounds", "Parameter deviations",
                         "F Ballpark", "Crash penalty")
  )

  labels <- data.frame(component_code = options) |>
    dplyr::left_join(component_translate, by = dplyr::join_by(component_code)) |>
    dplyr::select(component_simple) |>
    dplyr::pull()

  return(labels)
}
