# Copyright 2024 Fisheries Queensland

# This file is part of SSAND.
# SSAND is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
# SSAND is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
# You should have received a copy of the GNU General Public License along with SSAND. If not, see <https://www.gnu.org/licenses/>.

#' Prepare Stock Synthesis data for likelihoodprofileplot()
#' View ?likelihoodprofileplot to view full examples for running likelihood profiles for Stock Synthesis or DDUST.
#'
#' @param profile_input The likelihood profile object after running: r4ss::profile(), r4ss::SSgetoutput(), then r4ss::SSsummarize(). See example for details.
#' @param component The components for which you want to display the likelihood profile.
#' Use SSAND::likelihoodprofileplot_component_options(profile_input) to see options.
#' The default is c("Total","Index data", "Length data", "Age data", "Recruitment").
#' @param parameter Parameter used in likelihood profile. Use unique(profile_input$pars$Label) for valid options.
#'
#' @return A dataframe with columns x_vector, component and likelihood
#' @export
#'
#' @examples
#' \dontrun{
#' r4ss::profile(dir = '.', # directory of 4 SS files
#'               oldctlfile = "control.ctl",
#'               newctlfile = "control.ctl",
#'               string = "steep",
#'               profilevec = c(0.4,0.5,0.6),
#'               exe = "C:/stocksynthesis/ss_3.30.22.exe")
#'
#' profile_input <- r4ss::SSsummarize(
#'   r4ss::SSgetoutput(dirvec = ".",
#'                     keyvec = 1:3, # 1:length(profilevec)
#'                     getcovar = FALSE,
#'                     getcomp = FALSE))
#'
#'
#' data <- likelihoodprofileplot_prep_SS(profile_input, parameter="SR_BH_steep")
#' likelihoodprofileplot(data)
#' }
likelihoodprofileplot_prep_SS <- function(profile_input,
                                          component = c("Total","Index data", "Length data", "Age data", "Recruitment"),
                                          parameter = "SR_LN(R0)"
) {

  if (length(setdiff(component,likelihoodprofileplot_component_options(profile_input)))>0) {
    warning("component contains an invalid option. Use SSAND::likelihoodprofileplot_component_options(profile_input) to see options.")
  }

  if (!parameter %in% unique(profile_input$pars$Label)) {
    warning("parameter is not a valid option. Use unique(profile_input$pars$Label) to see valid options.")
  }

  data <- profile_input$likelihoods |>
    # Translate to simplified name
    dplyr::rename(label = Label) |>
    dplyr::mutate(label = dplyr::recode(label,
                                        "TOTAL" = "Total",
                                        "Catch" = "Catch",
                                        "Equil_catch" = "Equilibrium catch",
                                        "Survey" = "Index data",
                                        "Discard" = "Discard",
                                        "Mean_body_wt" = "Mean body weight",
                                        "Length_comp" = "Length data",
                                        "Age_comp" = "Age data",
                                        "Size_at_age" = "Size-at-age data",
                                        "SizeFreq" = "Generalized size data",
                                        "Morphcomp" = "Morph composition data",
                                        "Tag_comp" = "Tag recapture distribution",
                                        "Tag_negbin" = "Tag recapture total",
                                        "Recruitment" = "Recruitment",
                                        "InitEQ_Regime" = "Initital equilibrium recruitment",
                                        "Forecast_Recruitment" = "Forecast recruitment",
                                        "Parm_priors" = "Priors")) |>
  # Filter for likelihood profiles of interest
  dplyr::filter(label %in% component)

  # Pivot table
  data <- data |>
    `colnames<-`(c(1:(ncol(data)-1),"label")) |>
    tidyr::pivot_longer(-label, names_to = "model", values_to = "likelihood")

 # Calculate change in -log-likelihood by subtracting minimum
  data <- data |>
    dplyr::group_by(label) |>
    dplyr::mutate(likelihood = likelihood - min(likelihood)) |>
    dplyr::ungroup()

  # Extract x-axis vector
  x_vector <- profile_input$pars |>
    dplyr::filter(Label == parameter) |>
    dplyr::select(-Label,-Yr,-recdev) |>
    t() |>
    c()

  x_dataframe <- data.frame(model = 1:length(x_vector), x_vector)

  # Add x-axis vector then reshape dataframe
  data <- data |>
    dplyr::mutate(model = as.integer(model)) |>
    dplyr::left_join(x_dataframe, by = "model") |>
    dplyr::select(x_vector, component = label, likelihood)

  return(data)
}
