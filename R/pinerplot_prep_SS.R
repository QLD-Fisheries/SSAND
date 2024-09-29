# Copyright 2024 Fisheries Queensland

# This file is part of SSAND.
# SSAND is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
# SSAND is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
# You should have received a copy of the GNU General Public License along with SSAND. If not, see <https://www.gnu.org/licenses/>.

#' Prepare Stock Synthesis data for pinerplot()
#'
#' @param profile_input The likelihood profile object after running: r4ss::SS_profile(), r4ss::SSgetoutput(), then r4ss::SSsummarize(). See ?pinerplot for example.
#' @param component The component for which you want to display the likelihood profile. Use SSAND::pinerplot_component_options(profile_input) to see options.
#' @param likelihood_type Either "raw_times_lambda" or "raw".
#' @param parameter Parameter used in likelihood profile. Use unique(profile_input$pars$Label) for valid options.
#'
#' @return A dataframe with columns x_vector, fleet and likelihood
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
#' pinerplot_component_options(profile_input)
#' data <- pinerplot_prep_SS(profile_input, component="Length_like")
#' pinerplot(data)
#' }
pinerplot_prep_SS <- function(profile_input,
                              component = "Length_like",
                              likelihood_type = "raw_times_lambda",
                              parameter = "SR_LN(R0)"
) {

  if (!likelihood_type %in% c("raw", "raw_times_lambda")) {
    warning("likelihood_type must be either \"raw\" or \"raw_times_lambda\"")
  }

  if (!component %in% pinerplot_component_options(profile_input)) {
    warning("component is not a valid option. Use SSAND::pinerplot_component_options(profile_input) to see options.")
  }

  if (!parameter %in% unique(profile_input$pars$Label)) {
    warning("parameter is not a valid option. Use unique(profile_input$pars$Label) to see valid options.")
  }

  data <- profile_input$likelihoods_by_fleet |>
    dplyr::filter(Label == component)

  # If likelihood_type is raw time lambda, take the
  #   lambda multipliers for the component and multiply
  #   by the likelihood, then replace the likelihoods
  #   with this product.
  if (likelihood_type=="raw_times_lambda") {
    component_lambda <- paste0(substr(component,1,nchar(component)-4),"lambda")

    data_lambda <- profile_input$likelihoods_by_fleet |>
      dplyr::filter(Label == component_lambda)

    data[,-c(1:3)] <- data[,-c(1:3)] * data_lambda[,-c(1:3)]
  }

  # Calculate change in -log-likelihood by subtracting minimum
  #    value of each likelihood colum
  for (i in 3:ncol(data)) {
    data[,i] <- data[,i] - min(data[,i])
  }

  # Extract x-axis vector
  x_vector <- profile_input$pars |>
    dplyr::filter(Label == parameter) |>
    dplyr::select(-Label,-Yr,-recdev) |>
    t() |>
    c()

  # Add x-axis vector then reshape dataframe
  data <- data |>
    dplyr::mutate(x_vector = x_vector) |>
    tidyr::pivot_longer(cols=-c(model, Label, x_vector), names_to = "fleet", values_to = "likelihood")

  # Filter out any where entire column is zero
  data <- data |>
    dplyr::group_by(fleet) |>
    dplyr::mutate(sum = sum(likelihood)) |>
    dplyr::ungroup() |>
    dplyr::filter(sum > 0) |>
    dplyr::select(-model, -sum, -Label)

  return(data)
}
