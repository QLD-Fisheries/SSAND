# Copyright 2024 Fisheries Queensland

# This file is part of SSAND.
# SSAND is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
# SSAND is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
# You should have received a copy of the GNU General Public License along with SSAND. If not, see <https://www.gnu.org/licenses/>.

#' Options for component argument of pinerplot_prep_SS
#'
#' @param profile_input The likelihood profile object after running: r4ss::SS_profile(), r4ss::SSgetoutput(), then r4ss::SSsummarize(). See ?pinerplot for example.
#'
#' @return A vector of options that can be used in the component argument of pinerplot_prep_SS()
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
pinerplot_component_options <- function(profile_input) {
  options <- profile_input$likelihoods_by_fleet |>
    dplyr::select(Label) |>
    dplyr::filter(grepl("_like",Label)) |>
    unique() |>
    dplyr::pull()

  return(options)
}
