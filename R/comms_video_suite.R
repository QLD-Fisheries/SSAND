# Copyright 2025 Fisheries Queensland

# This file is part of SSAND.
# SSAND is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
# SSAND is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
# You should have received a copy of the GNU General Public License along with SSAND. If not, see <https://www.gnu.org/licenses/>.

#' @title
#' Animations and graphics for communication packages
#'
#' @description
#' Plots have been formatted for consistent visual assets
#'  - `comms_video_suite()`: An umbrella function that creates all assets
#'  - `comms_biomassposterior()`:  Generate MCMC final biomass posterior density
#'  plots and save x-axis information for histogram animations
#'  - `comms_histogram_animation()`: Generate histogram animation
#'  - `comms_hairymcmc()`: Generate hairy biomass plot and save x-axis
#'  information for spaghetti plot
#'  - `comms_spahettiplot()`: Generate spaghetti plot
#'
#' @name comms_video_suite
#'
#' @param data1 Output of mcmc_finalbiomassposterior_prep_SS(ss_mle,ss_mcmc_ens,cut=FALSE)
#' @param data2 Output of biomassplot_prep_SS(ss_mle,ss_mcmc_ens, scenarios=1)
#' @param data3 Output of spaghettiplot_prep_SS(ss_mle)
#' @param xlab1 x axis label for comms_biomasposterior and comms_histogram_animation.
#' If blank, defaults to paste0("Biomass in ",end_year)
#' @param ylab1 y axis label for comms_biomasposterior and comms_histogram_animation.
#' Should be NULL.
#' @param xlab2 x axis label for comms_hairymcmc and comms_spaghettiplot. Default
#' is "Year"
#' @param ylab2 y axis label for comms_hairymcmc and comms_spaghettiplot. Default
#' is "Spawning biomass (relative)"
#' @param dir Folder to save outputs. Default is getwd()
#' @param end_year Year of biomass to report. E.g. If SS model inputs end in
#' 2024, enter 2025.
#' @param bins Vector of bins for histogram animation
#' @param font Font to use on all assets
#' @param xlim1 The first object in the list output by comms_biomassposterior.
#' A vector of the xlims from comms_biomassposterior, used in
#' comms_histogram_animation
#' @param xbreaks1 The second object in the list output by comms_biomassposterior.
#' A vector of the xbreaks from comms_biomassposterior, used in
#' comms_histogram_animation
#' @param axis_info A list generated from comms_hairymcmc containing vectors for
#' x limits, x breaks, y limits and y breaks, used in comms_spaghettiplot
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(SSAND)
#' library(extrafont)
#' # font_import()
#' # loadfonts(device = "win")
#'
#' end_year <- 2025
#' fig_dir <- paste0(getwd(),'/')
#' load("data.Rdata")
#' ss_mcmc_ens <- mcmc_ensemble_SS(ss_mcmc)
#'
#' data1 <- mcmc_finalbiomassposterior_prep_SS(ss_mle,ss_mcmc_ens,cut=FALSE)
#' data2 <- biomassplot_prep_SS(ss_mle,ss_mcmc_ens, scenarios=1)
#' data3 <- spaghettiplot_prep_SS(ss_mle)
#'
#' comms_video_suite <- function(data1,
#'                                 data2,
#'                                 data3,
#'                                 dir = fig_dir,
#'                                 end_year = 2025)
#' }
comms_video_suite <- function(data1,
                              data2,
                              data3,
                              xlab1 = NULL,
                              ylab1 = NULL,
                              xlim1 = c(0,1),
                              xbreaks1 = c(0,0.2,0.4,0.6,0.8,1.0),
                              xlab2 = "Year",
                              ylab2 = "Spawning biomass (relative)",
                              dir,
                              end_year,
                              bins = c(50000,10000,5000,1000,500,100),
                              font = "Meta",
                              axis_info = NULL) {

  loadNamespace("extrafont")

  if (!font %in% extrafont::fonts()) {
    stop(paste0("You have not installed the font ",font," into R: 1. Install extrafont 2. Run font_import() 3. Run loadfonts(device = 'win')"))}

  # Generate biomass posterior density plots and save
  # x-axis information for histogram animations:
  message("Creating biomass posterior plots.")
  x_info <- SSAND::comms_biomassposterior(data1,
                                          dir = dir,
                                          end_year = end_year,
                                          xlab1 = xlab1,
                                          ylab1 = ylab1,
                                          font = font)

  # Generate histogram animation
  message("Creating histogram animation.")
  SSAND::comms_histogram_animation(data2,
                                   end_year = end_year,
                                   bins = bins,
                                   dir = dir,
                                   xlab1 = xlab1,
                                   ylab1 = ylab1,
                                   xlim1 = x_info[[1]],
                                   xbreaks1 = x_info[[2]],
                                   font = font)

  # Generate hairy biomass plot and save
  # x-axis information for spaghetti plot:
  message("Creating hairy MCMC biomass animation.")
  axis_info <- SSAND::comms_hairymcmc(data2,
                                      xlab2 = xlab2,
                                      ylab2 = ylab2,
                                      dir = dir,
                                      font = font)

  # Generate spaghetti plot
  message("Creating spaghetti plot.")
  SSAND::comms_spaghettiplot(data3,
                             xlab2 = xlab2,
                             ylab2 = ylab2,
                             axis_info = axis_info,
                             dir = dir,
                             font = font)

  # Tidy up
  rm(x_info, axis_info)
  message("Done!")
}
