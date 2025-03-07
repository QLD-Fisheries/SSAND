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
#' @param data3 Output of spaghettiplot_prep_SS(ss_mle)
#' @param xlab2 x axis label for comms_hairymcmc and comms_spaghettiplot. Default
#' is "Year"
#' @param ylab2 y axis label for comms_hairymcmc and comms_spaghettiplot. Default
#' is "Spawning biomass (relative)"
#' @param dir Folder to save outputs. Default is getwd()
#' @param font Font to use on all assets
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
#' data2 <- biomassplot_prep_SS(ss_mle,ss_mcmc_ens, scenarios=1)
#' data3 <- spaghettiplot_prep_SS(ss_mle)
#'
#' axis_info <- comms_hairymcmc(data2,
#'                              dir = fig_dir)
#'
#' comms_spaghettiplot(data3,
#'                     axis_info = axis_info,
#'                     dir = fig_dir)
#' }
comms_spaghettiplot <- function(data3,
                                axis_info,
                                dir = paste0(getwd(),'/'),
                                font = "Meta",
                                xlab2 = "Year",
                                ylab2 = "Spawning biomass (relative)") {

  loadNamespace("extrafont")

  if (!font %in% extrafont::fonts()) {
    stop(paste0("You have not installed the font ",font," into R: 1. Install extrafont 2. Run font_import() 3. Run loadfonts(device = 'win')"))}

  p <- SSAND::spaghettiplot(data3,
                            show_base_case = FALSE,
                            show_scenario_labels = FALSE,
                            legend_position  = "none",
                            colours = fq_palette("cols"),
                            xlab = xlab2,
                            ylab = ylab2) +
    ggplot2::theme(text=ggplot2::element_text(size=12, family=font)) +
    ggplot2::scale_x_continuous(limits = axis_info[[1]],
                                breaks = axis_info[[2]],
                                expand = ggplot2::expansion(mult = c(0.05, 0))) +
    ggplot2::scale_y_continuous(limits = axis_info[[3]],
                                breaks = axis_info[[4]])

  ggplot2::ggsave(filename =  paste0(dir,"spaghetti_plot.png"),
                  plot = p,
                  width = 1440,
                  height = 720,
                  dpi = 150,
                  units = "px")
}
