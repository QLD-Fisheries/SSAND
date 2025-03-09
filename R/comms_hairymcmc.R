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
#' @name comms_hairymcmc
#'
#' @param data2 Output of biomassplot_prep_SS(ss_mle,ss_mcmc_ens, scenarios=1)
#' @param xlab2 x axis label for comms_hairymcmc and comms_spaghettiplot. Default
#' is "Year"
#' @param ylab2 y axis label for comms_hairymcmc and comms_spaghettiplot. Default
#' is "Spawning biomass (relative)"
#' @param dir Folder to save outputs. Default is getwd()
#' @param font Font to use on all assets
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
#'
#' axis_info <- comms_hairymcmc(data2,
#'                              dir = fig_dir)
#' }
comms_hairymcmc <- function(data2,
                            dir = paste0(getwd(),'/'),
                            font = "Meta",
                            xlab2 = "Year",
                            ylab2 = "Spawning biomass (relative)") {

  loadNamespace("extrafont")

  if (!font %in% extrafont::fonts()) {
    stop(paste0("You have not installed the font ",font," into R: 1. Install extrafont 2. Run font_import() 3. Run loadfonts(device = 'win')"))}

  hairy <- SSAND::biomassplot(data2,
                       show_median = "none",
                       mcmc_style = "hairy",
                       sample = 500,
                       show_target_line = FALSE,
                       show_limit_line = FALSE,
                       xlab = xlab2,
                       ylab = ylab2) +
    ggplot2::theme(text=ggplot2::element_text(size=12, family=font)) +
    # scale_x_continuous(limits = c(1912,2025),breaks=c(1925,1950,1975,2000,2025)) +
    gganimate::transition_time(rownum) +
    gganimate::shadow_mark(size = 1, colour = 'grey30')

  gganimate::anim_save(paste0(dir,"hairy_mcmc.gif"), animation = hairy, width = 1440, height = 720, units = "px", res = 150, nframes = 500)

  axis_info <- list()
  axis_info[[1]] <- as.numeric(stats::na.omit(ggplot2::layer_scales(hairy)$x$get_limits()))
  axis_info[[2]] <- as.numeric(stats::na.omit(ggplot2::layer_scales(hairy)$x$break_positions()))
  axis_info[[3]] <- as.numeric(stats::na.omit(ggplot2::layer_scales(hairy)$y$get_limits()))
  axis_info[[4]] <- as.numeric(stats::na.omit(ggplot2::layer_scales(hairy)$y$break_positions()))
  return(axis_info)
}
