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
#' @name comms_biomassposterior
#'
#' @param data1 Output of mcmc_finalbiomassposterior_prep_SS(ss_mle,ss_mcmc_ens,cut=FALSE)
#' @param xlab1 x axis label for comms_biomasposterior and comms_histogram_animation.
#' If blank, defaults to paste0("Biomass in ",end_year)
#' @param ylab1 y axis label for comms_biomasposterior and comms_histogram_animation.
#' Should be NULL.
#' @param dir Folder to save outputs. Default is getwd()
#' @param end_year Year of biomass to report. E.g. If SS model inputs end in
#' 2024, enter 2025.
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
#' data1 <- mcmc_finalbiomassposterior_prep_SS(ss_mle,ss_mcmc_ens,cut=FALSE)
#' comms_biomassposterior(data1,
#'                        dir = fig_dir,
#'                        end_year = end_year)
#' }
comms_biomassposterior <- function(data1,
                                   dir = paste0(getwd(),'/'),
                                   end_year,
                                   xlab1 = NULL,
                                   ylab1 = NULL,
                                   font = "Meta") {

  loadNamespace("extrafont")

  if (!font %in% extrafont::fonts()) {
    stop(paste0("You have not installed the font ",font," into R: 1. Install extrafont 2. Run font_import() 3. Run loadfonts(device = 'win')"))}

  if (missing(xlab1)) {xlab1 = paste0("Biomass at end of ",end_year-1)}

  p <- SSAND::mcmc_finalbiomassposteriorplot(data1,
                                             colours = c("grey30","grey30","grey30","grey30"),
                                             ylab = ylab1,
                                             xlab = xlab1) +
    ggplot2::theme(text=ggplot2::element_text(size=12, family=font)) +
    ggplot2::theme(legend.position = "none")

  ggplot2::ggsave(filename = paste0(dir,"biomass_posterior_grey.png"),
                  plot = p,
                  width = 1440,
                  height = 720,
                  dpi = 150,
                  units = "px")

  x_info <- list()
  x_info[[1]] <- as.numeric(stats::na.omit(ggplot2::layer_scales(p)$x$get_limits()))/100
  x_info[[2]] <- as.numeric(stats::na.omit(ggplot2::layer_scales(p)$x$break_positions()))/100

  p <- SSAND::mcmc_finalbiomassposteriorplot(data1,
                                             xlab = xlab1,
                                             ylab = ylab1) +
    ggplot2::theme(text=ggplot2::element_text(size=12, family=font)) +
    ggplot2::theme(legend.position = "none")

  ggplot2::ggsave(filename =  paste0(dir,"biomass_posterior_coloured.png"),
                  plot = p,
                  width = 1440,
                  height = 720,
                  dpi = 150,
                  units = "px")

  p <- SSAND::mcmc_finalbiomassposteriorplot(data1,
                                             xlab = xlab1,
                                             ylab = ylab1) +
    ggplot2::theme(text=ggplot2::element_text(size=12, family=font))

  ggplot2::ggsave(filename =  paste0(dir,"biomass_posterior_coloured_legend.png"),
                  plot = p,
                  width = 1440,
                  height = 720,
                  dpi = 150,
                  units = "px")

  legend <- magick::image_read(paste0(dir,"biomass_posterior_coloured_legend.png"))
  legend <- magick::image_crop(legend, "1440x120")
  magick::image_write(legend, paste0(dir,"biomass_posterior_coloured_legend.png"))

  return(x_info)
}
