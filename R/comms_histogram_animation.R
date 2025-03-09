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
#' @name comms_histogram_animation
#'
#' @param data2 Output of biomassplot_prep_SS(ss_mle,ss_mcmc_ens, scenarios=1)
#' @param xlab1 x axis label for comms_biomasposterior and comms_histogram_animation.
#' If blank, defaults to paste0("Biomass in ",end_year)
#' @param ylab1 y axis label for comms_biomasposterior and comms_histogram_animation.
#' Should be NULL.
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
#'
#' x_info <- comms_biomassposterior(data1,
#'                                  dir = fig_dir,
#'                                  end_year = end_year)
#'
#' comms_histogram_animation(data2,
#'                           end_year = end_year,
#'                           dir = fig_dir,
#'                           xlim1 = x_info[[1]],
#'                           xbreaks1 = x_info[[2]])
#' }
comms_histogram_animation <- function(data2,
                                      end_year,
                                      bins = c(50000,10000,5000,1000,500,100),
                                      dir = paste0(getwd(),'/'),
                                      xlab1 = NULL,
                                      ylab1 = NULL,
                                      xlim1 = c(0,1),
                                      xbreaks1 = c(0,0.2,0.4,0.6,0.8,1.0),
                                      font = "Meta") {
  loadNamespace("extrafont")

  if (!font %in% extrafont::fonts()) {
    stop(paste0("You have not installed the font ",font," into R: 1. Install extrafont 2. Run font_import() 3. Run loadfonts(device = 'win')"))}

  hist_data <- data2 |>
    dplyr::filter(med == "MCMC", year == end_year)

  if (missing(xlab1)) {xlab1 = paste0("Biomass at end of ",end_year-1)}

  # Generate individual histogram plots
  for (i in 1:length(bins)) {
    p <- ggplot2::ggplot(hist_data) +
      ggplot2::theme_bw() +
      ggplot2::geom_histogram(ggplot2::aes(x=value),bins=bins[i]) +
      ggplot2::xlab(xlab1) +
      ggplot2::ylab(ylab1) +
      ggplot2::scale_fill_manual(values = c("grey30","grey50","grey70")) +
      ggplot2::theme(legend.position = "top") +
      ggplot2::scale_x_continuous(labels = scales::percent, limits = xlim1, breaks = xbreaks1) +
      ggplot2::theme(text=ggplot2::element_text(size=12, family=font)) +
      ggplot2::theme(panel.grid.minor.x = ggplot2::element_blank()) +
      ggplot2::theme(axis.ticks.y=ggplot2::element_blank(),axis.text.y=ggplot2::element_blank())

    ggplot2::ggsave(filename = paste0("hist_temp_",bins[i],".png"),
                    plot = p,
                    width = 1440,
                    height = 720,
                    dpi = 150,
                    units = "px")

    # Convert static images to MP4 videos
    system(paste0("ffmpeg -loop 1 -t 0.8 -framerate 30 -i hist_temp_",bins[i],".png -vf scale=1920:1080 -c:v libx264 -pix_fmt yuv420p -y hist_temp_",bins[i],".mp4"))
  }

  # Create a text file listing the videos to concatenate
  writeLines(paste(paste0("file 'hist_temp_", bins, ".mp4'"), collapse = "\n"), "input_files.txt")

  # Concatenate all videos into a final video
  system(paste0("ffmpeg -f concat -safe 0 -i input_files.txt -c:v libx264 -pix_fmt yuv420p -y ",dir,"histogram_animation.mp4"))

  # Remove working files
  for (i in 1:length(bins)) {
    file.remove(paste0("hist_temp_",bins,".png"))
    file.remove(paste0("hist_temp_",bins,".mp4"))
    file.remove("input_files.txt")
  }

  # Save a copy in specified direction and delete working files
  if (!missing(dir)) {
    file.copy(from = "histogram_animation.mp4",
              to = paste0(dir,"histogram_animation.mp4"))
    if (!dir == paste0(getwd(),'/')) {
      file.remove("histogram_animation.mp4")
    }
  }
}
