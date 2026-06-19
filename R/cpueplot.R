# Copyright 2024 Fisheries Queensland

# This file is part of SSAND.
# SSAND is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
# SSAND is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
# You should have received a copy of the GNU General Public License along with SSAND. If not, see <https://www.gnu.org/licenses/>.

#' CPUE plot with an option to add model fits
#'
#' @param data Output from cpueplot_prep(). A data frame with variables date (date), fleet (int), obs (num), exp (num), ub (num), lb (num), scenario (fac)
#' @param show_inputs Set to TRUE to show model inputs.
#' @param show_fits Set to TRUE to show model fits.
#' @param fleets A numeric or vector of fleet numbers to plot
#' @param fleet_names A vector of customised fleet names for legend
#' @param scenarios A vector of scenario numbers to be shown on plot (numeric). This was already specified in prep file, but this is a manual override to save running the prep function again.
#' @param scenario_labels Customise facet labels for individual scenarios
#' @param scenario_order A vector to reorder how scenarios are displayed (character). Use the label names defined in "scenario_labels".
#' If "scenario_labels" is left blank, the labels will be "Scenario 1", "Scenario 2" etc.
#' Any scenarios not included in "scenario_order" will be tacked on in the order they appear in the input data.
#' @param xlab Label for x-axis (character). Default is "Year".
#' @param ylab Label for y-axis (character). Default is "Catch rate (kg/operation day)".
#' @param xbreaks A vector of breaks between x-axis labels, used in ggplot2::scale_x_continous() (numeric).
#' @param ybreaks A vector of breaks between y-axis labels, used in ggplot2::scale_y_continous() (numeric).
#' @param xlim A vector of lower and upper x-axis limits (e.g. c(1950, 2020)) (numeric).
#' @param ylim A vector of lower and upper y-axis limits (e.g. c(0,1)) (numeric).
#' @param xlabels A vector of labels for the x-axis breaks.
#' @param ylabels A vector of labels for the y-axis breaks.
#' @param xangle Set to 90 to rotate x-axis labels 90 degrees.
#' @param show_colour Set to TRUE to activate coloured mode, or FALSE for greyscale (logical).
#' @param show_CI_ribbon Set to TRUE to show a confidence interval ribbon (logical).
#' @param show_error_bar Set to TRUE to include error bar (logical). If wanting to use confidence interval lines with error bars use make error_bar = FALSE
#' @param show_negative Set to TRUE to allow confidence interval to be less than zero (logical).
#' @param point_size Size of points used in ggplot2::geom_line(). Default is 1.5.
#' @param text_size Text size (num). Default is 12.
#' @param colours A vector of colours used (character).
#' @param legend_position Position of the legend ("none", "left", "right", "bottom", "top", or two-element numeric vector for x and y position). Default is "top".
#' @param ncol Number of columns for facet_wrap(). Default is 2.
#' @param scales Scales for ggplot2::facet_wrap(). Default is 'free', see ?ggplot2::facet_wrap for options.
#' @param financial_year Set to TRUE if the assessment was based on financial year (logical). Adds 1 to each year in the dataset.
#' @param seasonal Set to TRUE if the model is seasonal (i.e. has more than one timestep per year), to display a more detailed x-axis. If missing, or set to FALSE, the x-axis will just display year, not month.
#' @param show_point Set to TRUE to display fit/s as points rather than a line (e.g. if there is only one data point)
#' @param show_line Set to FALSE to show only points, not line (if there is only one observation in a scenario for example)
#' @param show_dates_on_axis Set to TRUE to show full dates on x-axis as opposed to years.
#' @param sample Number of samples to plot from each MCMC chain to ease burden of rendering dense plots (numeric).
#' @param band_colour Colour of bands (character). Only used when mcmc_style=="banded". Input one colour, bands will be distinguished using an alpha.
#' @param show_median Type of median shown. Default "median_cpue" shows the median of each year,
#' "trajectory" shows median trajectory based on biomass in final year
#' @param mcmc_style The type of MCMC plot to be displayed (character). Options are "banded", "hairy", "boxplot", "CI" and "joy", the default is "banded". Only one option can be selected.
#' @param legend_box Display option for legend (character). Choose "vertical" to stack legend types vertically, or "horizontal" to keep legends in one row.
#' @param show_observed_error Set to TRUE to display error bars around input data (MCMC only)
#' @param line_width Width of median lines (numeric). Default is 1.
#' @param hair_width Width of fine MCMC hairs (numeric). Default is 0.5.
#' @param input_colour Specify colour of input data (MCMC)
#' @param input_range_colour Specify colour of error around input data (MCMC)
#' @param aggregate_scenarios Set to TRUE to calculate credible intervals across all scenarios (logical). Only activated if mcmc_style==CI.
#' @param CI_range Specify credible interval range (numeric). Only activated if mcmc_style==CI.
#' @param alpha Transparency for range (numeric) used in ggplot2::geom_density_ridges(). Default is 0.7.
#' @param fit_colour Default is fleet colour. Otherwise input a colour here (useful when displaying fleets in a facet_wrap)
#' @param boxplot_outliers Set to FALSE to remove outlier points from boxplot. Default is TRUE.
#'
#' @return A plot that shows input data and model fits to CPUE data
#' @export
#'
#' @examples
#' data <- cpueplot_prep_SS(ss_mle, scenarios=1)
#' cpueplot(data)
#' cpueplot(data, xlim=c(1954,2023))
#'
#' # To create a suitable CPUE plot for a report summary:
#' cpueplot(data,
#'          fleets=1,
#'          show_inputs = TRUE,
#'          show_fits = FALSE,
#'          show_CI_ribbon = FALSE,
#'          show_error_bar = FALSE,
#'          show_point = TRUE)
cpueplot <- function(data,
                     show_inputs = TRUE,
                     show_fits = TRUE,
                     show_observed_error = FALSE,
                     fleets = NULL,
                     fleet_names = NULL,
                     scenarios = NULL,
                     scenario_labels = NULL,
                     scenario_order = NULL,
                     xlab = "Year",
                     ylab = "Catch rate (kg/operation day)",
                     xbreaks = NULL,
                     ybreaks = NULL,
                     xlim = NULL,
                     ylim = NULL,
                     xlabels = NULL,
                     ylabels = NULL,
                     show_colour = TRUE,
                     xangle = NULL,
                     show_CI_ribbon = TRUE,
                     show_error_bar = FALSE,
                     show_negative = TRUE,
                     point_size = 2,
                     text_size = 12,
                     colours = c("black",fq_palette("cols")),
                     legend_position = "top",
                     ncol = 2,
                     scales = "free",
                     financial_year = FALSE,
                     seasonal = NULL,
                     show_point = TRUE,
                     show_line = NULL,
                     show_dates_on_axis = FALSE,
                     sample = NULL,
                     band_colour = "black",
                     band_labels = NULL,
                     show_median = "median_cpue", # trajectory, median_cpue, none
                     mcmc_style = "banded", # hairy, boxplot, banded, CI, joy
                     line_width = 1,
                     hair_width = 0.5,
                     legend_box = "horizontal",
                     input_colour = "black",
                     input_range_colour = "black",
                     aggregate_scenarios = FALSE,
                     CI_range = 0.95,
                     alpha = NULL,
                     fit_colour = NULL,
                     boxplot_outliers = TRUE,

                     xangle = NULL,
                     colours = NULL,
                     legend_position = NULL,
                     financial_year = FALSE,
                     text_size = NULL,
                     legend_text_size = NULL,
                     text_colour = NULL,
                     legend_text_colour = NULL,
                     legend_box = NULL,
                     panel_border = NULL,
                     panel_border_colour = NULL,
                     ) {

  # ___________________
  # Data validation
  # ___________________

  # Identify MCMC or MLE
  MCMC <- "med" %in% names(data)

  # Data input warnings
  if (!MCMC) check_data_columns(data, c("date","fleet","obs","exp","ub","lb","scenario"))
  if (MCMC)  check_data_columns(data, c("year","month","fleet","obs","exp","ub","lb","rownum","med","interval","date","scenario"))


  if (!MCMC) {data$upper <- data$ub; data$lower <- data$lb}

  # MCMC warnings
  # show_median <- simplify_show_median(show_median, c("median_cpue","trajectory","none"))
  # check_mcmc_style(mcmc_style)
  # if (MCMC) {data$upper <- data$prob_upper; data$lower <- data$prob_lower}
  # if (MCMC) data$med[startsWith(data$med, "median_")] <- "annual"
  # if (MCMC) data <- sample_mcmc_runs(data, sample)

  data$xvar <- data$date

  if (is.null(alpha) & mcmc_style !="banded") {alpha=0.7}

  facet_wrap <- length(unique(data$scenario))>1 & !aggregate_scenarios


  # ___________________
  # Custom to this plot
  # ___________________

  if (!show_negative) {data <- dplyr::mutate(data, lower = ifelse(lower<0,0,lower))}
  if (!is.null(fleets)) {data <- data |> dplyr::filter(fleet %in% fleets)}
  if (is.null(fleets)) {fleets <- sort(unique(data$fleet))}
  if (is.null(fleet_names)) {fleet_names <- paste0('Fleet ',sort(unique(data$fleet)))}
  data <- data[which(data$fleet %in% as.character(fleets)),]

  # If "date" column is entered as just years, convert to dates
  if (!lubridate::is.Date(data$date)) {
    if (nchar(data$date[1])==4) {
      data$date <- as.Date(paste0(data$date,"-01-01"), format = "%Y-%m-%d")
    }
  }

  # If xlim is entered as just years, convert to dates
  if (!missing(xlim)) {
    if (nchar(xlim[1])==4) {
      xlim <- c(as.Date(paste0(xlim[1], "-01-01"), format = "%Y-%m-%d"),
                as.Date(paste0(xlim[2], "-01-01"), format = "%Y-%m-%d"))
    }
  }

  # In some circumstances there is just one observation in a scenario, in which case a line shouldn't be plotted, only points.
  if (missing(show_line)) {
    min_n_obs <- data |>
      dplyr::group_by(scenario) |>
      dplyr::mutate(n_obs = dplyr::n()) |>
      dplyr::ungroup() |>
      dplyr::summarise(min_n_obs = min(n_obs)) |>
      dplyr::pull()

    if (min_n_obs==1) {show_line = FALSE} else {show_line = TRUE}

    if (min_n_obs==1 && missing(xlim)) {xlim <- c(min(data$date)-365,max(data$date)+365)}
  }


  # ___________________
  # Basic plot set up
  # ___________________
  data <- apply_scenarios(data, scenarios, scenario_labels, scenario_order)

  x_axis <- build_x_axis(x = data$date,
                         xlab = xlab,
                         xlim = xlim,
                         xbreaks = xbreaks,
                         xlabels = xlabels,
                         financial_year = financial_year,
                         show_dates_on_axis = show_dates_on_axis,
                         expand_upper = 0,
                         xangle = xangle,
                         is_date = TRUE)

  y_axis <- build_y_axis(y = data$exp,
                         ylab = ylab,
                         ylim = ylim,
                         ybreaks = ybreaks,
                         ylabels = ylabels,
                         lower = 0,
                         upper = data$upper)

  p <- ggplot2::ggplot(data)
  p <- add_x_scale_date(p, x_axis)
  p <- add_y_scale_continuous(p, y_axis)


  # ___________________
  # Build MLE plot
  # ___________________
  if (!MCMC) {
    # Model inputs
    if (show_CI_ribbon){
      p <- p +
        ggplot2::geom_ribbon(ggplot2::aes(x=date, ymin = lower, ymax= upper, group = fleet), alpha = 0.1, group = 1) +
        ggplot2::scale_fill_manual(c(""),values="grey12", labels="95% confidence interval")
    }

    if (show_error_bar) {
      p <- p +
        ggplot2::geom_errorbar(ggplot2::aes(x=date,ymin=lower, ymax=upper), width=.5, position=ggplot2::position_dodge(0))
    }

    if (show_inputs) {
      if (show_line) {
        p <- p +
          ggplot2::geom_line(ggplot2::aes(x=date,y=obs,group=as.factor(fleet),colour=as.factor(fleet)),linewidth=0.75)
      }
      if (show_point) {
        p <- p +
          ggplot2::geom_point(ggplot2::aes(x=date,y=obs,group=as.factor(fleet),colour=as.factor(fleet)),size=point_size)
      }
    }

    # Model fits
    if (show_fits) { # Just line of model fits, no error on estimation
      if (show_line) {
        if (missing(fit_colour)) {
          p <- p +
            ggplot2::geom_line(ggplot2::aes(x=date,y=exp,group=as.factor(fleet), colour=as.factor(fleet), linetype="Model fit"))
        } else {
          p <- p +
            ggplot2::geom_line(ggplot2::aes(x=date,y=exp,group=as.factor(fleet), linetype="Model fit"), colour=fit_colour)
        }
        p <- p +
          ggplot2::scale_linetype_manual(values="dashed")
      }
    }

    p <- p +
      ggplot2::scale_colour_manual(values = colours, labels = fleet_names)

    # Remove legend if there is one fleet and set up for summary
    if (length(fleets)==1 & show_inputs & !show_fits & !show_CI_ribbon & !show_error_bar & show_point){
      p <- p + ggplot2::theme(legend.position="none")
    }
  }

  # ___________________
  # Build MCMC plot
  # ___________________

  # if (MCMC) {
  #   if (mcmc_style == "boxplot") p <- mcmc_boxplot(p, data, xlim, boxplot_outliers)
  #   if (mcmc_style == "banded")  p <- mcmc_banded(p, data, alpha, band_labels, band_colour)
  #   if (mcmc_style == "hairy")   p <- mcmc_hairy(p, data, hair_width)
  #   if (mcmc_style == "CI")      p <- mcmc_CI(p, data, aggregate_scenarios, CI_range, alpha)
  #   if (mcmc_style == "joy")     p <- mcmc_joy(p, data, CI_range, ridge_colour, rel_min_height, alpha, ridge_scale, show_CI,
  #                                              ybreaks, ylin,ylab, xlab, legend_position, text_size,xbreaks,legend_box,facet_wrap,
  #                                              show_median,xlabels,ylabels)
  #   # Add median lines
  #   p <- show_median_lines("catch rate",p,data,show_median,line_width,colours)
  #
  #
  #   # Add input data
  #   if (show_inputs) {
  #     data_fits <- data |> dplyr::filter(med=="trajectory") |> dplyr::mutate(med = "Input data")
  #
  #     if (show_observed_error) {
  #       p <- p +
  #         ggplot2::geom_linerange(data=data_fits, ggplot2::aes(x=date,y=obs,ymin=lower,ymax=upper), colour=input_range_colour)
  #     }
  #
  #     p <- p +
  #       ggplot2::geom_point(data=data_fits, ggplot2::aes(x=date,y=obs,shape=med), colour=input_colour) +
  #       ggplot2::scale_shape_manual(values=1,name="")
  #   }
  #
  #   # Add median lines
  #   p <- show_median_lines("catch rate",p,data,show_median,line_width,colours)
  #
  # }

  # ___________________
  # Final layers
  # ___________________
  if (facet_wrap) p <- add_scenario_facets(p, data, scales = scales, ncol = ncol)

  # ___________________
  # Theme
  # ___________________

  # set plot-specific override ONLY if user didn't specify
  # text_colour <- text_colour %||% "blue"
  # if (is.null(text_colour)) text_colour <- "blue"

  p <- add_ssand_theme(p,
                       text_size = text_size,
                       legend_text_size = legend_text_size,
                       text_colour = text_colour,
                       legend_text_colour = legend_text_colour,
                       legend_position = legend_position,
                       legend_box = legend_box,
                       legend_title_blank = legend_title_blank,
                       panel_border = panel_border,
                       panel_border_colour = panel_border_colour,
                       xangle = x_axis$angle)

  return(p)
}
