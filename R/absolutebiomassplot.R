# Copyright 2024 Fisheries Queensland

# This file is part of SSAND.
# SSAND is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
# SSAND is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
# You should have received a copy of the GNU General Public License along with SSAND. If not, see <https://www.gnu.org/licenses/>.

#' Absolute biomass plot
#'
#' @param data Output from absolutebiomassplot_prep(). A data frame with variables date (date), fleet (int), obs (num), exp (num), ub (num), lb (num), scenario (fac)
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
#' "trajectory" shows median trajectory based on biomass in final year,
#' "parameters" uses median of each parameter (not yet implemented)
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
#'
#' @return A plot that shows input data and model fits to CPUE data
#' @export
#'
#' @examples
#' \dontrun{
#' data <- absolutebiomassplot_prep_DD(dd_mle)
#' absolutebiomassplot(data)
#' absolutebiomassplot(data, xlim=c(1954,2023))
#' }
absolutebiomassplot <- function(data,
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
                                colours = NULL,
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
                                show_median = "median_cpue", # trajectory, median_cpue, parameters, none
                                mcmc_style = "banded", # hairy, boxplot, banded, CI, joy
                                line_width = 1,
                                hair_width = 0.5,
                                legend_box = "horizontal",
                                input_colour = "black",
                                input_range_colour = "black",
                                aggregate_scenarios = FALSE,
                                CI_range = 0.95,
                                alpha = NULL) {

  # 📐Set up ----
  if ("med" %in% names(data)) {MCMC <- TRUE} else {MCMC <- FALSE}

  # Data input warnings
  if (!MCMC & !"date" %in% names(data)) {warning("Input data is missing date column")}
  if (!MCMC & !"fleet" %in% names(data)) {warning("Input data is missing fleet column")}
  if (!MCMC & !"obs" %in% names(data)) {warning("Input data is missing obs column")}
  if (!MCMC & !"exp" %in% names(data)) {warning("Input data is missing exp column")}
  if (!MCMC & !"ub" %in% names(data)) {warning("Input data is missing ub column")}
  if (!MCMC & !"lb" %in% names(data)) {warning("Input data is missing lb column")}
  if (!MCMC & !"scenario" %in% names(data)) {warning("Input data is missing scenario column")}

  if (MCMC & !"year" %in% names(data)) {warning("Input data is missing year column")}
  if (MCMC & !"month" %in% names(data)) {warning("Input data is missing month column")}
  if (MCMC & !"fleet" %in% names(data)) {warning("Input data is missing fleet column")}
  if (MCMC & !"obs" %in% names(data)) {warning("Input data is missing obs column")}
  if (MCMC & !"exp" %in% names(data)) {warning("Input data is missing exp column")}
  if (MCMC & !"ub" %in% names(data)) {warning("Input data is missing ub column")}
  if (MCMC & !"lb" %in% names(data)) {warning("Input data is missing lb column")}
  if (MCMC & !"rownum" %in% names(data)) {warning("Input data is missing rownum column")}
  if (MCMC & !"med" %in% names(data)) {warning("Input data is missing med column")}
  if (MCMC & !"interval" %in% names(data)) {warning("Input data is missing interval column")}
  if (MCMC & !"date" %in% names(data)) {warning("Input data is missing date column")}
  if (MCMC & !"scenario" %in% names(data)) {warning("Input data is missing scenario column")}

  if (!show_negative) {data <- dplyr::mutate(data, lb = ifelse(lb<0,0,lb))}

  if (financial_year & xlab=="Year") {warning("Your x-axis implies calendar year, but you've indicated you're using financial year.")}

  if (!missing(fleets)) {data <- data |> dplyr::filter(fleet %in% fleets)}


  # If xlim is entered as just years, convert to dates
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


  if (missing(xlim)) {xlim <- c(min(data$date),max(data$date)+1)}
  if (missing(ylim)) {ylim <- c(0,max(data$ub, na.rm=TRUE))}

  if (missing(xbreaks)) {xbreaks <- pretty(xlim)}
  if (missing(ybreaks)) {ybreaks <- pretty(ylim)}
  if (missing(ylabels)) {ylabels <- ybreaks}


  if (missing(xlabels) & !financial_year & show_dates_on_axis) {xlabels <- xbreaks}
  if (missing(xlabels) & !financial_year & !show_dates_on_axis) {xlabels <- lubridate::year(xbreaks)}

  if (missing(xlabels) & financial_year & !show_dates_on_axis) {xlabels <- paste0(xbreaks-1,"\U2013",xbreaks)}
  if (missing(xlabels) & financial_year & show_dates_on_axis) {xlabels <- xbreaks}

  if (missing(xangle)) {xangle <- ifelse(financial_year,90,0)}

  if (missing(colours)) {colours <- c("black",fq_palette("cols"))}

  if (missing(fleets)) {fleets <- sort(unique(data$fleet))}

  if (missing(fleet_names)) {fleet_names <- paste0('Fleet ',sort(unique(data$fleet)))}

  if (!missing(scenarios)){data <- data |> dplyr::filter(scenario %in% scenarios)}

  if (missing(scenario_labels)) {
    data <- data |> dplyr::mutate(scenario_labels = as.factor(paste0("Scenario ",scenario)))
  } else {
    scenario.lookup<- data.frame(scenario = unique(data$scenario), scenario_labels = scenario_labels)
    data <- data |>
      dplyr::left_join(scenario.lookup, by = "scenario") |>
      dplyr::mutate(scenario_labels = as.factor(scenario_labels))
  }

  if (!missing(scenario_order)) {
    # Add on any scenarios not included in the scenario_order list
    scenario_order = c(scenario_order, setdiff(scenario_labels, scenario_order))
    # Reorder scenarios
    data$scenario_labels <- factor(data$scenario_labels, levels = scenario_order)
  }

  if (missing(alpha)) {alpha=0.7}


  # 🧮 MLE ----
  if (!MCMC) {

    # General
    p <- ggplot2::ggplot(data[which(data$fleet %in% as.character(fleets)),]) +
      ggplot2::theme_bw() +
      ggplot2::xlab(xlab) +
      ggplot2::ylab(ylab) +
      ggplot2::theme(text = ggplot2::element_text(size=text_size)) +
      ggplot2::theme(legend.title=ggplot2::element_blank(), legend.position = legend_position)

    # Model fits
    if (show_CI_ribbon){
      p <- p +
        ggplot2::geom_ribbon(ggplot2::aes(x=date, ymin = lb, ymax= ub, group = fleet), alpha = 0.1, group = 1) +
        ggplot2::scale_fill_manual(c(""),values="grey12", labels="95% confidence interval")
    }

    if (show_error_bar) {
      p <- p +
        ggplot2::geom_errorbar(ggplot2::aes(x=date,ymin=lb, ymax=ub), width=.5, position=ggplot2::position_dodge(0))
    }

    if (show_fits) { # Just line of model fits, no error on estimation
      if (show_line) {
        p <- p +
          ggplot2::geom_line(ggplot2::aes(x=date,y=exp,group=as.factor(fleet), colour=as.factor(fleet), linetype="Model fit")) +
          ggplot2::scale_linetype_manual(values="dashed")
      }
    }

    # Model inputs
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

    # General
    p <- p +
      ggplot2::scale_x_continuous(limits = xlim, breaks = xbreaks, labels = xlabels) +
      ggplot2::scale_y_continuous(limits = ylim) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = xangle, vjust = 0.5, hjust=ifelse(xangle==90,0,0.5)))

    if (length(unique(data$scenario))>1){
      p <- p +
        ggplot2::facet_wrap(~scenario_labels, scales = scales, ncol = ncol)
    }

    p <- p +
      ggplot2::theme(legend.title=ggplot2::element_blank(), legend.position = legend_position)

    p <- p +
      ggplot2::scale_colour_manual(values = colours, labels = fleet_names)


    # Remove legend if there is one fleet and set up for summary
    if (length(fleets)==1 & show_inputs & !show_fits & !show_CI_ribbon & !show_error_bar & show_point){
      p <- p +
        ggplot2::theme(legend.position="none")
    }
  }

  # 🍪 MCMC ----
  if (MCMC) {

    # Warnings
    if (MCMC & !any(show_median %in% c('median_cpue', 'trajectory', 'parameters', 'none'))){warning('Please check show_median options using ?cpueplot')}
    if (MCMC & any(show_median %in% c('Parameters'))){warning('Median parameters feature not available yet.')}
    if (MCMC & length(mcmc_style)>1) {warning("You can only select one mcmc_type at a time.")}

    # Subset MCMC iterations
    if (!missing(sample)) {
      data2 <- data |> dplyr::filter(med == "trajectory")

      data3 <- data |>
        dplyr::filter(med == "MCMC") |>
        dplyr::filter(rownum %in% sample(unique(data$rownum), size=sample))

      data <- rbind(data2, data3)
    }

    # Box plot
    if (mcmc_style == "boxplot") {
      databox <- data |>
        dplyr::filter(rownum > 0)

      # Expand limits of x-axis to include box
      xlim[1] <- xlim[1]-0.5
      xlim[2] <- xlim[2]+0.5

      p <- ggplot2::ggplot(data) +
        ggplot2::geom_boxplot(data = databox, ggplot2::aes(x=year, y=obs, group=date))
    }

    # Banded plot
    if (mcmc_style == "banded") {
      tmp <- unique(data$interval)[!is.na(unique(data$interval))]
      alpha_scale <- seq(round(1/length(tmp),2),1,round(1/length(tmp),2))^2 + 0.1
      alpha_scale <- alpha_scale/max(alpha_scale)

      p <- ggplot2::ggplot(data) +
        ggplot2::geom_ribbon(data = data |> dplyr::filter(!is.na(interval)),
                             ggplot2::aes(x=date, ymin=lb, ymax=ub, group=interval, alpha=as.factor(-interval)),
                             fill=band_colour) +
        ggplot2::scale_alpha_manual(values = alpha_scale,
                                    labels = rev(unique(data$interval)[!is.na(unique(data$interval))]),
                                    name = "Credible interval")
    }

    # Hairy plot
    if (mcmc_style == "hairy") {
      p <- ggplot2::ggplot(data) +
        ggplot2::geom_line(data = data |> dplyr::filter(med == "MCMC"),
                           ggplot2::aes(x=date,y=exp, group=rownum), colour = 'grey20', linewidth=hair_width, alpha = 1)
    }


    # Credible interval
    if (mcmc_style == "CI") {
      if (aggregate_scenarios) {
        dataCI <- data |>
          dplyr::filter(med=="MCMC") |>
          dplyr::group_by(date) |>
          dplyr::summarise(upper = quantile(exp,probs=1-(1-CI_range)/2),
                           lower = quantile(exp,probs=(1-CI_range)/2),
                           .groups = 'drop') |>
          dplyr::mutate(fill_label = paste0(CI_range*100,"% credible interval"))
      } else {
        dataCI <- data |>
          dplyr::filter(med=="MCMC") |>
          dplyr::group_by(scenario_labels,date) |>
          dplyr::summarise(upper = quantile(exp,probs=1-(1-CI_range)/2),
                           lower = quantile(exp,probs=(1-CI_range)/2),
                           .groups = 'drop') |>
          dplyr::mutate(fill_label = paste0(CI_range*100,"% credible interval"))
      }
      p <- ggplot2::ggplot(data) +
        ggplot2::geom_ribbon(data = dataCI, ggplot2::aes(x=date, ymax=upper, ymin = lower, fill = fill_label), alpha = alpha) +
        ggplot2::scale_fill_manual(values = "grey60", name="")
    }

    # Add input data
    if (show_inputs) {
      data_fits <- data |> dplyr::filter(med=="trajectory") |> dplyr::mutate(med = "Input data")


      if (show_observed_error) {
        p <- p +
          ggplot2::geom_linerange(data=data_fits, ggplot2::aes(x=date,y=obs,ymin=lb,ymax=ub), colour=input_range_colour)
      }

      p <- p +
        ggplot2::geom_point(data=data_fits, ggplot2::aes(x=date,y=obs,shape=med), colour=input_colour) +
        ggplot2::scale_shape_manual(values=1,name="")
    }


    # Add median lines
    if (!"none" %in% show_median) {
      data_med <- data |>
        dplyr::filter(med %in% show_median) |>
        dplyr::mutate(med = dplyr::recode(med,
                                          "median_cpue" = "Median catch rate",
                                          "trajectory" = "Median trajectory",
                                          "parameters" = "Median parameters"))

      p <- p +
        ggplot2::geom_line(data=data_med, ggplot2::aes(x=date,y=exp, colour=med), linewidth=line_width) +
        ggplot2::scale_color_manual(values = colours, name = ggplot2::element_blank())
    }

    # Universal aesthetics
    p <- p +
      ggplot2::scale_x_continuous(limits = xlim, breaks = xbreaks, labels = xlabels) +
      ggplot2::scale_y_continuous(limits = ylim, breaks = ybreaks, labels = ylabels) +
      ggplot2::theme_bw() +
      ggplot2::xlab(xlab) +
      ggplot2::ylab(ylab) +
      ggplot2::theme(legend.position=legend_position) +
      ggplot2::theme(legend.text = ggplot2::element_text(size=text_size)) +
      ggplot2::theme(text = ggplot2::element_text(size=text_size)) +
      ggplot2::theme(legend.box=legend_box) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = xangle, vjust = 0.5, hjust=ifelse(xangle==90,0,0.5)))

    # Facet wrap
    if (length(unique(data$scenario))>1) {
      suppressMessages({
        p <- p +
          ggplot2::scale_x_continuous(limits = xlim, breaks = xbreaks, labels = xlabels) +
          ggplot2::scale_y_continuous(limits = ylim, breaks = ybreaks, labels = ylabels) +
          ggplot2::facet_wrap(~scenario_labels, ncol = ncol, scales = scales)

      })
    }
  }
  return(p)
}

