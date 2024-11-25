# Copyright 2024 Fisheries Queensland

# This file is part of SSAND.
# SSAND is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
# SSAND is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
# You should have received a copy of the GNU General Public License along with SSAND. If not, see <https://www.gnu.org/licenses/>.

#' Biomass plot
#'
#' MLE biomass plot displays 95% confidence interval
#' Multiple options are available for MCMC plots.
#' The variable "mcmc_type" can be set to "hairy", "boxplot", "banded", "joy" or "CI" to display different plot types.
#' On top of this, one or more median types can be overlaid using "show_median".
#' The types of median to be shown are "annual_biomass" or "trajectory".
#'
#' @param data If MCMC is being used, a data frame with variables called rownum, scenario, year, value, med, interval, prob_lower, prob_upper, biomass_type, biomass_definition. If MLE, a data frame with variables called year, value, lower, upper, scenario, biomass_type, biomass_definition.
#' @param xlab Label for x-axis (character). Default is "Year".
#' @param ylab Label for y-axis (character). Default is "Spawning biomass (relative)".
#' @param xbreaks A vector of breaks between x-axis labels, used in ggplot2::scale_x_continous() (numeric).
#' @param ybreaks A vector of breaks between y-axis labels, used in ggplot2::scale_y_continous() (numeric).
#' @param xlabels A vector of labels for the x-axis breaks.
#' @param ylabels A vector of labels for the y-axis breaks.
#' @param xlim A vector of lower and upper x-axis limits (e.g. c(1950, 2020)) (numeric).
#' @param ylim A vector of lower and upper y-axis limits (e.g. c(0,1)) (numeric).
#' @param xangle Set to 90 to rotate x-axis labels 90 degrees.
#' @param colours A vector of colours used for median types (character).
#' @param legend_position Position of the legend ("none", "left", "right", "bottom", "top", or two-element numeric vector for x and y position). Default is "top".
#' @param annotation_position Horizontal position of annotation
#' @param financial_year Set to TRUE if the assessment was based on financial year (logical). Adjusts the x-axis to show full financial year notation.
#' @param text_size text_size, default 12
#' @param show_target_line Set to TRUE to show target reference point line (logical).
#' @param target_value target reference point, default is 0.6. Colour for line is second element of colours
#' @param show_limit_line show limit reference point
#' @param limit_value limit reference point, default is 0.2. Colour for line is third element of colours
#' @param scenarios A vector of scenario numbers to be shown on plot (numeric). This was already specified in prep file, but this is a manual override to save running the prep function again.
#' @param scenario_labels A vector of customised scenario names (character). Default is "Scenario 1", "Scenario 2", etc.
#' @param scenario_order A vector to reorder how scenarios are displayed (character). Use the label names defined in "scenario_labels".
#' If "scenario_labels" is left blank, the labels will be "Scenario 1", "Scenario 2" etc.
#' Any scenarios not included in "scenario_order" will be tacked on in the order they appear in the input data.
#' @param scales Scales for ggplot2::facet_wrap(). Default is 'fixed', see ?ggplot2::facet_wrap for options.
#' @param ncol Number of columns for facet_wrap(). Default is 2.
#' @param sample Number of samples to plot from each MCMC chain to ease burden of rendering dense plots (numeric).
#' @param alpha Transparency for range (numeric) used in ggplot2::geom_density_ridges(). Default is 0.7.
#' @param show_median Type of median shown. Default "annual_biomass" shows the median of each year,
#' "trajectory" shows median trajectory based on biomass in final year,
#' "parameters" uses median of each parameter (not yet implemented)
#' @param mcmc_style The type of MCMC plot to be displayed (character). Options are "banded", "hairy", "boxplot", "CI" and "joy", the default is "banded". Only one option can be selected.
#' @param aggregate_scenarios Set to TRUE to calculate credible intervals across all scenarios (logical). Only activated if mcmc_style==CI.
#' @param CI_range Specify credible interval range (numeric). Only activated if mcmc_style==CI.
#' @param line_width Width of median lines (numeric). Default is 1.
#' @param hair_width Width of fine MCMC hairs (numeric). Default is 0.5.
#' @param legend_box Display option for legend (character). Choose "vertical" to stack legend types vertically, or "horizontal" to keep legends in one row.
#' @param show_CI Set to TRUE to show CI range on joy plot (logical).
#' @param rel_min_height Only used when mcmc_style=="joy". Lines with heights below this cutoff will be removed (passed to geom_density_ridges). Default is 0.01.
#' @param ridge_scale Only used when mcmc_style=="joy". Scale the height of the ridgelines relative to the spacing between them (passed to geom_density_ridges). Default is 4.5.
#' @param ridge_colour Two-element vector for the fill and outline of ridges (character). Only used when mcmc_style=="joy".
#' @param shapes Vector of shapes to denote different median types. Only used when mcmc_style=="joy".
#' @param band_colour Colour of bands (character). Only used when mcmc_style=="banded". Input one colour, bands will be distinguished using an alpha.
#' @param show_final_biomass Set to TRUE to show final biomass value at the end of the time series.
#'
#' @return Biomass plot
#' @export
#'
#' @examples
#' data <- biomassplot_prep_DD(dd_mle)
#' biomassplot(data)
#'
#' data <- biomassplot_prep_SS(ss_mle, ss_mcmc)
#' biomassplot(data, mcmc_style = "banded", show_median = c("annual_biomass","trajectory"))
#' biomassplot(data, mcmc_style = "boxplot", show_median = c("annual_biomass","trajectory"))
#' biomassplot(data, mcmc_style = "hairy", show_median = c("annual_biomass","trajectory"))
#' biomassplot(data, mcmc_style = "CI", show_median = c("annual_biomass","trajectory"), CI_range = 0.9)
#' biomassplot(data, mcmc_style = "joy", show_median = c("none"))
biomassplot <- function(data,
                        xlab = "Year",
                        ylab = NULL, #"Spawning biomass (relative)",
                        xbreaks = NULL,
                        ybreaks = NULL,
                        xlabels = NULL,
                        ylabels = NULL,
                        xlim = NULL,
                        ylim = NULL,
                        xangle = NULL,
                        colours = NULL,
                        legend_position= "top",
                        annotation_position = NULL,
                        financial_year = FALSE,
                        text_size = 12,
                        show_target_line = TRUE,
                        target_value = 0.6,
                        show_limit_line = TRUE,
                        limit_value = 0.2,
                        scenarios = NULL,
                        scenario_labels = NULL,
                        scenario_order = NULL,
                        scales = 'fixed',
                        ncol = 2,
                        sample = NULL,
                        alpha = NULL,
                        show_median = c("trajectory","annual_biomass"), # trajectory, annual_biomass, parameters, none
                        mcmc_style = "banded", # hairy, boxplot, banded, CI, joy
                        aggregate_scenarios = FALSE,
                        CI_range = 0.95,
                        show_CI = TRUE,
                        line_width = 1,
                        hair_width = 0.5,
                        legend_box = "horizontal",
                        rel_min_height = 0.01,
                        ridge_scale = 4.5,
                        ridge_colour = c("grey30","black"),
                        shapes = c(16,18,17),
                        band_colour = "black",
                        show_final_biomass = FALSE) {

  # Standard plot data set up
  MCMC <- "med" %in% names(data)

  # Data input warnings
  if (!MCMC & !"year"          %in% names(data)) {warning("Input data is missing year column")}
  if (!MCMC & !"value"         %in% names(data)) {warning("Input data is missing value column")}
  if (!MCMC & !"upper"         %in% names(data)) {warning("Input data is missing upper column")}
  if (!MCMC & !"lower"         %in% names(data)) {warning("Input data is missing lower column")}
  if (!MCMC & !"scenario"      %in% names(data)) {warning("Input data is missing scenario column")}
  if (!MCMC & !"biomass_type"  %in% names(data)) {warning("Input data is missing biomass_type column")}

  if (MCMC & !"rownum"        %in% names(data)) {warning("Input data is missing rownum column")}
  if (MCMC & !"scenario"      %in% names(data)) {warning("Input data is missing scenario column")}
  if (MCMC & !"year"          %in% names(data)) {warning("Input data is missing year column")}
  if (MCMC & !"value"         %in% names(data)) {warning("Input data is missing value column")}
  if (MCMC & !"interval"      %in% names(data)) {warning("Input data is missing interval column")}
  if (MCMC & !"prob_lower"    %in% names(data)) {warning("Input data is missing prob_lower column")}
  if (MCMC & !"prob_upper"    %in% names(data)) {warning("Input data is missing prob_upper column")}
  if (MCMC & !"biomass_type"  %in% names(data)) {warning("Input data is missing biomass_type column")}

  biomass_type <- data$biomass_type[1]
  biomass_definition_label <- ifelse(data$biomass_definition == 'spawning', 'Spawning', 'Vulnerable')

  if (missing(ylab)) {ylab <- ifelse(biomass_type=="relative", paste0(biomass_definition_label," biomass (relative)"),
                                     paste0(biomass_definition_label," biomass"))}

  if (financial_year & xlab=="Year") {warning("Your x-axis implies calendar year, but you've indicated you're using financial year.")}

  if (missing(xlim) & show_final_biomass==FALSE) {xlim <- c(min(data$year),max(data$year))}
  if (missing(xlim) & show_final_biomass==TRUE) {xlim <- c(min(data$year),max(data$year)+1)}
  if (MCMC & missing(ylim)) {ylim <- c(0,max(max(data$value,na.rm = T),max(data$prob_upper,na.rm = T)))}
  if (!MCMC & missing(ylim)) {ylim <- c(0,max(max(data$value,na.rm = T),max(data$upper,na.rm = T)))}

  if (missing(xbreaks)) {xbreaks <- pretty(xlim)}
  if (missing(ybreaks)) {ybreaks <- pretty(ylim)}

  if (missing(xlabels)) {xlabels <- xbreaks}
  if (missing(ylabels)) {ylabels <- ybreaks}

  if (financial_year) {xlabels <- paste0(xbreaks-1,"\U2013",xbreaks)} else {xlabels <- xbreaks}
  if (missing(xangle)) {xangle <- ifelse(financial_year==TRUE,90,0)}

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

  # Take a subsample of MCMC runs if specified
  if (MCMC & !missing(sample)) {
    data2 <- data |> dplyr::filter(med == "trajectory")

    data3 <- data |>
      dplyr::filter(med == "MCMC") |>
      dplyr::filter(rownum %in% sample(unique(data$rownum), size=sample))

    data <- rbind(data2, data3)
  }

  # MCMC warnings
  if (MCMC & !any(show_median %in% c('annual_biomass', 'trajectory', 'parameters', 'none'))){warning('Please check show_median options using ?biomassplot.')}
  if (MCMC & any(show_median %in% c('Parameters'))){warning('Median parameters feature not available yet.')}
  if (MCMC & length(mcmc_style)>1) {warning("You can only select one mcmc_type at a time.")}

  # Determine axis settings if missing
  if (length(unique(data$scenario))>1 & !aggregate_scenarios) {facet_wrap=TRUE} else {facet_wrap=FALSE}
  if (missing(annotation_position)) {annotation_position = min(data$year)+1}

  # Determine aesthetics if missing
  if (!MCMC & missing(colours)) {colours <- "black"}
  if (MCMC & missing(colours)) {colours <- c(c("#7CC8FC", "#FFC000", "#773158", "#01917C"))} # fq_palette("alisecolours"). Colour blind friendly, bright against the grey, not red or green
  if (missing(alpha)) {alpha=0.7}

  if (!mcmc_style == "joy") {
    # Build MLE plot
    if (!MCMC) {
      p <- ggplot2::ggplot(data) +
        ggplot2::geom_line(ggplot2::aes(x=year,y=value, colour="A")) +
        ggplot2::scale_colour_manual(c("","",""),values=colours,labels = c("Estimate")) +
        ggplot2::scale_fill_manual("",values="grey12")

      if (show_CI) {
        p <- p +
          ggplot2::geom_ribbon(ggplot2::aes(x=year,ymin=lower,ymax=upper,fill="95% confidence interval"), alpha = 0.2)
      }

      if (show_final_biomass) {
        p <- p +
          ggrepel::geom_text_repel(
            data = subset(data, year == max(data$year)),
            ggplot2::aes(
              x = year,
              y = value,
              label = paste0(round(subset(data, year == max(data$year))$value,2)*100,"%"),
              colour= subset(data, year == max(data$year))$colour_categories),
            size = 4,
            nudge_x = .5,
            nudge_y = 0.1,
            segment.color = '#cccccc',
            segment.size = 0.5,
            show.legend  = FALSE,
            max.overlaps = Inf)

      }

    }

    # Build MCMC plot (not joy plot)
    if (MCMC) {
      # Box plot
      if (mcmc_style == "boxplot") {
        databox <- data |>
          dplyr::filter(rownum > 0)

        # Expand limits of x-axis to include box
        xlim[1] <- xlim[1]-0.5
        xlim[2] <- xlim[2]+0.5

        p <- ggplot2::ggplot(data) +
          ggplot2::geom_boxplot(data = databox, ggplot2::aes(x=year, y=value, group=year))
      }

      # Banded plot
      if (mcmc_style == "banded") {
        tmp <- unique(data$interval)[!is.na(unique(data$interval))]
        alpha_scale <- seq(round(1/length(tmp),2),1,round(1/length(tmp),2))^2 + 0.1
        alpha_scale <- alpha_scale/max(alpha_scale)

        p <- ggplot2::ggplot(data) +
          ggplot2::geom_ribbon(data = data |> dplyr::filter(!is.na(interval)),
                               ggplot2::aes(x=year, ymin=prob_lower, ymax=prob_upper, group=interval, alpha=as.factor(-interval)),
                               fill=band_colour) +
          ggplot2::scale_alpha_manual(values = alpha_scale,
                                      labels = unique(data$interval)[!is.na(unique(data$interval))],
                                      name = "Credible interval")

      }

      # Hairy plot
      if (mcmc_style == "hairy") {
        p <- ggplot2::ggplot(data) +
          ggplot2::geom_line(data = data |> dplyr::filter(med == "MCMC"),
                             ggplot2::aes(x=year,y=value, group=rownum), colour = 'grey20', linewidth=hair_width, alpha = 1)
      }


      # Credible interval
      if (mcmc_style == "CI") {
        if (aggregate_scenarios) {
          dataCI <- data |>
            dplyr::filter(med=="MCMC") |>
            dplyr::group_by(year) |>
            dplyr::summarise(upper = quantile(value,probs=1-(1-CI_range)/2),
                             lower = quantile(value,probs=(1-CI_range)/2),
                             .groups = 'drop')
        } else {
          dataCI <- data |>
            dplyr::filter(med=="MCMC") |>
            dplyr::group_by(scenario_labels,year) |>
            dplyr::summarise(upper = quantile(value,probs=1-(1-CI_range)/2),
                             lower = quantile(value,probs=(1-CI_range)/2),
                             .groups = 'drop')
        }
        p <- ggplot2::ggplot(data) +
          ggplot2::geom_ribbon(data = dataCI, ggplot2::aes(x=year, ymax=upper, ymin = lower), fill = "grey60", alpha = alpha)
      }


      # Add median lines
      if (!"none" %in% show_median) {
        data_med <- data |>
          dplyr::filter(med %in% show_median) |>
          dplyr::mutate(med = dplyr::recode(med,
                                            "annual_biomass" = "Median annual biomass",
                                            "trajectory" = "Median trajectory",
                                            "parameters" = "Median parameters"))

        p <- p +
          ggplot2::geom_line(data=data_med, ggplot2::aes(x=year,y=value, colour=med), linewidth=line_width) +
          ggplot2::scale_color_manual(values = colours, name = ggplot2::element_blank())
      }

      if (show_final_biomass) {

        final_biomass <- data |>
          dplyr::group_by(scenario_labels) |>
          dplyr::summarise(year = max(year,na.rm = TRUE)) |>
          dplyr::left_join(
            data |> dplyr::filter(med == "annual_biomass"), by = c("scenario_labels", "year")
          )

        p <- p +
          ggrepel::geom_text_repel(
            data = final_biomass,
            ggplot2::aes(
              x = year,
              y = value,
              label = paste0(round(value,2)*100,"%"),
              colour= subset(data, year == max(data$year))$colour_categories#
            ),
            nudge_x = 0.5,
            nudge_y = 0.1,
            size = 4,
            segment.color = '#cccccc',
            segment.size = 0.5,
            show.legend  = FALSE,
            max.overlaps = Inf)
      }
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



    # Customisable features
    if (biomass_type == "relative") {
      if (show_target_line) {
        p <- p +
          ggplot2::geom_hline(yintercept = target_value, color="#127B06", linetype="solid",alpha=0.5) +
          ggplot2::annotate("text", x = annotation_position, y = target_value+0.02, color="#127B06", label = "Target reference point", size=3,hjust = 0)
      }

      if (show_limit_line) {
        p <- p +
          ggplot2::geom_hline(yintercept = limit_value, color="#AD3D25",linetype="solid",alpha=0.5) +
          ggplot2::annotate("text", x = annotation_position, y = limit_value+0.02, color="#AD3D25", label = "Limit reference point", size=3,hjust = 0)
      }
    }


    if (biomass_type == "absolute") {

      data <- data |>
        dplyr::group_by(scenario_labels) |>
        dplyr::mutate(target_value = ifelse(dplyr::row_number()==1, dplyr::first(value)*target_value, NA),
                      limit_value  = ifelse(dplyr::row_number()==1, dplyr::first(value)*limit_value, NA)) |>
        dplyr::ungroup() |>
        dplyr::mutate(annotation_position = annotation_position)

      if (show_target_line) {
        p <- p +
          ggplot2::geom_hline(data=data[1,], ggplot2::aes(yintercept = target_value), color="#127B06", linetype="solid",alpha=0.5) +
          ggplot2::geom_text(data=data[1,], ggplot2::aes(x = annotation_position, y = target_value), color="#127B06", label = "Target reference point", size=3,hjust = 0,vjust = 0, nudge_y = 0.5)
      }

      if (show_limit_line) {
        p <- p +
          ggplot2::geom_hline(data=data[1,], ggplot2::aes(yintercept = limit_value), color="#AD3D25",linetype="solid",alpha=0.5) +
          ggplot2::geom_text(data=data[1,], ggplot2::aes(x = annotation_position, y = limit_value), color="#AD3D25", label = "Limit reference point", size=3,hjust = 0,vjust = 0, nudge_y = 0.5)
      }
    }

    # Facet wrap
    if (facet_wrap) {
      suppressMessages({
        p <- p +
          ggplot2::scale_x_continuous(limits = xlim, breaks = xbreaks, labels = xlabels) +
          ggplot2::scale_y_continuous(limits = ylim, breaks = ybreaks, labels = ylabels) +
          ggplot2::facet_wrap(~scenario_labels, ncol = ncol, scales = scales)

      })
    }

  }

  # Joy plot
  # Joy plot is different to the others as years need to be factors, and the x and y axes are swapped.
  # There might be a more clever way to do this that we'll think of one day 🤷️
  if (MCMC & mcmc_style == "joy") {
    datajoy <- data |>
      dplyr::mutate(yearf = as.factor(year))

    datajoy95 <- datajoy |>
      dplyr::group_by(yearf) |>
      dplyr::summarise(min = quantile(value, probs = (1-CI_range)/2),
                       max = quantile(value, probs = 1-(1-CI_range)/2)) |>
      dplyr::mutate(linetype = paste0(CI_range*100,"% credible interval"))

    p <- ggplot2::ggplot(data=datajoy) +
      ggridges::geom_density_ridges(ggplot2::aes(x = value, y = yearf, group = yearf),
                                    fill = ridge_colour[1],
                                    colour = ridge_colour[2],
                                    rel_min_height = rel_min_height,
                                    alpha = alpha,
                                    scale = ridge_scale)

    if (show_CI){
      p <- p +
        ggplot2::geom_segment(data = datajoy95, ggplot2::aes(x = min, xend = min, y = as.numeric(yearf), yend = as.numeric(yearf) + 0.75, linetype = linetype), color = "black") + # colours[3]
        ggplot2::geom_segment(data = datajoy95, ggplot2::aes(x = max, xend = max, y = as.numeric(yearf), yend = as.numeric(yearf) + 0.75, linetype = linetype), color = "black") + # colours[3]
        ggplot2::scale_linetype_manual(name = ggplot2::element_blank(), values = "solid")
    }

    p <- p +
      ggplot2::scale_x_continuous(breaks = ybreaks, limits = ylim) +
      ggplot2::theme_bw() +
      ggplot2::xlab(ylab) +
      ggplot2::ylab(xlab) +
      ggplot2::theme(legend.position=legend_position) +
      ggplot2::theme(legend.text = ggplot2::element_text(size=text_size)) +
      ggplot2::theme(text = ggplot2::element_text(size=text_size)) +
      ggplot2::scale_y_discrete(breaks = xbreaks) +
      ggplot2::theme(legend.box=legend_box)


    if (facet_wrap) {

      suppressMessages({
        p <- p +
          ggplot2::scale_x_continuous(limits = c(NA,NA), breaks = xbreaks, labels = xlabels) +
          ggplot2::scale_y_discrete(limits = c(NA,NA), breaks = ybreaks, labels = ylabels) +
          ggplot2::facet_wrap(~scenario_labels, ncol = ncol, scales = scales)

      })

    }

    # Add median lines
    if (!"none" %in% show_median) {
      data_med <- data |>
        dplyr::filter(med %in% show_median) |>
        dplyr::mutate(med = dplyr::recode(med,
                                          "annual_biomass" = "Median annual biomass",
                                          "trajectory" = "Median trajectory",
                                          "parameters" = "Median parameters")) |>
        dplyr::mutate(yearf = as.factor(year))

      p <- p +
        ggplot2::geom_point(data=data_med, ggplot2::aes(y=yearf, x=value, colour=med, shape=med)) +
        ggplot2::scale_colour_manual(values = colours, name = ggplot2::element_blank()) +
        ggplot2::scale_shape_manual(values = shapes, name = ggplot2::element_blank())

      # It would be much nicer for the medians to be displayed using lines, however this is difficult
      # as the x-axis is coded as a factor and the axes are later flipped.

      # This is an example I found that achieves this, however I can't get it to work for this data:
      # https://stackoverflow.com/questions/16350720/using-geom-line-with-x-axis-being-factors
      # hist <- data.frame(date=Sys.Date() + 0:13, counts=1:14)
      # hist <- transform(hist, weekday=factor(weekdays(date), levels=c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday')))
      # ggplot2::ggplot(hist, ggplot2::aes(x=weekday, y=counts, group=1)) +
      #   ggplot2::geom_point(stat='summary', fun=sum) +
      #   ggplot2::stat_summary(fun=sum, geom="line")

      # q <- q +
      #   ggplot2::geom_point(data=data_med, ggplot2::aes(y=yearf, x=value, colour=med, shape=med, group=1), stat='summary', fun=sum) +
      #   ggplot2::stat_summary(data=data_med, ggplot2::aes(x=year,y=value, group=1),fun=sum, geom="line")
    }

    if (show_target_line) {
      p <- p +
        ggplot2::geom_vline(xintercept = target_value, color="#127B06", linetype="solid",alpha=0.5) +
        ggplot2::annotate("text", y = as.factor(annotation_position), x = target_value+0.02, color="#127B06", label = "Target reference point", size=3,hjust = 0)
    }

    if (show_limit_line) {
      p <- p +
        ggplot2::geom_vline(xintercept = limit_value, color="#AD3D25",linetype="solid",alpha=0.5) +
        ggplot2::annotate("text", y = as.factor(annotation_position), x = limit_value+0.02, color="#AD3D25", label = "Limit reference point", size=3,hjust = 0)
    }
    p <- p +
      ggplot2::coord_flip(expand = TRUE, clip = "on")
  }
  return(p)
}
