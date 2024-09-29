# Copyright 2024 Fisheries Queensland

# This file is part of SSAND.
# SSAND is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
# SSAND is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
# You should have received a copy of the GNU General Public License along with SSAND. If not, see <https://www.gnu.org/licenses/>.

#' Management plot
#'
#' @param data Output of SSAND::biomassplot_prep(). MCMC or MLE inputs are accepted into either. See ?biomassplot_prep_DD or ?biomassplot_prep_SS for more information.
#' @param management_text A vector of management measures over time, each starting with the year and a colon. For example '2004: Rezoning of GBRMP'. Use back-slash followed by n to create a line-break
#' @param management_pos A vector of vertical positions, corresponding to the management info. Defaults to 1 for each management measure.
#' @param base_case The scenario number that pertains to the base case (numeric). If NA, no base case was selected.
#' @param xlab Label for x-axis (character). Default is "Year".
#' @param ylab Label for y-axis (character). Default is "Biomass (relative to unfished)".
#' @param xbreaks A vector of breaks between x-axis labels, used in ggplot2::scale_x_continous() (numeric).
#' @param xlim A vector of lower and upper x-axis limits (e.g. c(1950, 2020)) (numeric).
#' @param ylim A vector of lower and upper y-axis limits (e.g. c(0,1)) (numeric).
#' @param title Title of plot (recommend: "Species name (YEAR) stock")
#' @param subtitle Subtitle of plot (recommend: "Stock assessment biomass estimate and key management actions")
#' @param title_position Vertical position of title
#' @param subtitle_position Vertical position of subtitle
#' @param management_line_width Width of management line (recommend: 7)
#' @param label_offset Amount of offset for management labels (recommend: 0.3)
#' @param legend_position Two-element vector with proportional x and y position of top left corner of legend (default c(0.1, 0.4))
#' @param show_target Flag to include 60% target reference line (Boolean)
#' @param show_limit Flag to include 20% limit reference line (Boolean)
#' @param custom_reference_values Vector of heights for custom reference lines (e.g. c(0.4))
#' @param custom_reference_colours Vector of colours for custom reference lines (e.g. c("#FFC000"))
#' @param custom_reference_labels Vector of labels for custom reference lines (e.g. c('B40')
#' @param draft_text Watermark text (chr)
#' @param draft_position Vector of x and y position of watermark
#' @param draft_colour Colour of watermark
#' @param logo_scale Scale (as a percentage) of QG logo for overlay onto plot (recommend: 7, adjusting as necessary)
#' @param logo_padding Padding around QG logo as percentage of plot width and height (recommend: 0.05)
#' @param infographic_path Directory of infographic to be overlaid onto plot (use NA if not infographic exists)
#' @param infographic_position Vector of x and y position of top-left corner of infographic, as fraction of plot dimensions (e.g. c(0.5, 0.4))
#' @param infographic_scale Scale (multiplier) of infographic for overlay onto plot (defaults to 25% the plot width)
#' @param plotdir Filepath, including file name and extension (.png) of where final management plot should be saved
#' @param show_final_biomass Flag to show final biomass at the end of the trajectory. Only works if result==TRUE
#' @param show_result Set to TRUE if a result was found for the assessment (logical). Set to FALSE if no result was found (e.g. 2023 red spot king prawn)
#' @param scenarios A vector of scenario numbers to be shown on plot (numeric). This was already specified in prep file, but this is a manual override to save running the prep function again.
#' @param mcmc_style The type of MCMC plot to be displayed (character). Options are "banded", "hairy", "boxplot", "CI" and "joy", the default is "banded". Only one option can be selected.
#' @param band_colour Colour of bands (character). Only used when mcmc_style=="banded". Input one colour, bands will be distinguished using an alpha.
#' @param show_median Type of median shown. Default "annual_biomass" shows the median of each year,
#' "trajectory" shows median trajectory based on biomass in final year,
#' "parameters" uses median of each parameter (not yet implemented)
#' @param legend_name Name of legend
#' @param legend_entries A vector of labels for the legend.
#'
#' @return A management plot, saved in the location specified in plotdir
#' @export
#' @importFrom stats "median"
#' @importFrom stats "quantile"
#'
#' @examples
#' management_text <- c(
#' "1980: Management measure",
#' "1988: Management measure",
#' "2004: - Many things \n - happened in \n - this year",
#' "2010: Management measure",
#' "2021: - Many things \n - happened in \n - this year")
#'
#' management_pos <- c(0.8, 0.6, 1.0, 0.5, 0.2)
#'
#' data <- biomassplot_prep_SS(ss_mle)
#' # OR: data <- biomassplot_prep_SS(ss_mle, ss_mcmc)
#' # OR: data <- biomassplot_prep_DD(dd_mle)
#' # OR: data <- biomassplot_prep_DD(dd_mle, dd_mcmc, dd_sim)
#'
#' # Produce a management plot with a base case (scenario 1), result found,
#' # and the cloud is determined from the base case uncertainty:
#' \dontrun{
#' managementplot(data, management_text, management_pos,
#'                 base_case = 1, show_result = TRUE, scenarios = 1)
#' }
#'
#' # Produce a management plot with a base case (scenario 1), result found,
#' # and the cloud is determined from scenario uncertainty:
#' \dontrun{
#' managementplot(data, management_text, management_pos, base_case = 1,
#'                 show_result = TRUE, scenarios = unique(data$scenario))
#' }
#'
#' # Produce a management plot where no result was found (and therefore no base case),
#' # so the cloud is replaced with individual spaghetti strands:
#' \dontrun{
#' managementplot(data,management_text, management_pos, base_case = NA,
#'                 show_result = FALSE, scenarios = unique(data$scenario))
#' }
managementplot <- function(data,
                           management_text = NULL,
                           management_pos = NULL,
                           base_case,
                           xlab = "Year",
                           ylab = "Biomass (relative to unfished)",
                           xlim = c(min(data$year)-1,max(data$year)+1),
                           ylim = NULL,
                           xbreaks = NULL,
                           title = "Species (YEAR) east coast",
                           subtitle = "Stock assessment biomass estimate and key management actions",
                           title_position = NULL,
                           subtitle_position = NULL,
                           management_line_width = 7,
                           label_offset = 0.3,
                           legend_position = c(0.1,0.4),
                           show_result = TRUE,
                           scenarios = base_case,
                           show_target = TRUE,
                           show_limit = TRUE,
                           show_final_biomass = TRUE,
                           custom_reference_values = NULL,
                           custom_reference_colours = c("#FFC000"),
                           custom_reference_labels = c("B40"),
                           draft_text = "DRAFT",
                           draft_position = c(1970,0.1),
                           draft_colour = "red",
                           logo_scale = 10,
                           logo_padding = 0.05,
                           infographic_path = NA,
                           infographic_position = c(0.5,0.4),
                           infographic_scale = NULL,
                           plotdir = paste0(getwd(),'/management_plot.png'),
                           mcmc_style = "banded",
                           band_colour = "black",
                           show_median = "none",
                           legend_name = "Legend",
                           legend_entries = c("Biomass estimate","Range of biomass estimates","PLEASE CUSTOMISE legend_entries","x","x","x","x","x","x","x","x","x")
) {

  # Magick (image overlay) code adapted from https://themockup.blog/posts/2019-01-09-add-a-logo-to-your-plot/

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


  if (any(!grepl("^[0-9]+$", substr(management_text, 1, 4)))) {stop("Each item of management_text needs to begin with the year followed by a colon. For example '2004: Rezoning of GBRMP'. Try again.")}

  if (!missing(scenarios)){data <- data |> dplyr::filter(scenario %in% scenarios)}

  if (xlim[1] > min(data$year)) {warning('You have defined your x-axis to be shorter than your biomass timeseries. Decrease xlim[1] to include your entire timeseries')}
  if (xlim[2] < max(data$year)) {warning('You have defined your x-axis to be shorter than your biomass timeseries. Decrease xlim[1] to include your entire timeseries')}
  # if (ylim[2] < max(data$value)) {warning('You have set the upper y limit to be less than your greatest biomass estimate. Increase ylim[2] to include biomass estimates for all years.')}
  if (missing(xbreaks)) {xbreaks <- pretty(xlim) }

  year <- substr(management_text, 1, 4)
  managementinfo <- data.frame(year, management_text, management_pos)


  # If management actions start before data, add management action years to data set
  if (min(as.numeric(year)) < min(data$year)) {
    for (scenario in scenarios) {
      tmp <- data |> dplyr::filter(scenario==scenario)
      if (!MCMC) {
        data <- data |>
          rbind(tmp[1,] |> dplyr::mutate(year = min(as.numeric(year)), value=1, upper=1, lower=1))
      } else {
        for (i in length(unique(data$intervals))) {
          data <- data |>
            rbind(tmp[1,] |> dplyr::mutate(year = min(as.numeric(year)),
                                           value=1,
                                           prob_upper=1,
                                           prob_lower=1,
                                           interval= unique(data$intervals)[i]))
        }
      }
    }
  }

  if (missing(title_position)) {
    if (MCMC) {
      title_position <- max(data$value+0.05)
    } else {
      title_position <- max(data$upper+0.05)
    }
  }
  if (missing(subtitle_position)) {
    if (MCMC) {
      subtitle_position <- max(data$value+0.05)-0.06
    } else {
      subtitle_position <- max(data$upper+0.05)-0.06
    }
  }

  if (title_position > xlim[2]) {warning('You have placed the title outside of the range of the graph. Decrease title_position or increase ylim[2]')}
  if (subtitle_position > xlim[2]) {warning('You have placed the subtitle outside of the range of the graph. Decrease subtitle_position or increase ylim[2]')}

  if (MCMC & missing(ylim)) {ylim <- c(0, max(data$value)+0.07)}
  if (!MCMC & missing(ylim)) {ylim <- c(0, max(data$upper)+0.07)}

  if (show_result) {
    if (MCMC) {
      data <- data |>
        # dplyr::filter(med == 'Median') |>
        dplyr::left_join(data |>
                           dplyr::filter(med == 'MCMC') |>
                           dplyr::group_by(year) |>
                           dplyr::summarise(lower = quantile(value, probs = 0.025), upper = quantile(value, probs = 0.975)),
                         by = dplyr::join_by(year)) |>
        dplyr::mutate(year = as.character(year), base = value, upper_se = upper, lower_se = lower) |>
        dplyr::select(year, base, upper_se, lower_se, scenario) |>
        dplyr::left_join(managementinfo, by = dplyr::join_by(year)) |>
        dplyr::mutate(upordown = ifelse(management_pos>base,"up","down")) |>
        dplyr::select(year, management_text, management_pos, base, upordown, upper_se, lower_se, scenario) |>
        dplyr::mutate(year = as.numeric(year),
                      upper_se = as.numeric(upper_se),
                      lower_se = as.numeric(lower_se))

    } else {
      data <- data |>
        dplyr::rename(base = value, upper_se = upper, lower_se = lower) |>
        dplyr::mutate(year = as.character(year)) |>
        dplyr::left_join(managementinfo, by = dplyr::join_by(year)) |>
        dplyr::mutate(upordown = ifelse(management_pos>base,"up","down")) |>
        dplyr::select(year, management_text, management_pos, base, upordown, upper_se, lower_se, scenario) |>
        dplyr::mutate(year = as.numeric(year),
                      upper_se = as.numeric(upper_se),
                      lower_se = as.numeric(lower_se))
    }
  }

  if (!show_result) {
    if (MCMC) {
      data <- data |>
        dplyr::mutate(base = value, year = as.character(year)) |>
        dplyr::left_join(managementinfo, by = dplyr::join_by(year)) |>
        dplyr::mutate(upordown = ifelse(management_pos>base,"up","down")) |>
        dplyr::mutate(year = as.numeric(year))

    } else {
      data <- data |>
        dplyr::rename(year = year, base = value, upper_se = upper, lower_se = lower) |>
        dplyr::mutate(lower_se = NA, upper_se = NA) |>
        dplyr::mutate(year = as.character(year)) |>
        dplyr::left_join(managementinfo, by = dplyr::join_by(year)) |>
        dplyr::mutate(upordown = ifelse(management_pos>base,"up","down")) |>
        dplyr::select(year, management_text, management_pos, base, upordown, upper_se, lower_se, scenario) |>
        dplyr::mutate(year = as.numeric(year),
                      upper_se = as.numeric(upper_se),
                      lower_se = as.numeric(lower_se))
    }
  }

  # If many scenarios are listed, and show_result==1, that means the ribbon represents the range of scenarios
  if (length(scenarios) > 1 & show_result) {
    data <- data |>
      dplyr::group_by(year) |>
      dplyr::mutate(upper_se = max(upper_se),
                    lower_se = min(lower_se)) |>
      dplyr::ungroup()
  }

  # Remove duplicate manangement_text (and potentially conflicting upordown)
  if (!is.na(base_case)) {
    data <- data |>
      dplyr::mutate(management_text = ifelse(scenario==base_case,management_text,NA),
                    management_pos = ifelse(scenario==base_case,management_pos,NA),
                    upordown = ifelse(scenario==base_case,upordown,NA))
  } else {
    data <- data |>
      dplyr::mutate(management_text = ifelse(scenario==unique(data$scenario)[1],management_text,NA),
                    management_pos = ifelse(scenario==unique(data$scenario)[1],management_pos,NA),
                    upordown = ifelse(scenario==unique(data$scenario)[1],upordown,NA))
  }


  # GENERATE PLOT
  # Basic biomass trajectory and final label
  if (!MCMC) {

    if (show_result) {
      p <- ggplot2::ggplot(data) +
        ggplot2::geom_line(data = data |> dplyr::filter(scenario == base_case),
                           ggplot2::aes(x=year, y=base, color="A", linetype="A")) +
        ggplot2::geom_ribbon(ggplot2::aes(x=year,ymin=lower_se,ymax=upper_se, fill="A"), alpha = 0.1)
    } else {
      p <- ggplot2::ggplot(data) +
        ggplot2::geom_line(ggplot2::aes(x=year, y=base, color="A", linetype="A", group=scenario))
    }
  } else {

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
                                    labels = legend_entries,
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
      dataCI <- data |>
        dplyr::filter(med=="MCMC") |>
        dplyr::group_by(scenario_labels,year) |>
        dplyr::summarise(upper = quantile(value,probs=1-(1-CI_range)/2),
                         lower = quantile(value,probs=(1-CI_range)/2),
                         .groups = 'drop')

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
  }


  if (show_final_biomass) {
    if (!MCMC) {
      # Retrieve final biomass
      final <- round(data$base[nrow(data)] * 100)
      datalabelpos <- data$year[nrow(data)] + label_offset

      p  <- p +
        ggplot2::annotate("text", x = datalabelpos, y = final/100, label = paste0(final,"%"), hjust=0, vjust=1, size=3, color=SSAND::fq_palette("DAF")[1])

    } else {
      final_biomass <- data |>
        dplyr::group_by(scenario) |>
        dplyr::summarise(year = max(year,na.rm = TRUE)) |>
        dplyr::left_join(
          data |> dplyr::filter(med == "annual_biomass"), by = c("scenario", "year")
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


  # Theme and axes
  p <- p +
    ggplot2::theme_bw() +
    ggplot2::scale_x_continuous(limits = xlim,
                                breaks = xbreaks,
                                labels = xbreaks) +
    ggplot2::scale_y_continuous(limits = ylim,
                                breaks = seq(0, ylim[2], 0.2)) +
    ggplot2::theme(panel.grid = ggplot2::element_blank()) +
    ggplot2::xlab(xlab) +
    ggplot2::ylab(ylab)

  # 20% limit reference horizontal line
  if (show_limit) {
    p <- p +
      ggplot2::geom_hline(yintercept = 0.2, color=SSAND::fq_palette("biomass")[2], linetype="dotted",alpha=0.5) +
      ggplot2::annotate("text", x = xlim[1]+1, y = 0.22, color=SSAND::fq_palette("biomass")[2], label = 'paste(B[20*"%"]~limit~reference~point)', parse=TRUE, size=3, hjust = 0)
  }

  # 60% target reference horizontal line
  if (show_target) {
    p <- p +
      ggplot2::geom_hline(yintercept = 0.6, color=SSAND::fq_palette("biomass")[1], linetype="dotted",alpha=0.5) +
      ggplot2::annotate("text", x = xlim[1]+1, y = 0.62, color=SSAND::fq_palette("biomass")[1], label = 'paste(B[60*"%"]~target~reference~point)', parse=TRUE, size=3, hjust = 0)
  }

  # Custom reference point horizontal line
  if (length(custom_reference_values)>0) {
    for (i in 1:length(custom_reference_values)) {
      p <- p +
        ggplot2::geom_hline(yintercept = custom_reference_values[i], color=custom_reference_colours[i], linetype="dotted",alpha=0.5) +
        ggplot2::annotate("text", x = xlim[1]+1, y = custom_reference_values[i]+0.02, color=custom_reference_colours[i], label = custom_reference_labels[i], size=3, hjust = 0)
    }
  }

  if (show_result) {
    # Vertical management action lines
    p <- p +
      ggplot2::geom_segment(data = data[which(!is.na(data$management_pos)),],
                            ggplot2::aes(x=year, xend=year,
                                         y=base,
                                         yend=ifelse(upordown=="down",management_pos-0.01,management_pos+0.01)), colour="gray70")

    # Horizontal management action lines
    p <- p +
      ggplot2::geom_segment(data = data[which(!is.na(data$management_pos)),],
                            ggplot2::aes(x=year-management_line_width,
                                         y=ifelse(upordown=="down",management_pos-0.01,management_pos+0.01),
                                         xend=year, yend=ifelse(upordown=="down",management_pos-0.01,management_pos+0.01)), colour="gray70")

    # Management action text
    p <- p +
      ggplot2::geom_text(data = data[which(!is.na(data$management_pos)),],
                         ggplot2::aes(x=year-0.5,
                                      y=management_pos,
                                      label=management_text,
                                      hjust=1,
                                      vjust=ifelse(upordown=="down",0,1)),
                         size=3)
  } else {

    if (!MCMC) {
      managementlabel_simp <- data[which(!is.na(data$management_pos)),] |>
        dplyr::group_by(management_text) |>
        dplyr::summarise(year = dplyr::first(year),
                         management_pos = dplyr::first(management_pos),
                         upper = max(base),
                         lower = min(base),
                         .groups= 'drop') |>
        dplyr::mutate(upordown = ifelse(management_pos < lower, "down","up"))
    } else {
      if (mcmc_style == "hairy") {
        managementlabel_simp <- data |>
          dplyr::filter(med == "MCMC") |>
          dplyr::group_by(management_text) |>
          dplyr::summarise(year = dplyr::first(year),
                           management_pos = dplyr::first(management_pos),
                           upper = max(base),
                           lower = min(base),
                           .groups= 'drop') |>
          dplyr::mutate(upordown = ifelse(management_pos < lower, "down","up"))
      }

      if (mcmc_style == "banded" | mcmc_style == "boxplot") {
        managementlabel_simp <- data |>
          dplyr::filter(!is.na(interval)) |>
          dplyr::group_by(management_text) |>
          dplyr::summarise(year = dplyr::first(year),
                           management_pos = dplyr::first(management_pos),
                           upper = max(prob_upper),
                           lower = min(prob_lower),
                           .groups= 'drop') |>
          dplyr::mutate(upordown = ifelse(management_pos < lower, "down","up"))
      }
      if (mcmc_style == "CI") {
        managementlabel_simp <- data |>
          dplyr::left_join(dataCI, by = "year") |>
          dplyr::filter(!is.na(interval)) |>
          dplyr::group_by(management_text) |>
          dplyr::summarise(year = dplyr::first(year),
                           management_pos = dplyr::first(management_pos),
                           upper = max(upper),
                           lower = min(lower),
                           .groups= 'drop') |>
          dplyr::mutate(upordown = ifelse(management_pos < lower, "down","up"))
      }
    }
    # Vertical management action lines
    p <- p +
      ggplot2::geom_segment(data = managementlabel_simp,
                            ggplot2::aes(x=year, xend=year,
                                         y=ifelse(upordown=="down", upper, lower),
                                         yend=ifelse(upordown=="down",management_pos-0.01,management_pos+0.01)), colour="gray70")

    # Horizontal management action lines
    p <- p +
      ggplot2::geom_segment(data = managementlabel_simp,
                            ggplot2::aes(x=year-management_line_width,
                                         y=ifelse(upordown=="down",management_pos-0.01,management_pos+0.01),
                                         xend=year, yend=ifelse(upordown=="down",management_pos-0.01,management_pos+0.01)), colour="gray70")

    # Management action text
    p <- p +
      ggplot2::geom_text(data = managementlabel_simp,
                         ggplot2::aes(x=year-0.5,
                                      y=management_pos,
                                      label=management_text,
                                      hjust=1,
                                      vjust=ifelse(upordown=="down",0,1)),
                         size=3)
  }

  # Graph titles
  p <- p +
    ggplot2::annotate("text", x = xlim[1], y = title_position, label = paste(title), hjust=0, vjust=1, size=7) +
    ggplot2::annotate("text", x = xlim[1], y = subtitle_position, label = paste(subtitle), hjust=0, vjust=1, size=5.5)

  # Timestamp plot
  p <- p +
    ggplot2::annotate("text", x = xlim[2], y = ylim[1], label = paste0("Plot generated ",paste(Sys.Date(),sep="-")), hjust=1, vjust=0, size=3.5,  colour="gray70")

  # Legend and colours
  p <- p +
    ggplot2::scale_linetype_manual(name=legend_name, values="solid", labels=legend_entries[1]) +
    ggplot2::scale_colour_manual(name=legend_name, values = SSAND::fq_palette("DAF")[1], labels=legend_entries[1]) +
    ggplot2::scale_fill_manual(name="Guide2", values = SSAND::fq_palette("DAF")[1], labels=legend_entries[2]) +
    ggplot2::guides(linetype = ggplot2::guide_legend(order = 1), colour = ggplot2::guide_legend(order = 1), fill = ggplot2::guide_legend(order = 2, title=ggplot2::element_blank())) +
    ggplot2::theme(legend.box.background = ggplot2::element_rect(color = SSAND::fq_palette("DAF")[2],fill = "white", linewidth=0.8),legend.box.margin = ggplot2::margin(0.7,0.3,0.3,0.3,"cm")) +
    ggplot2::theme(legend.margin = ggplot2::margin(-0.5,0,0,0, unit="cm")) +
    ggplot2::theme(legend.position = legend_position)

  # Draft watermark
  if (!is.na(draft_text)) {
    p <- p +
      ggplot2::annotate("text", x = xlim[1]+10, y = 0.5, label = draft_text, colour = draft_colour, alpha=0.5, hjust=0, vjust=1, size=20)
  }

  # Plotting now complete. Save as temp file then overlay external images
  ggplot2::ggsave(p, height = 10, width = 16, filename = 'plot_without_logo.png')


  # ADD QG LOGO
  # read in raw images
  plot_without_logo <- magick::image_read("plot_without_logo.png")
  invisible(file.remove('plot_without_logo.png')) # Remove temp file
  logo_raw <- magick::image_read(system.file("fig/qg_logo.png", package = "SSAND"))

  # get dimensions of plot for scaling
  plot_height <- magick::image_info(plot_without_logo)$height
  plot_width <- magick::image_info(plot_without_logo)$width

  # default scale to 1/10th width of plot
  # Can change with logo_scale
  logo <- magick::image_scale(logo_raw, as.character(plot_width/logo_scale))

  # Get width of logo
  logo_width <- magick::image_info(logo)$width
  logo_height <- magick::image_info(logo)$height

  # Set position of logo
  # Position starts at 0,0 at top left
  # Using 0.01 for 1% - aesthetic padding
  x_pos = logo_padding * plot_width
  y_pos = plot_height - logo_height - logo_padding * plot_height

  # Compose the actual overlay
  p_logo <- magick::image_composite(plot_without_logo, logo, offset = paste0("+", x_pos, "+", y_pos))

  # INFOGRAPHIC
  if (!is.na(infographic_path)) {
    infographic_raw <- magick::image_read(infographic_path)
    infographic_width <- magick::image_info(infographic_raw)$width
    infographic_height <- magick::image_info(infographic_raw)$height

    if (missing(infographic_scale)) {infographic_scale <- (plot_width*0.25) / infographic_width}
    infographic_scaled <- magick::image_scale(infographic_raw, as.character(infographic_width*infographic_scale))

    p_logo <- magick::image_composite(p_logo, infographic_scaled, offset = paste0("+",infographic_position[1]*plot_width, "+",infographic_position[2]*plot_height))
  }

  # SAVE FINAL IMAGE
  if (file.exists(plotdir)) {
    invisible(file.remove(plotdir))
  }
  magick::image_write(p_logo, plotdir)

  print("Plot created and stored in the working directory. Please do not commit this image file to the repository.")
}



