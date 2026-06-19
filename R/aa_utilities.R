




# Data input warnings ----
check_data_columns <- function(data,variables) {
  for (i in 1:length(variables)) {
    if (!variables[i] %in% names(data)) {warning(paste0("Input data is missing '",variables[i],"' column"))}
  }
}




# Set up scenarios ----
apply_scenarios <- function(
    data,
    scenarios = NULL,
    scenario_labels = NULL,
    scenario_order = NULL) {

  if (!is.null(scenarios)) {
    data <- dplyr::filter(data, scenario %in% scenarios)
  }

  if (is.null(scenario_labels)) {
    data <- dplyr::mutate(data, scenario_labels = as.factor(paste0("Scenario ", scenario)))
  } else {
    lookup <- data.frame(
      scenario = unique(data$scenario),
      scenario_labels = scenario_labels
    )
    data <- data |>
      dplyr::left_join(lookup, by = "scenario") |>
      dplyr::mutate(scenario_labels = as.factor(scenario_labels))
  }

  if (!is.null(scenario_order)) {
    scenario_order <- c(scenario_order, setdiff(levels(data$scenario_labels), scenario_order))
    data$scenario_labels <- factor(data$scenario_labels, levels = scenario_order)
  }

  data
}


# Set up fleets ----
apply_fleet_names <- function(
    data,
    fleet_names = NULL) {

  if(!is.null(fleet_names)) {

    if (length(fleet_names) != length(unique(data$fleet))) {
      warning("The number of fleet names provided does not match the number of unique fleets in the data.")
    }

    fleet_names.lookup <- data.frame(fleet = unique(data$fleet), fleet_names = fleet_names)
    data <- data |>
      dplyr::left_join(fleet_names.lookup, by = "fleet") |>
      dplyr::select(-fleet) |>
      dplyr::rename(fleet = fleet_names)
  }

  data
}


# Set up x axis ----
build_x_axis <- function(
    x,
    xlab = NULL,
    xlim = NULL,
    xbreaks = NULL,
    xlabels = NULL,
    financial_year = FALSE,
    show_dates_on_axis = FALSE,
    expand_upper = 0,
    xangle = NULL,
    is_date = inherits(x, "Date")
) {
  # if (missing(xbreaks) & xlim[1]!=xlim[2]) {xbreaks <- unique(floor(pretty(xlim)))} # unique(floor()) ensures integers only

  if(financial_year & xlab == "Year") {
    warning("Your x-axis implies calendar year, but you've indicated you're using financial year.")
  }

  if (is.null(xlim)) {
    xlim <- c(min(x, na.rm = TRUE), max(x, na.rm = TRUE) + expand_upper)
  }

  if (is.null(xbreaks)) {
    xbreaks <- pretty(xlim)
  }

  if (is.null(xlabels)) {
    if (is_date) {
      if (financial_year && !show_dates_on_axis) {
        xlabels <- paste0(lubridate::year(xbreaks) - 1, "\u2013", lubridate::year(xbreaks))
      } else if (!financial_year && !show_dates_on_axis) {
        xlabels <- lubridate::year(xbreaks)
      } else {
        xlabels <- xbreaks
      }
    } else {
      if (financial_year) {
        xlabels <- paste0(xbreaks - 1, "\u2013", xbreaks)
      } else {
        xlabels <- xbreaks
      }
    }
  }

  if (is.null(xangle)) {
    xangle <- ifelse(financial_year, 90, 0)
  }

  list(
    xlab = xlab,
    limits = xlim,
    breaks = xbreaks,
    labels = xlabels,
    angle = xangle
  )
}

# Set up y axis ----
build_y_axis <- function(
    y,
    ylab = NULL,
    ylim = NULL,
    ybreaks = NULL,
    ylabels = NULL,
    lower = 0,
    upper = NULL) {

  if (is.null(ylim)) {
    if (is.null(upper)) {
      ylim <- c(lower, max(y, na.rm = TRUE))
    } else {
      ylim <- c(lower,max(max(y,na.rm = T),max(upper,na.rm = T)))
    }
  }

  if (is.null(ybreaks)) ybreaks <- pretty(ylim)
  if (is.null(ylabels)) ylabels <- ybreaks

  list(
    ylab = ylab,
    limits = ylim,
    breaks = ybreaks,
    labels = ylabels
  )
}


# Add axes ----
add_x_scale_date <- function(p, axis) {
  p +
    ggplot2::scale_x_date(
      name   = axis$xlab,
      limits = axis$limits,
      breaks = axis$breaks,
      labels = axis$labels
    )
}

add_x_scale_continuous <- function(p, axis) {
  p +
    ggplot2::scale_x_continuous(
      name   = axis$xlab,
      limits = axis$limits,
      breaks = axis$breaks,
      labels = axis$labels
    )
}

add_y_scale_continuous <- function(p,
                                   axis) {
  p +
    ggplot2::scale_y_continuous(
      name   = axis$ylab,
      limits = axis$limits,
      breaks = axis$breaks,
      labels = axis$labels
    )
  # ggplot2::coord_cartesian(ylim = c(-0, NA), xlim = c(0,NA)) # alternative to ylim that doesn't cut off ribbons

}


# Set up facets ----
add_scenario_facets <- function(p,
                                data,
                                scales = "fixed",
                                ncol = 2) {

  if (length(unique(data$scenario)) > 1) {
    p + ggplot2::facet_wrap(~scenario_labels, scales = scales, ncol = ncol)
  } else {
    p
  }
}


# MCMC ----
simplify_show_median <- function(selection, options) {
  # Check for invalid entries
  invalid <- setdiff(selection, options)
  if (length(invalid) > 0) {
    warning(
      paste(
        "Invalid show_median option(s):",
        paste(invalid, collapse = ", "),
        "\nValid options are:",
        paste(options, collapse = ", ")
      )
    )
  }
  # Simplify annual_* to annual
  selection[grepl("^annual_", selection)] <- "annual"
  selection[grepl("^median_", selection)] <- "annual"
  return(selection)
}


check_mcmc_style <- function(mcmc_style) {
  if (length(mcmc_style)>1) {warning("You can only select one mcmc_type at a time.")}
}

sample_mcmc_runs <- function(data,
                             sample = NULL) {

  if (is.null(sample)) return(data)
  data2 <- data |> dplyr::filter(med == "trajectory")
  data3 <- data |>
    dplyr::filter(med == "MCMC") |>
    dplyr::filter(rownum %in% sample(unique(data$rownum), size = sample))

  dplyr::bind_rows(data2, data3)
}


# MCMC timeseries ----
mcmc_boxplot <- function(p, data, xlim, boxplot_outliers) {
  databox <- data |>
    dplyr::filter(rownum > 0)

  # Expand limits of x-axis to include box
  xlim[1] <- xlim[1]-0.5
  xlim[2] <- xlim[2]+0.5

  p +
    ggplot2::geom_boxplot(data = databox,
                          ggplot2::aes(x=xvar, y=value, group=xvar),
                          outliers = boxplot_outliers)
}


mcmc_banded <- function(p, data, alpha, band_labels, band_colour){

  tmp <- unique(data$interval)[!is.na(unique(data$interval))]

  if (is.null(alpha)) {
    alpha_scale <- seq(round(1/length(tmp),2),1,round(1/length(tmp),2))^2 + 0.1
    alpha_scale <- alpha_scale/max(alpha_scale)
  } else {
    if (length(alpha) != length(unique(stats::na.omit(data$interval)))) {
      stop("The number of alpha values provided does not match the number of credible intervals to plot.")
    }
    alpha_scale <- alpha
  }

  if (is.null(band_labels)) {band_labels <- rev(unique(data$interval)[!is.na(unique(data$interval))])}

  p +
    ggplot2::geom_ribbon(data = data |> dplyr::filter(!is.na(interval)),
                         ggplot2::aes(x=xvar, ymin=lower, ymax=upper, group=interval, alpha=as.factor(-interval)),
                         fill=band_colour) +
    ggplot2::scale_alpha_manual(values = alpha_scale,
                                labels = band_labels,
                                name = "Credible interval")
}


mcmc_hairy <- function(p, data, hair_width) {
  p +
    ggplot2::geom_line(data = data |> dplyr::filter(med == "MCMC"),
                       ggplot2::aes(x=xvar,y=value, group=rownum),
                       colour = 'grey20',
                       linewidth=hair_width,
                       alpha = 1)
}


mcmc_CI <- function(p, data, aggregate_scenarios, CI_range, alpha) {
  if (aggregate_scenarios) {
    dataCI <- data |>
      dplyr::filter(med=="MCMC") |>
      dplyr::group_by(xvar) |>
      dplyr::summarise(upper = quantile(value,probs=1-(1-CI_range)/2),
                       lower = quantile(value,probs=(1-CI_range)/2),
                       .groups = 'drop')
  } else {
    dataCI <- data |>
      dplyr::filter(med=="MCMC") |>
      dplyr::group_by(scenario_labels,xvar) |>
      dplyr::summarise(upper = quantile(value,probs=1-(1-CI_range)/2),
                       lower = quantile(value,probs=(1-CI_range)/2),
                       .groups = 'drop')
  }

  p <- p +
    ggplot2::geom_ribbon(data = dataCI, ggplot2::aes(x=xvar, ymax=upper, ymin = lower), fill = "grey60", alpha = alpha)
}

show_median_lines <- function(label,p,data,show_median,line_width,colours) {
  if (!"none" %in% show_median) {
    data_med <- data |>
      dplyr::filter(med %in% show_median) |>
      dplyr::mutate(med = dplyr::recode(med,
                                        "annual" = paste0("Median ",label),
                                        "trajectory" = "Median trajectory"))

    p +
      ggplot2::geom_line(data=data_med, ggplot2::aes(x=xvar,y=value, colour=med), linewidth=line_width) +
      ggplot2::scale_color_manual(values = colours, name = ggplot2::element_blank())
  }
}




show_final_biomass <- function(p, data, MCMC, colour_categories,scenario_labels) {
  if (MCMC) {
    plot_dat <- data |>
      dplyr::group_by(scenario_labels) |>
      dplyr::summarise(xvar = max(xvar,na.rm = TRUE)) |>
      dplyr::left_join(data |> dplyr::filter(med == "annual"), by = c("scenario_labels", "xvar"))
  } else {
    plot_dat <- data
  }

  p +
    ggrepel::geom_text_repel(
      data = subset(data, year == max(data$xvar)),
      ggplot2::aes(
        x = xvar,
        y = value,
        label = paste0(round(subset(data, xvar == max(data$xvar))$value,2)*100,"%"),
        colour= subset(data, xvar == max(data$xvar))$colour_categories),
      size = 4,
      nudge_x = .5,
      nudge_y = 0.1,
      segment.color = '#cccccc',
      segment.size = 0.5,
      show.legend  = FALSE,
      max.overlaps = Inf)

}



add_reference_line <- function(p, data, yvalue, colour, annotation_position, label) {
  p +
    ggplot2::geom_hline(yintercept = yvalue, color=colour, linetype="solid",alpha=0.5) +
    ggplot2::geom_text(data=data,x = annotation_position, y = yvalue+0.02, color=colour, label = label, size=3, hjust = 0)
}










mcmc_joy <- function(p, data, CI_range, ridge_colour, rel_min_height, alpha, ridge_scale, show_CI,
                     ybreaks, ylin,ylab, xlab, legend_position, text_size,xbreaks,legend_box,facet_wrap,
                     show_median,xlabels,ylabels) {

  # Joy plot
  # Joy plot is different to the others as years need to be factors, and the x and y axes are swapped.
  # There might be a more clever way to do this that we'll think of one day 🤷️
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

  if (show_CI) {
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

  p
}


# Bits I could probably tidy further:

# Catch plot:
# if(show_annual_aggregate){
#   data <- data |>
#     dplyr::mutate(year = as.numeric(format(date, "%Y"))) |>
#     dplyr::group_by(year, fleet) |>
#     dplyr::summarise(value = sum(value), .groups = 'drop') |>
#     dplyr::mutate(date = as.Date(paste0('01/01/', year), format = '%d/%m/%Y'))
# }

# # If xlim is entered as just years, convert to dates
# if(!is.null(xlim)) {
#   if(nchar(xlim[1]) == 4) {
#     xlim <- c(
#       as.Date(paste0(xlim[1], "-01-01"), format = "%Y-%m-%d"),
#       as.Date(paste0(xlim[2], "-01-01"), format = "%Y-%m-%d"))
#   }
# }



# Biomass plot:
# # Add median lines
# if (!"none" %in% show_median) {
#   data_med <- data |>
#     dplyr::filter(med %in% show_median) |>
#     dplyr::mutate(med = dplyr::recode(med,
#                                       "annual_biomass" = "Median annual biomass",
#                                       "trajectory" = "Median trajectory"))
#
#   p <- p +
#     ggplot2::geom_line(data=data_med, ggplot2::aes(x=year,y=value, colour=med), linewidth=line_width) +
#     ggplot2::scale_color_manual(values = colours, name = ggplot2::element_blank())
# }


# CPUE
# if (!is.null(fleets)) {data <- data |> dplyr::filter(fleet %in% fleets)}
# if (is.null(fleets)) {fleets <- sort(unique(data$fleet))}
# if (is.null(fleet_names)) {fleet_names <- paste0('Fleet ',sort(unique(data$fleet)))}
