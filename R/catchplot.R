# Copyright 2026 Fisheries Queensland

# This file is part of SSAND.
# SSAND is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
# SSAND is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
# You should have received a copy of the GNU General Public License along with SSAND. If not, see <https://www.gnu.org/licenses/>.

#' Catch plot
#'
#' @param data Output from catchplot_prep(). A dataframe with date (date), value (int), fleet (fac), scenario (int)
#' @param xlab Label for x-axis (character). Default is "Year".
#' @param ylab Label for y-axis (character). Default is "Retained catch (t)" for DDUST models. Default is determined by catchplot_prep_SS partition setting for SS models.
#' @param fleet_names A vector of customised fleet names for legend (character).
#' @param xbreaks A vector of breaks between x-axis labels, used in ggplot2::scale_x_date() (numeric).
#' @param ybreaks A vector of breaks between y-axis labels, used in ggplot2::scale_y_contiunous() (numeric).
#' @param xlabels A vector of labels for the x-axis breaks.
#' @param ylabels A vector of labels for the y-axis breaks.
#' @param xlim A vector of lower and upper x-axis limits (e.g. c(1950, 2020)) (numeric).
#' @param ylim A vector of lower and upper y-axis limits (e.g. c(0,1)) (numeric).
#' @param colours A vector of colours used (character).
#' @param reverse Set to TRUE to reverse the default stacking order (logical). This is useful if you're rotating both the plot and legend.
#' @param strip_position Customise ggplot2::facet_wrap() strips. By default, the labels are displayed on the top of the plot. Using strip_position it is possible to place the labels on either of the four sides by setting strip.position = c("top", "bottom", "left", "right").
#' @param show_annual_aggregate Set to TRUE to aggregate annually (logical). Default is FALSE.
#' @param scenarios A vector of scenarios to plot (numeric). Shows all scenarios if left blank. Can be overridden in the plotting function.
#' @param scenario_labels A vector of customised scenario names (character). Default is "Scenario 1", "Scenario 2", etc.
#' @param scenario_order A vector to reorder how scenarios are displayed (character). Use the label names defined in "scenario_labels".
#' If "scenario_labels" is left blank, the labels will be "Scenario 1", "Scenario 2" etc.
#' Any scenarios not included in "scenario_order" will be tacked on in the order they appear in the input data.
#' @param scales Scales for ggplot2::facet_wrap(). Default is 'free', see ?ggplot2::facet_wrap for options.
#' @param ncol Number of columns for facet_wrap(). Default is 2.
#' @param financial_year Set to TRUE if the assessment was based on financial year (logical). Adjusts the x-axis to show full financial year notation.
#' @param show_dates_on_axis Set to TRUE to show full dates on x-axis as opposed to years.
#' @param text_size,legend_text_size,text_colour,legend_text_colour,legend_position,legend_box,legend_title_blank,panel_border,panel_border_colour,xangle Optional plotting theme overrides. Defaults are controlled by `theme_ssand()`
#'   and can be set globally via `set_ssand_style()`.
#'
#' @return Catch plot
#' @export
#'
#' @examples
#' data <- catchplot_prep_SS(ss_mle)
#' catchplot(data, fleet_names = c("Commercial","Recreational"))
#' catchplot(data, financial_year = TRUE, xlab = "Financial year")
#'
#' data <- catchplot_prep_DD(dd_mle)
#' catchplot(data)
catchplot <- function(data,
                      xlab = "Year",
                      ylab = NULL,
                      xbreaks = NULL,
                      ybreaks = NULL,
                      xlabels = NULL,
                      ylabels = NULL,
                      xlim = NULL,
                      ylim = NULL,
                      fleet_names = NULL,
                      colours = c("grey70", fq_palette("alisecolours")[1:9]),
                      reverse = FALSE,
                      strip_position = NA,
                      show_annual_aggregate = FALSE,
                      show_dates_on_axis = FALSE,
                      scenarios = NULL,
                      scenario_labels = NULL,
                      scenario_order = NULL,
                      scales = 'free',
                      ncol = 2,

                      xangle = NULL,
                      legend_position = NULL,
                      financial_year = FALSE,
                      text_size = NULL,
                      legend_text_size = NULL,
                      text_colour = NULL,
                      legend_text_colour = NULL,
                      legend_box = NULL,
                      legend_title_blank = NULL,
                      panel_border = NULL,
                      panel_border_colour = NULL
                      ) {

  # ___________________
  # Data validation
  # ___________________
  check_data_columns(data,c("date","value","fleet","scenario"))
  if (!lubridate::is.Date(data$date)) stop("Please ensure date column is of date format")

  data$xvar <- data$date
  data$year <- lubridate::year(data$xvar)

  # ___________________
  # Custom to this plot
  # ___________________
  if (is.null(ylab)) {
    if ("partition" %in% names(data)) {
      if (data$partition[1] == "sel")    ylab <- "Catch (retained and total discarded) (t)"
      if (data$partition[1] == "retain") ylab <- "Retained catch (t)"
      if (data$partition[1] == "dead")   ylab <- "Dead catch (t)"
    } else {
      ylab <- "Retained catch (t)"
    }
  }

  if(show_annual_aggregate){
    data <- data |>
      dplyr::mutate(year = as.numeric(format(xvar, "%Y"))) |>
      dplyr::group_by(year, fleet) |>
      dplyr::summarise(value = sum(value), .groups = 'drop') |>
      dplyr::mutate(xvar = as.Date(paste0('01/01/', year), format = '%d/%m/%Y'))
  }

  # If xlim is entered as years, convert the whole vector to Date.
  # Important: convert the whole vector at once so the Date class is preserved.
  if (is.null(xlim)) {
    xlim <- as.Date(
      paste0(
        c(
          min(data$year, na.rm = TRUE) - 1,
          max(data$year, na.rm = TRUE) + 1
        ),
        "-01-01"
      )
    )
  } else {
    xlim <- coerce_year_axis_to_date(xlim, arg = "xlim")
  }

  # If xbreaks are entered as years, convert them too.
  xbreaks <- coerce_year_axis_to_date(xbreaks, arg = "xbreaks")

  if (is.null(ylim)) {
    ylim[1] <- 0
    ylim[2] <- data |>
      dplyr::group_by(xvar,scenario) |>
      dplyr::summarise(value = sum(value), .groups='drop') |>
      dplyr::summarise(value = max(value)) |>
      dplyr::pull(value)
  }

  # ___________________
  # Basic plot set up
  # ___________________
  data <- apply_scenarios(data, scenarios, scenario_labels, scenario_order)
  data <- apply_fleet_names(data, fleet_names)

  p <- ggplot2::ggplot(data) +
    ggplot2::geom_bar(data,
                      mapping  = ggplot2::aes(x = xvar, y = value, fill = as.factor(fleet)),
                      position = ggplot2::position_stack(reverse = reverse),
                      stat     = 'identity') +
    ggplot2::scale_fill_manual(values = colours) +
    ggplot2::scale_colour_manual(values = "#3d4040")

  p <- add_scenario_facets(p, data, scales = scales, ncol = ncol)

  # ___________________
  # Axes and theme
  # ___________________
  x_axis <- build_x_axis(x = data$xvar,
                         xlab = xlab,
                         xlim = xlim,
                         xbreaks = xbreaks,
                         xlabels = xlabels,
                         financial_year = financial_year,
                         show_dates_on_axis = show_dates_on_axis,
                         expand_lower = 0,
                         expand_upper = 0,
                         xangle = xangle,
                         is_date = TRUE)

  y_axis <- build_y_axis(y = data$value,
                         ylab = ylab,
                         ylim = ylim,
                         ybreaks = ybreaks,
                         ylabels = ylabels,
                         lower = 0)

  p <- add_x_scale_date(p, x_axis)
  p <- add_y_scale_continuous(p, y_axis)

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
