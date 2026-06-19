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
#' @param xbreaks A vector of breaks between x-axis labels, used in ggplot2::scale_x_continous() (numeric).
#' @param ybreaks A vector of breaks between y-axis labels, used in ggplot2::scale_y_continous() (numeric).
#' @param xlabels A vector of labels for the x-axis breaks.
#' @param ylabels A vector of labels for the y-axis breaks.
#' @param xlim A vector of lower and upper x-axis limits (e.g. c(1950, 2020)) (numeric).
#' @param ylim A vector of lower and upper y-axis limits (e.g. c(0,1)) (numeric).
#' @param xangle Set to 90 to rotate x-axis labels 90 degrees.
#' @param colours A vector of colours used (character).
#' @param legend_position Position of the legend ("none", "left", "right", "bottom", "top", or two-element numeric vector for x and y position). Default is "top".
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
#'
#' @return Catch plot
#' @export
#'
#' @examples
#' data <- catchplot_prep_SS(ss_mle)
#' catchplot(data, fleet_names = "Commercial")
#' catchplot(data, fleet_names = "Commercial", financial_year = TRUE, xlab = "Financial year")
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
                      xangle = NULL,
                      financial_year = FALSE,
                      fleet_names = NULL,
                      colours = c("grey70", fq_palette("alisecolours")[1:9]),
                      legend_position = "top",
                      reverse = FALSE,
                      strip_position = NA,
                      show_annual_aggregate = FALSE,
                      show_dates_on_axis = FALSE,
                      scenarios = NULL,
                      scenario_labels = NULL,
                      scenario_order = NULL,
                      scales = 'free',
                      ncol = 2) {

  # ___________________
  # Data validation
  # ___________________

  check_data_columns(data,c("date","value","fleet","scenario"))
  data$xvar <- data$date


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
      dplyr::mutate(year = as.numeric(format(date, "%Y"))) |>
      dplyr::group_by(year, fleet) |>
      dplyr::summarise(value = sum(value), .groups = 'drop') |>
      dplyr::mutate(date = as.Date(paste0('01/01/', year), format = '%d/%m/%Y'))
  }

  # If xlim is entered as just years, convert to dates
  if(!is.null(xlim)) {
    if(nchar(xlim[1]) == 4) {
      xlim <- c(
        as.Date(paste0(xlim[1], "-01-01"), format = "%Y-%m-%d"),
        as.Date(paste0(xlim[2], "-01-01"), format = "%Y-%m-%d"))
    }
  }

  # ___________________
  # Basic plot set up
  # ___________________

  data <- apply_scenarios(data, scenarios, scenario_labels, scenario_order)

  data <- apply_fleet_names(data, fleet_names)


  x_axis <- build_x_axis(x = data$xvar,
                         xlab = xlab,
                         xlim = xlim,
                         xbreaks = xbreaks,
                         xlabels = xlabels,
                         financial_year = financial_year,
                         show_dates_on_axis = show_dates_on_axis,
                         expand_upper = 1,
                         xangle = xangle,
                         is_date = TRUE
  )

  y_axis <- build_y_axis(y = data$value,
                         ylab = ylab,
                         ylim = ylim,
                         ybreaks = ybreaks,
                         ylabels = ylabels,
                         lower = 0
  )

  p <- ggplot2::ggplot(data) +
    get_theme_ssand() +
    ggplot2::geom_bar(data,
                      mapping  = ggplot2::aes(x = date, y = value, fill = as.factor(fleet)),
                      position = ggplot2::position_stack(reverse = reverse),
                      stat     = 'identity'
    ) +
    ggplot2::scale_fill_manual(values = colours) +
    ggplot2::scale_colour_manual(values = "#3d4040")

  p <- add_x_scale_continuous(p, x_axis)  # or date version depending on representation
  p <- add_y_scale_continuous(p, y_axis)
  p <- add_scenario_facets(p, data, scales = scales, ncol = ncol)

  # ___________________
  # Final layers
  # ___________________

  return(p)
}
