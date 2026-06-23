# Copyright 2024 Fisheries Queensland

# This file is part of SSAND.
# SSAND is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
# SSAND is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
# You should have received a copy of the GNU General Public License along with SSAND. If not, see <https://www.gnu.org/licenses/>.

#' Maturity plot
#'
#' @param data A data frame with variables value (num), maturity (num), sex (int), scenario (int), type (chr)
#' @param maturity_type To define the x-axis. Use "length1" or "age1", depending on what you'd like on the x-axis, if you modelled length-based maturity. Use "length2" or "age2", depending on what you'd like on the x-axis, if you modelled age-based maturity.
#' @param xlab Label for x-axis (character). Default is "Age".
#' @param ylab Label for y-axis (character). Default is "Carapace length (cm, beginning of year)".
#' @param xbreaks A vector of breaks between x-axis labels, used in ggplot2::scale_x_continous() (numeric).
#' @param ybreaks A vector of breaks between y-axis labels, used in ggplot2::scale_y_continous() (numeric).
#' @param xlabels A vector of labels for the x-axis breaks.
#' @param ylabels A vector of labels for the y-axis breaks.
#' @param xlim A vector of lower and upper x-axis limits (e.g. c(1950, 2020)) (numeric).
#' @param ylim A vector of lower and upper y-axis limits (e.g. c(0,1)) (numeric).
#' @param scenarios A vector of scenario numbers to be shown on plot (numeric). This was already specified in prep file, but this is a manual override to save running the prep function again.
#' @param scenario_labels A vector of customised scenario names (character). Default is "Scenario 1", "Scenario 2", etc.
#' @param scenario_order A vector to reorder how scenarios are displayed (character). Use the label names defined in "scenario_labels".
#' If "scenario_labels" is left blank, the labels will be "Scenario 1", "Scenario 2" etc.
#' Any scenarios not included in "scenario_order" will be tacked on in the order they appear in the input data.
#' @param colours A vector of colours used (character).
#' @param scales Scales for ggplot2::facet_wrap(). Default is 'free', see ?ggplot2::facet_wrap for options.
#' @param ncol Number of columns for facet_wrap(). Default is 2.
#' @param financial_year Set to TRUE if the assessment was based on financial year (logical). Adjusts the x-axis to show full financial year notation.
#' @param text_size,legend_text_size,text_colour,legend_text_colour,legend_position,legend_box,legend_title_blank,panel_border,panel_border_colour,xangle Optional plotting theme overrides. Defaults are controlled by `theme_ssand()`
#'   and can be set globally via `set_ssand_style()`.
#'
#' @return Maturity plot
#' @export
#'
#' @examples
#' data <- maturityplot_prep_SS(ss_mle)
#' maturityplot(data)
#'
#' data <- maturityplot_prep_DD(x_max=10,x_mat=2)
#' maturityplot(data)
maturityplot <- function(data,
                         maturity_type = "length1",
                         xlab = NULL,
                         ylab = "Maturity",
                         xbreaks = NULL,
                         ybreaks = NULL,
                         xlabels = NULL,
                         ylabels = NULL,
                         xlim = NULL,
                         ylim = NULL,
                         scenarios = NULL,
                         scenario_labels = NULL,
                         scenario_order = NULL,
                         colours = c("grey70",fq_palette("alisecolours")),
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
                         panel_border_colour = NULL) {

  # ___________________
  # Data validation
  # ___________________
  check_data_columns(data, c("value","maturity","sex","scenario","type"))
  data$xvar <- data$value

  # ___________________
  # Custom to this plot
  # ___________________
  data <- data |>
    dplyr::filter(type %in% maturity_type) |>
    dplyr::mutate(sex = dplyr::recode(sex, "1" = "Female" ,  "2" = "Male"))

  if (missing(xlab)) {xlab <- ifelse(maturity_type %in% c("length1","length2"),"Length (cm)", "Age (years)")}

  # ___________________
  # Basic plot set up
  # ___________________
  data <- apply_scenarios(data, scenarios, scenario_labels, scenario_order)

  p <- ggplot2::ggplot(data)

  # ___________________
  # Build MLE plot
  # ___________________
  p <- p +
    ggplot2::geom_line(ggplot2::aes(x=value, y=maturity, colour=sex)) +
    ggplot2::geom_point(ggplot2::aes(x=value, y=maturity, colour=sex)) +
    ggplot2::scale_colour_manual(values = colours)

  # ___________________
  # Final layers
  # ___________________
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
                         expand_upper = as.numeric(show_final_biomass),
                         xangle = xangle,
                         is_date = FALSE)

  y_axis <- build_y_axis(y = data$maturity,
                         ylab = ylab,
                         ylim = ylim,
                         ybreaks = ybreaks,
                         ylabels = ylabels,
                         lower = 0,
                         upper = 1)

  p <- add_x_scale_continuous(p, x_axis)
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
