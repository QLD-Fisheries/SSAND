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
#' @param text_size Text size (num). Default is 12.
#' @param scenarios A vector of scenario numbers to be shown on plot (numeric). This was already specified in prep file, but this is a manual override to save running the prep function again.
#' @param scenario_labels A vector of customised scenario names (character). Default is "Scenario 1", "Scenario 2", etc.
#' @param scenario_order A vector to reorder how scenarios are displayed (character). Use the label names defined in "scenario_labels".
#' If "scenario_labels" is left blank, the labels will be "Scenario 1", "Scenario 2" etc.
#' Any scenarios not included in "scenario_order" will be tacked on in the order they appear in the input data.
#' @param colours A vector of colours used (character).
#' @param scales Scales for ggplot2::facet_wrap(). Default is 'free', see ?ggplot2::facet_wrap for options.
#' @param ncol Number of columns for facet_wrap(). Default is 2.
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
                         text_size = 12,
                         scenarios = NULL,
                         scenario_labels = NULL,
                         scenario_order = NULL,
                         colours = c("grey70",fq_palette("alisecolours")),
                         scales = 'free',
                         ncol = 2) {

  # ___________________
  # Data validation
  # ___________________
  # Data input warnings
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


  p <- ggplot2::ggplot(data) + get_theme_ssand()
  p <- add_x_scale_continuous(p, x_axis)
  p <- add_y_scale_continuous(p, y_axis)

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

  return(p)
}
