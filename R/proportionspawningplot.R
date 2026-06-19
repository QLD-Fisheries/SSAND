# Copyright 2024 Fisheries Queensland

# This file is part of SSAND.
# SSAND is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
# SSAND is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
# You should have received a copy of the GNU General Public License along with SSAND. If not, see <https://www.gnu.org/licenses/>.

#' Proportion spawning plot
#'
#' @param data Output from proportionspawningplot_prep() with columns month (fac), value (num), scenario (fac)
#' @param scenarios A vector of scenario numbers to be shown on plot (numeric). This was already specified in prep file, but this is a manual override to save running the prep function again.
#' @param scenario_labels A vector of customised scenario names (character). Default is "Scenario 1", "Scenario 2", etc.
#' @param scenario_order A vector to reorder how scenarios are displayed (character). Use the label names defined in "scenario_labels".
#' If "scenario_labels" is left blank, the labels will be "Scenario 1", "Scenario 2" etc.
#' Any scenarios not included in "scenario_order" will be tacked on in the order they appear in the input data.
#' @param scales Scales for ggplot2::facet_wrap(). Default is 'free', see ?ggplot2::facet_wrap for options.
#' @param ncol Number of columns for facet_wrap(). Default is .
#' @param xlab Label for x-axis (character). Default is "Month".
#' @param ylab Label for y-axis (character). Default is "Proportion spawning".
#' @param ylim A vector of lower and upper y-axis limits (e.g. c(0,1)) (numeric).
#' @param ylabels A vector of labels for the y-axis breaks.
#' @param ybreaks A vector of breaks between y-axis labels, used in ggplot2::scale_y_continous() (numeric).
#'
#' @return Proportion spawning plot
#' @export
#'
#' @examples
#' data <- proportionspawningplot_prep_DD(dd_mle)
#' proportionspawningplot(data)
proportionspawningplot <- function(data,
                                   scenarios = NULL,
                                   scenario_labels = NULL,
                                   scenario_order = NULL,
                                   ylim = NULL,
                                   ylabels = NULL,
                                   ybreaks = NULL,
                                   scales = 'free',
                                   ncol = 2,
                                   xlab = "Month",
                                   ylab = "Proportion spawning"){

  # ___________________
  # Data validation
  # ___________________

  check_data_columns(data, c("month","value","scenario"))

  # ___________________
  # Basic plot set up
  # ___________________

  if (is.null(ylim)) {ylim <- c(0,max(data$value))}
  if (is.null(ybreaks)) {ybreaks <- pretty(ylim)}
  if (is.null(ylabels)) {ylabels <- ybreaks}

  data <- apply_scenarios(data, scenarios, scenario_labels, scenario_order)


  p <- ggplot2::ggplot(data) +
    ggplot2::geom_point(ggplot2::aes(x=month,y=value),size=2) +
    get_theme_ssand() +
    ggplot2::xlab(xlab) +
    ggplot2::ylab(ylab)

  # ___________________
  # Final layers
  # ___________________

  p <- add_scenario_facets(p, data, scales = scales, ncol = ncol)

  return(p)
}
