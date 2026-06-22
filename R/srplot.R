# Copyright 2024 Fisheries Queensland

# This file is part of SSAND.
# SSAND is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
# SSAND is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
# You should have received a copy of the GNU General Public License along with SSAND. If not, see <https://www.gnu.org/licenses/>.

#' Stock-recruit curve plot
#'
#' @param data Output from srplot_prep() with columns year (int), spawn_bio (num), pred_recr (num), exp_recr (num), dev (num), bias_adjusted (num), scenario (int)
#' @param xlab Label for x-axis (character). Default is "Spawning output (relative to B0)".
#' @param ylab Label for y-axis (character). Default is "Recruitment (relative to R0)".
#' @param xbreaks A vector of breaks between x-axis labels, used in ggplot2::scale_x_continous() (numeric).
#' @param show_bias_adjust Set to TRUE to include bias adjusted recruitment (set to FALSE for DDUST or REDDUST) (logical).
#' @param scenarios A vector of scenario numbers to be shown on plot (numeric). This was already specified in prep file, but this is a manual override to save running the prep function again.
#' @param scenario_labels A vector of customised scenario names (character). Default is "Scenario 1", "Scenario 2", etc.
#' @param scenario_order A vector to reorder how scenarios are displayed (character). Use the label names defined in "scenario_labels".
#' If "scenario_labels" is left blank, the labels will be "Scenario 1", "Scenario 2" etc.
#' Any scenarios not included in "scenario_order" will be tacked on in the order they appear in the input data.
#' @param scales Scales for ggplot2::facet_wrap(). Default is 'free', see ?ggplot2::facet_wrap for options.
#' @param ncol Number of columns for facet_wrap(). Default is 2.
#' @param text_size,legend_text_size,text_colour,legend_text_colour,legend_position,legend_box,legend_title_blank,panel_border,panel_border_colour,xangle Optional plotting theme overrides. Defaults are controlled by `theme_ssand()`
#'   and can be set globally via `set_ssand_style()`.
#'
#' @return Plot of S-R curve
#' @export
#'
#' @examples
#' data <- srplot_prep_SS(ss_mle)
#' srplot(data)
#'
#' data <- srplot_prep_DD(dd_mle)
#' srplot(data)
srplot <- function(data,
                   xlab = expression(paste("Spawning output (relative to ", italic(B)[0], ")")),
                   ylab = expression(paste("Recruitment (relative to ", italic(R)[0], ")")),
                   xbreaks = NULL,
                   show_bias_adjust=FALSE,
                   scenarios = NULL,
                   scenario_labels = NULL,
                   scenario_order = NULL,
                   scales = 'free',
                   ncol = 2,
                   xangle = NULL,
                   legend_position = NULL,
                   text_size = NULL,
                   legend_text_size = NULL,
                   text_colour = NULL,
                   legend_text_colour = NULL,
                   legend_box = NULL,
                   legend_title_blank = NULL,
                   panel_border = NULL,
                   panel_border_colour = NULL){

  # Data input warnings
  check_data_columns(data, c("year","spawn_bio","pred_recr","exp_recr","dev","bias_adjusted","scenario"))

  if (missing(xbreaks)) {xbreaks <- seq(0,1,0.2)}

  data <- apply_scenarios(data, scenarios, scenario_labels, scenario_order)

  data2 = data[order(data$spawn_bio),]
  data$year = as.numeric(data$year)

  # for year labels
  data[is.na(data$dev), "dev"]<- 0 # avoid NA's that cause facetting issues.

  textmindev = 0.5
  show <- abs(data$dev) > textmindev | data$year == max(data$year) | data$year == min(data$year)

  year_breaks <-  seq(round(min(data$year),-1), round(max(data$year),-1), 10)

  p <- ggplot2::ggplot(data, ggplot2::aes(x = spawn_bio, y = pred_recr, colour = year)) +
    ggplot2::scale_colour_manual(c("", ""), values = c("black", "gray50"), labels = c("Expected", "Bias adjusted"))+
    ggplot2::scale_linetype_manual(c("", ""), values = c("solid", "dashed"), labels = c("Expected", "Bias adjusted"))+
    ggplot2::geom_point(data = data, ggplot2::aes(x = spawn_bio, y = pred_recr, fill = year), shape = 21, colour = "gray20", size = 3, alpha = 0.7)+
    ggplot2::geom_line(data = data2, ggplot2::aes(x = spawn_bio, y = exp_recr, linetype = "A", colour = "A")) +
    ggplot2::geom_text(data = data[show,],
                       ggplot2::aes(x = spawn_bio, y = pred_recr, label = year),
                       check_overlap = TRUE,
                       position = ggplot2::position_nudge(y = 0.05, x = 0),
                       size = 3,
                       colour = "black")+
    ggplot2::scale_fill_gradientn("Year", colours = c("#9E0142","#D53E4F","#F46D43","#FDAE61","#FEE08B","#FFFFBF","#E6F598","#ABDDA4","#66C2A5","#3288BD","#5E4FA2"),
                                  breaks = year_breaks, labels = year_breaks) +
    ggplot2::scale_x_continuous(breaks = xbreaks, limits = c(0,NA)) +
    ggplot2::labs(x = xlab, y = ylab) +
    ggplot2::ylim(0,NA)

  p <- add_scenario_facets(p, data, scales = scales, ncol = ncol)

  if(show_bias_adjust) {
    p <- p + ggplot2::geom_path(data = data, ggplot2::aes(x = spawn_bio, y = bias_adjusted, linetype = "B", colour = "B"))
  }

  # ___________________
  # Axes and theme
  # ___________________
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

