# Copyright 2024 Fisheries Queensland

# This file is part of SSAND.
# SSAND is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
# SSAND is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
# You should have received a copy of the GNU General Public License along with SSAND. If not, see <https://www.gnu.org/licenses/>.

#' Age composition plot with an option to add model fits
#'
#' @param data Output from ageplot_prep(). A data frame with year (int), fleet (int), bin (int), sex (chr), obs (num), exp (num), scenario (num)
#' @param show_fits Set to TRUE to show model fits.
#' @param fleet Specify which fleet to plot (numeric). By default, fleet 1 will be shown.
#' @param point_size Size of points used in ggplot2::geom_line(). Default is 1.
#' @param colours A vector of colours used for sex types (character).
#' @param xlab Label for x-axis (character). Default is "Year".
#' @param ylab Label for y-axis (character). Default is "Spawning biomass (relative)".
#' @param xbreaks A vector of breaks between x-axis labels, used in ggplot2::scale_x_continous() (numeric).
#' @param ybreaks A vector of breaks between y-axis labels, used in ggplot2::scale_y_continous() (numeric).
#' @param xlabels A vector of labels for the x-axis breaks.
#' @param ylabels A vector of labels for the y-axis breaks.
#' @param xlim A vector of lower and upper x-axis limits (e.g. c(1950, 2020)) (numeric).
#' @param ylim A vector of lower and upper y-axis limits (e.g. c(0,1)) (numeric).
#' @param scenarios A vector of scenario numbers to be shown on plot (numeric). This was already specified in prep file, but this is a manual override to save running the prep function again.
#' @param scenario_labels A vector of customised scenario names (character). Default is "Scenario 1", "Scenario 2", etc.
#' @param scenario_order A vector to reorder how scenarios are displayed (character). Use the label names defined in "scenario_labels".
#' @param scales Scales for ggplot2::facet_wrap(). Default is 'free', see ?ggplot2::facet_wrap for options.
#' @param financial_year Set to TRUE if the assessment was based on financial year (logical). Adjusts the x-axis to show full financial year notation.
#' @param ncol Number of columns for facet_wrap(). Default is 2.
#' @param direction Direction of facet wrap (character). Enter "v" for vertical or "h" for horizontal.
#' @param MLS Value of minimum legal size (or equivalent) (numeric). Leave blank to exclude from plot.
#' @param MLS_colour Colour of MLS line (character). Default is "black".
#' @param MLS_label Label of MLS line (character). Default is "MLS".
#' @param scales Scales for ggplot2::facet_wrap(). Default is 'free', see ?ggplot2::facet_wrap for options.
#' @param text_size,legend_text_size,text_colour,legend_text_colour,legend_position,legend_box,legend_title_blank,panel_border,panel_border_colour,xangle Optional plotting theme overrides. Defaults are controlled by `theme_ssand()`
#'   and can be set globally via `set_ssand_style()`.
#'
#' @return An age plot, with an option to add model fits
#' @export
#'
#' @examples
#' \dontrun{
#' data <- ageplot_prep_SS(ss_mle)
#' ageplot(data, show_fits=FALSE)
#' ageplot(data)
#' }
ageplot <- function(data,
                    show_fits = TRUE,
                    fleet = 1,
                    point_size = 1,
                    colours = c("#9E480E", "#7CC8FC"),
                    xlab = "Age (years)",
                    ylab = "Proportion",
                    xbreaks = NULL,
                    ybreaks = NULL,
                    xlabels = NULL,
                    ylabels = NULL,
                    xlim = NULL,
                    ylim = NULL,
                    scenarios = NULL,
                    scenario_labels = NULL,
                    scenario_order = NULL,
                    scales = "fixed",
                    ncol = 4,
                    direction = "v",
                    MLS = NULL,
                    MLS_colour = "black",
                    MLS_label = "MLS",
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

  # Data input warnings
  check_data_columns(data, c("year","fleet","bin","sex","obs","exp","scenario"))
  if (direction == "vertical") {direction = "v"}
  if (direction == "horizontal") {direction = "h"}

  data$xvar <- data$bin
  data$upper <- mutate(data,max())

  if (is.null(xlim)) {xlim <- c(min(data$bin),max(data$bin))}
  if (is.null(ylim)) {ylim <- c(min(min(data$obs),min(data$exp)),
                                max(max(data$obs),max(data$exp)))}

  # ___________________
  # Custom to this plot
  # ___________________
  data <- data |> dplyr::filter(fleet==fleet)
  data <- apply_scenarios(data, scenarios, scenario_labels, scenario_order)

  # ___________________
  # Basic plot set up
  # ___________________
  p <- ggplot2::ggplot(data)

  # ___________________
  # Build MLE plot
  # ___________________
  p <- p +
    ggplot2::geom_area( ggplot2::aes(x=bin, y=obs, group=sex, fill=as.factor(sex)),alpha=0.7, colour="black", outline.type = "full") +
    ggplot2::geom_point(ggplot2::aes(x=bin, y=obs, group=sex), size = point_size) +
    ggplot2::facet_wrap(~year, scales=scales, ncol = ncol, dir = direction) +
    ggplot2::scale_fill_manual(name = "Sex", values=c("grey60", "grey30", "grey80"))

  if (show_fits) {
    p <- p +
      ggplot2::geom_line(ggplot2::aes(x=bin, y=exp, group=sex, colour=sex), linewidth=1) +
      ggplot2::scale_colour_manual(name="Sex", values = colours)
  }

  if (!missing(MLS)) {
    p <- p +
      ggplot2::geom_vline(aes(xintercept = MLS, linetype = MLS_label), colour = MLS_colour) +
      ggplot2::scale_linetype_manual(values = "dashed", name = ggplot2::element_blank())
  }

  # ___________________
  # Axes and theme
  # ___________________
  x_axis <- build_x_axis(x = data$xvar,
                         xlab = xlab,
                         xlim = xlim,
                         xbreaks = xbreaks,
                         xlabels = xlabels,
                         financial_year = financial_year,
                         expand_upper = 0,
                         xangle = xangle,
                         is_date = FALSE)

  y_axis <- build_y_axis(y = data$value,
                         ylab = ylab,
                         ylim = ylim,
                         ybreaks = ybreaks,
                         ylabels = ylabels,
                         lower = 0,
                         upper = data$upper)

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



