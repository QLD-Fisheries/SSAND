# Copyright 2024 Fisheries Queensland

# This file is part of SSAND.
# SSAND is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
# SSAND is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
# You should have received a copy of the GNU General Public License along with SSAND. If not, see <https://www.gnu.org/licenses/>.

#' Influence plot
#'
#' @param data A data frame prepared outside of SSAND, containing CPUE model outputs for at least one CPUE model.
#' Columns should be year (int), cpue (num), cpue_lwr (num), cpue_upr (num), model (num)
#' @param step Set to TRUE to step out addition of influences. If FALSE, all models are on one panel.
#' @param model_names A vector of labels for CPUE models (e.g. c("Raw", "Add season", "Add fishing power"))
#' @param colours A vector of colours used (character).
#' @param xlab Label for x-axis (character). Default is "Year".
#' @param xlim A vector of lower and upper x-axis limits (e.g. c(1950, 2020)) (numeric).
#' @param xbreaks A vector of breaks between x-axis labels, used in ggplot2::scale_x_continous() (numeric).
#' @param ylab Label for y-axis (character). Default is "Catch rate (kg/day)".
#' @param ylim A vector of lower and upper y-axis limits (e.g. c(0,1)) (numeric).
#' @param ybreaks A vector of breaks between y-axis labels, used in ggplot2::scale_y_continous() (numeric).
#' @param show_CI Set to TRUE to show a confidence intervals (logical).
#' @param legend_position Position of the legend ("none", "left", "right", "bottom", "top", or two-element numeric vector for x and y position). Default is "top".
#'
#' @return A plot showing the influence of each term in a CPUE model
#' @export
#'
#' @examples
#' data <- data.frame(year=rep(1988:2024,6),
#'                    cpue = c(sin(1988:2024 / 10),
#'                             cos(1988:2024 / 10),
#'                             cos(sin(1988:2024 / 10)),
#'                             sin(cos(1988:2024 / 10)),
#'                             sin(cos(sin(1988:2024 / 10))),
#'                             cos( sin(cos(1988:2024 / 10)))),
#'                    cpue_lwr = c(sin(1988:2024 / 10) /1.1,
#'                                 cos(1988:2024 / 10)  /1.1,
#'                                 cos(sin(1988:2024 / 10))  /1.1,
#'                                 sin(cos(1988:2024 / 10)) /1.1,
#'                                 sin(cos(sin(1988:2024 / 10)) /1.1),
#'                                 cos(sin(cos(1988:2024 / 10)))/1.1),
#'                    cpue_upr = c(sin(1988:2024 / 10) /1.1,
#'                                 cos(1988:2024 / 10)  /0.9,
#'                                 cos(sin(1988:2024 / 10))  /0.9,
#'                                 sin(cos(1988:2024 / 10)) /0.9,
#'                                 sin(cos(sin(1988:2024 / 10)) /0.9),
#'                                 cos(sin(cos(1988:2024 / 10)))/0.9),
#'                    model=c(rep(1,37),rep(2,37),rep(3,37),
#'                               rep(4,37),rep(5,37),rep(6,37)))
#'
#' influenceplot(data)
#' influenceplot(data, step=FALSE)
#' influenceplot(data, step=FALSE, show_CI=TRUE)
#' influenceplot(data, show_CI=TRUE)
#' influenceplot(data, model_names = c("Raw","Add A","Add B",
#'                                      "Add C","Add D","Add E"))
influenceplot <- function(data,
                          step = TRUE,
                          model_names = NULL,
                          colours = NULL,
                          xlab = "Year",
                          xlim = NULL,
                          xbreaks = NULL,
                          ylab = "Catch rate (kg/day)",
                          ylim = NULL,
                          ybreaks = NULL,
                          legend_position = "top",
                          show_CI = FALSE
) {
  if (!missing(model_names) & !length(model_names)==length(unique(data$model))) {warning("Please provide a model name for every unique model in data")}
  if (missing(colours) & !step) {colours <- c(colours, fq_palette("cols"))}
  if (missing(colours) & step) {colours <- c("black","grey70")}

  if (missing(xlim)) {xlim <- c(min(data$year),max(data$year))}
  if (!show_CI & missing(ylim)) {ylim <- c(0,max(data$cpue))}
  if (show_CI & missing(ylim)) {ylim <- c(0,max(max(data$cpue,na.rm = T),max(data$cpue_upr,na.rm = T)))}

  if (missing(xbreaks)) {xbreaks <- pretty(xlim)}
  if (missing(ybreaks)) {ybreaks <- pretty(ylim)}

  if (!missing(model_names)) {
    model_lookup <- data.frame(model_index = 1:length(sort(unique(data$model))),
                               model = sort(unique(data$model)))

    data <- data |>
      dplyr::left_join(model_lookup, by="model") |>
      dplyr::mutate(model = model_names[model_index])
  } else {
  model_lookup <- data.frame(model_index = 1:length(sort(unique(data$model))),
                               model = sort(unique(data$model)))

    data <- data |>
      dplyr::left_join(model_lookup, by="model")
  }

  p <- ggplot2::ggplot(data)

  if (!step) {
    if (show_CI) {
      p  <- p +
        ggplot2::geom_line(ggplot2::aes(x=year,y=cpue_lwr, colour=as.factor(model)), linetype="dashed") +
        ggplot2::geom_line(ggplot2::aes(x=year,y=cpue_upr, colour=as.factor(model)), linetype="dashed")
    }

    p <- p +
      ggplot2::geom_point(ggplot2::aes(x=year,y=cpue, colour=as.factor(model))) +
      ggplot2::geom_line(ggplot2::aes(x=year,y=cpue, colour=as.factor(model))) +
      ggplot2::scale_x_continuous(limits = xlim, breaks = xbreaks) +
      ggplot2::scale_y_continuous(limits = ylim, breaks = ybreaks) +
      ggplot2::xlab(xlab) +
      ggplot2::ylab(ylab) +
      ggplot2::theme_bw() +
      ggplot2::theme(legend.position = legend_position,
                     legend.title = ggplot2::element_blank())
  }
  if (step) {

    models <- sort(unique(data$model_index))
    data_step <- data.frame()
    for (i in models[1:(length(models)-1)]) {
      tmp <- data |>
        dplyr::filter(model_index %in% c(i,i+1)) |>
        dplyr::mutate(panel = max(model_index)) |>
        dplyr::mutate(treatment = ifelse(model_index==i, "Previous step","Added influence"))

    data_step <- rbind(data_step,tmp)
    }
    if (!missing(model_names)) {
      data_step <- data_step |>
        dplyr::mutate(panel = model_names[panel])
    }

    p <- ggplot2::ggplot(data_step)

      if (show_CI) {
        p  <- p +
          ggplot2::geom_ribbon(ggplot2::aes(x=year,ymin=cpue_lwr,ymax=cpue_upr,fill=as.factor(treatment)), alpha=0.1)
      }

      p <- p +
        ggplot2::geom_point(ggplot2::aes(x=year,y=cpue, colour=as.factor(treatment), shape=treatment)) +
        ggplot2::geom_line (ggplot2::aes(x=year,y=cpue, colour=as.factor(treatment), linetype=treatment)) +
        ggplot2::scale_linetype_manual(name="Model", values=c("solid","dashed")) +
        ggplot2::scale_shape_manual(name="Model", values=c(1,2)) +
        ggplot2::scale_colour_manual(name="Model", values=colours) +
        ggplot2::scale_fill_manual(name="Model", values=colours) +
        ggplot2::scale_x_continuous(limits = xlim, breaks = xbreaks, labels=xbreaks) +
        ggplot2::scale_y_continuous(limits = ylim, breaks = ybreaks, labels=ybreaks) +
        ggplot2::theme_bw() +
        ggplot2::theme(legend.position = legend_position,
                       legend.title = ggplot2::element_blank()) +
        ggplot2::facet_wrap(~panel)
  }
  return(p)
}
