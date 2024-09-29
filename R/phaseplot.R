# Copyright 2024 Fisheries Queensland

# This file is part of SSAND.
# SSAND is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
# SSAND is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
# You should have received a copy of the GNU General Public License along with SSAND. If not, see <https://www.gnu.org/licenses/>.

#' Phase plot
#'
#' @param data Output from phaseplot_prep()
#' @param xbreaks A vector of breaks between x-axis labels, used in ggplot2::scale_x_continous() (numeric).
#' @param text_size Text size (num). Default is 12.
#' @param year_labels A vector of years to be displayed on plot (numeric).
#' @param year_label_size Text size for year labels (num). Default is 3.
#' @param scenarios A vector of scenario numbers to be shown on plot (numeric). This was already specified in prep file, but this is a manual override to save running the prep function again.
#' @param scenario_labels A vector of customised scenario names (character). Default is "Scenario 1", "Scenario 2", etc.
#' @param scenario_order A vector to reorder how scenarios are displayed (character). Use the label names defined in "scenario_labels".
#' If "scenario_labels" is left blank, the labels will be "Scenario 1", "Scenario 2" etc.
#' Any scenarios not included in "scenario_order" will be tacked on in the order they appear in the input data.
#' @param scales Scales for ggplot2::facet_wrap(). Default is 'free', see ?ggplot2::facet_wrap for options.
#' @param ncol Number of columns for facet_wrap(). Default is 2.
#' @param xlab Label for x-axis (character). Default is "Biomass (relative)".
#' @param ylab Label for y-axis (character). Default is "Fishing pressure ratio (relative to FMSY))".
#'
#' @return A phase plot
#' @export
#'
#' @examples
#' data <- phaseplot_prep_SS(ss_mle)
#' phaseplot(data)
#'
#' data <- phaseplot_prep_DD(dd_mle)
#' phaseplot(data)
phaseplot <- function(data,
                      xbreaks = seq(0,2,0.2),
                      scales = 'free_y',
                      text_size = 12,
                      year_label_size = 3,
                      ncol = 2,
                      year_labels = NULL,
                      scenarios = NULL,
                      scenario_labels = NULL,
                      scenario_order = NULL,
                      xlab = "Biomass (relative)",
                      ylab = expression(Fishing~pressure~ratio~(relative~to~F[MSY]))) {

  # Data input warnings
  if (!"year" %in% names(data)) {warning("Input data is missing year column")}
  if (!"Bratio" %in% names(data)) {warning("Input data is missing Bratio column")}
  if (!"F_" %in% names(data)) {warning("Input data is missing F_ column")}
  if (!"scenario" %in% names(data)) {warning("Input data is missing scenario column")}
  if (!"B_MSY" %in% names(data)) {warning("Input data is missing B_MSY column")}
  if (!"F_max" %in% names(data)) {warning("Input data is missing F_max column")}
  if (!missing(scenarios)){data <- data |> dplyr::filter(scenario %in% scenarios)}

  if (missing(scenario_labels)) {
    data <- data |> dplyr::mutate(scenario_labels = as.factor(paste0("Scenario ",scenario)))
  } else {
    scenario.lookup<- data.frame(scenario = unique(data$scenario), scenario_labels = scenario_labels)
    data <- data |>
      dplyr::left_join(scenario.lookup, by = "scenario") |>
      dplyr::mutate(scenario_labels = as.factor(scenario_labels))
  }

  if (!missing(scenario_order)) {
    # Add on any scenarios not included in the scenario_order list
    scenario_order = c(scenario_order, setdiff(scenario_labels, scenario_order))
    # Reorder scenarios
    data$scenario_labels <- factor(data$scenario_labels, levels = scenario_order)
  }

  PhaseData <- data
  cols <- c("#ECB1A2","#FFD699","#FFE699","#C6DEB5","#CC6677")

  # If no years are specified to show, select first and last year, plus 8 other random years
  if (missing(year_labels)) {
    year_labels <- c(data$year[1],
                    unique(data$year)[sample(length(unique(data$year)), size=8)] ,
                    data$year[length(data$year)])
    year_labels <- unique(year_labels)
  }
    PhaseData <- PhaseData |>
      dplyr::mutate(show_year = ifelse(year %in% year_labels, TRUE, FALSE))


  p <- ggplot2::ggplot(PhaseData) +
    ggplot2::geom_rect(ggplot2::aes(xmin = 0, xmax = B_MSY, ymin = ifelse(F_max>0.44,1,F_max), ymax = F_max), fill = cols[1], alpha = .05) +
    ggplot2::geom_rect(ggplot2::aes(xmin = 0, xmax = B_MSY, ymin = 0, ymax = ifelse(F_max>0.44,1,F_max)), fill = cols[2], alpha = .05) +
    ggplot2::geom_rect(ggplot2::aes(xmin = B_MSY, xmax = max(Bratio*1.1), ymin = ifelse(F_max>0.44,1,F_max), ymax = F_max), fill = cols[3], alpha = .05) +
    ggplot2::geom_rect(ggplot2::aes(xmin = B_MSY, xmax = max(Bratio*1.1), ymin = 0, ymax = ifelse(F_max>0.44,1,F_max)), fill = cols[4], alpha = .05) +
    ggplot2::geom_point(ggplot2::aes(x=Bratio, y= F_),col=1,pch=21, cex=3) +
    ggplot2::geom_point(data=PhaseData[nrow(PhaseData),],ggplot2::aes(x=Bratio, y= F_),col="gray30",bg=1,pch=21, cex=6) +
    ggplot2::geom_path(ggplot2::aes(x=Bratio, y= F_),linewidth = 0.8, colour = 1) +
    ggrepel::geom_text_repel(data = PhaseData[PhaseData$show_year,],
                             ggplot2::aes(x = Bratio, y = F_, label = year),
                             size = year_label_size,
                             show.legend  = FALSE,
                             max.overlaps = Inf) +
    ggplot2::xlab(xlab) +
    ggplot2::ylab(ylab) +
    ggplot2::theme_bw() +
    ggplot2::scale_x_continuous(breaks=xbreaks)+
    ggplot2::theme(text = ggplot2::element_text(size=text_size))
  if (length(unique(PhaseData$scenario))>1){
    p <- p + ggplot2::facet_wrap(~scenario, scales = scales, ncol = ncol)
  }

  return(p)
}

