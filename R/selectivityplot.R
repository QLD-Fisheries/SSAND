# Copyright 2024 Fisheries Queensland

# This file is part of SSAND.
# SSAND is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
# SSAND is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
# You should have received a copy of the GNU General Public License along with SSAND. If not, see <https://www.gnu.org/licenses/>.

#' Selectivity plot
#'
#' @param data Output from selectivityplot_prep(). A data frame with variables fleet (num), year (int), sex (fac), value (num), type (fac), selectivity (num), scenario (fac)
#' @param selectivity_type A vector of types of selectivity information to show on the plot. Options are "Selectivity (length)", "Selectivity (age)", "Retention", "Discard mortality", "Dead", "Discard".
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
#' @param fleets A numeric or vector of fleet numbers to plot
#' @param fleet_names A vector of customised fleet names for legend
#' @param MLS Number for the minimum legal size. If a value is entered, a dashed line for MLS will be added to the plot.
#' @param MLS_colour Colour of MLS vertical line.
#' @param MLS_linetype Linetype of MLS vertical line.
#' @param MLS_position Vertical position of MLS label
#' @param MLS_label Label for MLS line. Default is "MLS: X cm"
#' @param MLS_label_size Size of label for MLS line
#' @param show_ribbon Set to TRUE to display 95% credible interval ribbon for MCMC results
#' @param time_blocks Set to TRUE to show time blocks. You might want to also set selectivity_type to something relevant such as "Discard"
#' @param years A vector of years to show. Relevant if time_blocks==TRUE.
#'
#' @return Selectivity plot
#' @export
#'
#' @examples
#' data <- selectivityplot_prep_SS(ss_mle)
#' selectivityplot(data)
#' selectivityplot(data, selectivity_type = "Selectivity (length)")
#' selectivityplot(data, selectivity_type = "Selectivity (age)", xlab = "Age (years)")
#' selectivityplot(data,
#'                 selectivity_type = c("Selectivity (length)",
#'                                      "Retention",
#'                                      "Dead",
#'                                      "Discard"),
#'                 xlab = "Age (years)")
#'
#' selectivityplot(data,
#'                 selectivity_type = c("Discard"),
#'                 time_blocks = TRUE,
#'                 scenarios = 1,
#'                 fleets = 2,
#'                 years = c(1993,2022))
selectivityplot <- function(data,
                            fleets = NULL,
                            fleet_names = NULL,
                            selectivity_type = "Selectivity (length)",
                            xlab = "Length (cm)",
                            ylab = "Selectivity",
                            text_size = 12,
                            scenarios = NULL,
                            scenario_labels = NULL,
                            scenario_order = NULL,
                            colours = NULL,
                            scales = 'free',
                            ncol = 2,
                            MLS = NULL,
                            MLS_colour = "black",
                            MLS_linetype = "dotted",
                            MLS_position = 0.9,
                            MLS_label = NULL,
                            MLS_label_size = 3,
                            show_ribbon = TRUE,
                            time_blocks = FALSE,
                            years = NULL) {
  if (data$scenario[1]=="Ensemble") {MCMC<-TRUE}else{MCMC<-FALSE}

  # Data input warnings
  if (!"fleet" %in% names(data)) {warning("Input data is missing fleet column")}
  if (!"year" %in% names(data)) {warning("Input data is missing year column")}
  if (!"sex" %in% names(data)) {warning("Input data is missing sex column")}
  if (!"value" %in% names(data)) {warning("Input data is missing value column")}
  if (!"type" %in% names(data)) {warning("Input data is missing type column")}
  if (!"selectivity" %in% names(data)) {warning("Input data is missing selectivity column")}
  if (!"scenario" %in% names(data)) {warning("Input data is missing scenario column")}

  if (missing(fleets)) {fleets <- unique(data$fleet)}

  if (!MCMC) {

    data <- data |>
      dplyr::filter(type %in% selectivity_type) |>
      dplyr::filter(fleet %in% fleets) |>
      dplyr::mutate(sex = dplyr::recode(sex, "1" = "Female" ,  "2" = "Male"))

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

    if (missing(fleet_names)) {
      data <- data |> dplyr::mutate(fleet_names = as.factor(paste0("Fleet ",fleet)))
    } else {
      fleet.lookup <- data.frame(fleet = unique(data$fleet), fleet_names = fleet_names)
      data <- data |>
        dplyr::left_join(fleet.lookup, by = "fleet") |>
        dplyr::mutate(fleet_names = as.factor(fleet_names))
    }

    if (missing(colours)) {colours = c("grey70",fq_palette("alisecolours"))}


    if(!time_blocks) {
    data <- data |> dplyr::filter(year==endyear)

    p <- ggplot2::ggplot(data) +
      ggplot2::geom_line(ggplot2::aes(x=value, y=selectivity, colour=type, linetype=sex)) +
      ggplot2::theme_bw() +
      ggplot2::xlab(xlab) +
      ggplot2::ylab(ylab) +
      ggplot2::ylim(c(0,1)) +
      ggplot2::theme(text = ggplot2::element_text(size=text_size)) +
      ggplot2::theme(legend.position = "top", legend.title = ggplot2::element_blank()) +
      ggplot2::scale_colour_manual(values = colours)
    }

    if(time_blocks) {
      if (!missing(years)) {data <- data |> dplyr::filter(year %in% years)}
      data <- data |> dplyr::mutate(year = as.factor(year))

      p <- ggplot2::ggplot(data) +
        ggplot2::geom_line(ggplot2::aes(x=value, y=selectivity, colour=year, linetype=sex)) +
        ggplot2::theme_bw() +
        ggplot2::xlab(xlab) +
        ggplot2::ylab(ylab) +
        ggplot2::ylim(c(0,1)) +
        ggplot2::theme(text = ggplot2::element_text(size=text_size)) +
        ggplot2::theme(legend.position = "top", legend.title = ggplot2::element_blank()) +
        ggplot2::scale_colour_manual(values = colours)
    }
  }

  if (MCMC) {
    p <- ggplot2::ggplot(data) +
      ggplot2::geom_line(ggplot2::aes(x = value, y = selectivity), colour = "grey30") +
      ggplot2::scale_fill_manual("", values="grey30") +
      ggplot2::theme_bw() +
      ggplot2::xlab(xlab) +
      ggplot2::ylab(ylab) +
      ggplot2::theme(legend.position="top")

    if (show_ribbon) {
      p <- p +
        ggplot2::geom_ribbon(ggplot2::aes(x = value, ymin = lb, ymax= ub, fill = "95% credible interval"), alpha = 0.3)
    }
  }

  if (!missing(MLS)) {
    if (missing(MLS_label)) {MLS_label = "MLS"}
    p <- p +
      ggplot2::geom_vline(xintercept=MLS,linetype=MLS_linetype, colour=MLS_colour) +
      ggplot2::annotate("text", x=MLS*1.05, y=MLS_position, label=MLS_label, hjust=0, colour=MLS_colour, size=MLS_label_size)
  }

  # Facet as required
    if (length(unique(data$fleet))>1 && length(unique(data$scenario))>1) {
      p <- p +
        ggplot2::facet_grid(rows=ggplot2::vars(fleet_names), cols=ggplot2::vars(scenario_labels))
    }

    if (length(unique(data$fleet))==1 && length(unique(data$scenario))>1) {
      p <- p +
        ggplot2::facet_wrap(~scenario_labels)
    }

    if (length(unique(data$fleet))>1 && length(unique(data$scenario))==1) {
      p <- p +
        ggplot2::facet_wrap(~fleet_names)
    }

  return(p)
}
