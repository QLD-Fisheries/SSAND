# Copyright 2024 Fisheries Queensland

# This file is part of SSAND.
# SSAND is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
# SSAND is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
# You should have received a copy of the GNU General Public License along with SSAND. If not, see <https://www.gnu.org/licenses/>.

#' Spaghetti plot
#'
#' Display biomass trajectories for multiple scenarios. See Details for customisation options.
#'
#' \itemize{
#' \item Can use pre-built template ("coloured" or "greyscale") or design a custom scheme on the fly.
#' \item A custom scheme can distinguish groups of scenarios using line-type and colour.
#' \item For example, you might want to show catch rate settings using colours, and natural mortality settings using linetype.
#' \item The variable linetype_categories expects a vector, as long as your list of scenarios, where each element of the vector specifies which linetype category the scenario belongs to.
#' \item For example, if you had six scenarios and wanted the first half belonging to group 1 and the second belonging to group 2, you would enter linetype_categories = c(1,1,1,2,2,2)
#' \item You can then specify labels for these groups using linetype_labels.
#' \item For example, linetype_labels = c("M = 1", "M = 2")
#' \item You can also specify the linetypes used for these categories using linetypes
#' \item For example, linetypes = c("solid", "dashed")
#' \item You can set up the colours a similar way, simply replacing "linetype" with "colour" in the examples above.
#' }
#'
#' @param data Output from spaghettiplot_prep(). A data frame with variables called year, scenario, base and value
#' @param template Specify a pre-built template to use: options are "coloured" or "greyscale"
#' @param show_base_case Set to TRUE to highlight the base case (logical)
#' @param scenarios A vector of scenarios to show on plot. Already specified in prep file, but this is a manual override to save running the prep function again
#' @param scenario_labels A vector of customised scenario names (character). Default is "Scenario 1", "Scenario 2", etc.
#' @param scenario_order A vector to reorder how scenarios are displayed (character). Use the label names defined in "scenario_labels".
#' If "scenario_labels" is left blank, the labels will be "Scenario 1", "Scenario 2" etc.
#' Any scenarios not included in "scenario_order" will be tacked on in the order they appear in the input data.
#' @param legend_labels A vector to specify legend labels
#' @param legend_position Legend position (default is "top")
#' @param xlab Label for x-axis (character). Default is "Year".
#' @param ylab Label for y-axis (character). Default is "Biomass (relative unfished)".
#' @param xbreaks A vector of breaks between x-axis labels, used in ggplot2::scale_x_continous() (numeric).
#' @param ybreaks A vector of breaks between y-axis labels, used in ggplot2::scale_y_continous() (numeric).
#' @param xlim A vector of lower and upper x-axis limits (e.g. c(1950, 2020)) (numeric).
#' @param ylim A vector of lower and upper y-axis limits (e.g. c(0,1)) (numeric).
#' @param xlabels A vector of labels for the x-axis breaks.
#' @param ylabels A vector of labels for the y-axis breaks.
#' @param show_scenario_labels Set to TRUE to include scenario labels on the plots (logical).
#' @param linetype_categories For a custom design scheme. A vector, as long as your list of scenarios, where each element of the vector specifies which linetype category the scenario belongs to.
#' @param linetype_labels A vector of labels for the groups specified in linetype_categories
#' @param linetypes A vector of the linetypes desired for the groups specified in linetype_labels
#' @param colour_categories For a custom design scheme. A vector, as long as your list of scenarios, where each element of the vector specifies which colour category the scenario belongs to.
#' @param colour_labels A vector of labels for the groups specified in colour_categories
#' @param colours A vector of the colours desired for the groups specified in colour_labels
#' @param line_width Width of lines
#' @param financial_year Set to TRUE if the assessment was based on financial year (logical). Adjusts the x-axis to show full financial year notation.
#' @param xangle Set to 90 to rotate x-axis labels 90 degrees.
#' @param force Force of repulsion between overlapping text labels. Default is 1.
#' @param text_size text_size, default 12
#' @param scenario_text_size scenario_text_size, default 4
#'
#' @return A spaghetti plot
#' @export
#'
#' @examples
#' data <- spaghettiplot_prep_DD(dd_mle)
#' spaghettiplot(data)
#' spaghettiplot(data, financial_year=TRUE)
#'
#' data <- spaghettiplot_prep_SS(ss_mle, ss_mcmc)
#' spaghettiplot(data)
#' spaghettiplot(data, template = "greyscale", line_width = 0.5)
#' spaghettiplot(data,
#'               linetype_categories = c(1,2),
#'               linetype_labels = c("Catch rate A", "Catch rate B"),
#'               linetypes = c("solid", "dashed"),
#'               colour_categories = c(1,2),
#'               colour_labels = c("High M", "Medium M"),
#'               colours = fq_palette("alisecolours")[1:2])
spaghettiplot <- function(data,
                          template = "coloured",
                          show_base_case = TRUE,
                          legend_labels = NULL,
                          legend_position = "top",
                          xlab = "Year",
                          ylab = "Biomass (relative unfished)",
                          xlim = c(NA,NA),
                          ylim = c(0,NA),
                          xbreaks = NULL,
                          ybreaks = NULL,
                          xlabels = NULL,
                          ylabels = NULL,
                          xangle = NULL,
                          show_scenario_labels = TRUE,
                          linetype_categories = NA,
                          linetype_labels = NA,
                          linetypes = NA,
                          colour_categories = NA,
                          colour_labels = NA,
                          colours = NA,
                          scenarios = NULL,
                          scenario_labels = NULL,
                          scenario_order = NULL,
                          line_width = 1,
                          financial_year = FALSE,
                          force = 1,
                          text_size = 12,
                          scenario_text_size = 4
) {

  # Data input warnings
  if (!"year" %in% names(data)) {warning("Input data is missing year column")}
  if (!"scenario" %in% names(data)) {warning("Input data is missing scenario column")}
  if (!"value" %in% names(data)) {warning("Input data is missing value column")}
  if (!"base" %in% names(data)) {warning("Input data is missing base column")}

  if (financial_year & xlab=="Year") {warning("Your x-axis implies calendar year, but you've indicated you're using financial year.")}

  if (missing(xlim)) {xlim <- c(min(data$year),max(data$year)+5)}
  if (missing(ylim)) {ylim <- c(0,max(data$value))}

  if (missing(xbreaks)) {xbreaks <- pretty(xlim)}
  if (missing(ybreaks)) {ybreaks <- pretty(ylim)}

  if (missing(xlabels)) {xlabels <- xbreaks}
  if (missing(ylabels)) {ylabels <- ybreaks}

  if (financial_year) {xlabels <- paste0(xbreaks-1,"\U2013",xbreaks)} else {xlabels <- xbreaks}
  if (missing(xangle)) {xangle <- ifelse(financial_year,90,0)}

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

  if (!missing(linetype_categories) & length(linetype_categories) != length(unique(data$scenario))) {
    warning("You have not specified a line type category for each scenario")
  }

  if (length(unique(linetype_categories)) !=  length(linetype_labels)) {
    warning("The number of linetype categories defined in linetype_categories is not the same as the number of line type labels provided in linetype_labels.")
  }

  if (!missing(linetypes) & length(linetype_labels) != length(linetypes)) {
    warning("The length of linetypes should either be blank or match the length of linetype_labels.")
  }


  if (!missing(colour_categories) & length(colour_categories) != length(unique(data$scenario))) {
    warning("You have not specified a colour category for each scenario")
  }

  if (length(unique(colour_categories)) !=  length(colour_labels)) {
    warning("The number of colour categories defined in colour_categories is not the same as the number of colour_labels provided in colour_labels")
  }

  if (!missing(colour_labels) & length(colour_labels) != length(colours)) {
    warning("The length of colours should either be blank or match the length of colour_labels")
  }

  if (!missing(colour_categories) && missing(linetype_categories)) {
    warning("If customising colour, please also customise linetype (even if there is only one category for linetype).")
  }

  if (missing(colour_categories) && !missing(linetype_categories)) {
    warning("If customising linetype, please also customise colour (even if there is only one category for colour).")
  }


  # Determine from data if a base case has been selected
  result <- sum(stringr::str_detect(data$base, '^Base$')) > 0
  if (!show_base_case) {data$base <- "Alt"}

  # Set up data based on manual line type categories
  if (!missing(colour_categories)) {
    template = "none"
  }
  if (!missing(linetype_categories)) {
    template = "none"
  }

  if (template=="none") {
    linetypedat <- data.frame(linetype_categories = linetype_categories) |>
      dplyr::mutate(linetypelabel = linetype_labels[linetype_categories]) |>
      dplyr::mutate(scenario = as.factor(dplyr::row_number())) |>
      dplyr::mutate(linetype_categories = as.factor(linetype_categories))

    data <- data |>
      dplyr::left_join(linetypedat, by = dplyr::join_by(scenario))
  }

  # Set up data based on manual colour categories
  if (template=="none") {
    colourdat <- data.frame(colour_categories = colour_categories) |>
      dplyr::mutate(colourlabel = colour_labels[colour_categories]) |>
      dplyr::mutate(scenario = as.factor(dplyr::row_number())) |>
      dplyr::mutate(colour_categories = as.factor(colour_categories))

    data <- data |>
      dplyr::left_join(colourdat, by = dplyr::join_by(scenario))
  }


  data <- data |>
    dplyr::mutate(linewidthcat = as.numeric(1)) |>
    dplyr::mutate(alphacat = as.numeric(1))

  # Use pre-build "coloured" template
  if (template == "coloured") {
    data <- data |>
      dplyr::mutate(linetype_categories = as.factor(1),
                    linetypelab = ifelse(base=="Base",
                                         paste0("Base case estimate (",as.numeric(unique(data$scenario[data$base=="Base"])),")"),
                                         paste0("Alternative scenarios (",min(as.numeric(as.character(data$scenario[data$base!="Base"]))),"-",max(as.numeric(as.character(data$scenario[data$base!="Base"]))),")")),
                    colour_categories = as.factor(scenario),
                    colourlabel = ifelse(base=="Base",
                                         paste0("Base case estimate (",as.numeric(unique(data$scenario[data$base=="Base"])),")"),
                                         paste0("Alternative scenarios (",min(as.numeric(as.character(data$scenario[data$base!="Base"]))),"-",max(as.numeric(as.character(data$scenario[data$base!="Base"]))),")")),
                    linewidthcat = 1,
                    alphacat = base)
  }



  # Use pre-build "grey scale" template
  if (template == "greyscale") {
    data <- data |>
      dplyr::mutate(linetype_categories = as.factor(base),
                    linetypelab = ifelse(base=="Base",
                                         paste0("Base case estimate (",as.numeric(unique(data$scenario[data$base=="Base"])),")"),
                                         paste0("Alternative scenarios (",min(as.numeric(as.character(data$scenario[data$base!="Base"]))),"-",max(as.numeric(as.character(data$scenario[data$base!="Base"]))),")")),
                    colour_categories = as.factor(base),
                    colourlabel = ifelse(base=="Base",
                                         paste0("Base case estimate (",as.numeric(unique(data$scenario[data$base=="Base"])),")"),
                                         paste0("Alternative scenarios (",min(as.numeric(as.character(data$scenario[data$base!="Base"]))),"-",max(as.numeric(as.character(data$scenario[data$base!="Base"]))),")")),
                    linewidthcat = 1,
                    alphacat = as.numeric(base))
  }

  # Set up legend labels
  if (missing(legend_labels)) {
    legend_labels = c(paste0("Base case estimate (",as.numeric(unique(data$scenario[data$base=="Base"])),")"),
                      paste0("Alternative scenarios (",min(as.numeric(as.character(data$scenario[data$base!="Base"]))),"-",
                             max(as.numeric(as.character(data$scenario[data$base!="Base"]))),")"))
  }

  if (!template=="none") {
    colour_labels = legend_labels
    linetype_labels = legend_labels
  }


  # Set up colour palettes
  if (missing(colours) & template=="coloured") {
    colours <- c("black",fq_palette("cols")[1:length(unique(data$scenario))])
  }

  if (template=="greyscale") {
    colours <- c("black","grey70")
  }

  # Set up linetypes
  if (missing(linetypes) & template=="coloured") {
    linetypes <- "solid"
  }

  if (template=="greyscale") {
    linetypes <- c("solid","dashed")
  }



  ### PLOT

  # Set up canvas
  p <- ggplot2::ggplot(data, ggplot2::aes(year, value, group=scenario)) +
    ggplot2::theme_bw() +
    ggplot2::coord_cartesian(xlim = c(min(data$year), max(data$year) + 5)) +
    ggplot2::xlab(xlab) +
    ggplot2::ylab(ylab) +
    ggplot2::theme(legend.title = ggplot2::element_blank()) +
    ggplot2::theme(text = ggplot2::element_text(size=text_size),legend.key.size = ggplot2::unit(3,"line")) +
    ggplot2::scale_x_continuous(limits = xlim, breaks = xbreaks, labels = xlabels) +
    ggplot2::scale_y_continuous(limits = ylim, breaks = ybreaks, labels = ylabels) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = xangle, vjust = 0.5, hjust=ifelse(xangle==90,0,0.5)))


  # Add data
  if (template == "coloured") {
    p <- p +
      ggplot2::geom_line(data = data, ggplot2::aes(colour=colour_categories, linetype=linetype_categories, alpha = as.factor(alphacat)), linewidth = line_width)
  }

  if (template == "greyscale") {
    p <- p +
      ggplot2::geom_line(data = data, ggplot2::aes(colour=colour_categories, linetype=linetype_categories), linewidth = line_width)
  }

  if (template == "none") {
    if (missing(colour_categories) & !missing(linetype_categories)) {

      if (missing(colours)) {
        p <- p +
          ggplot2::geom_line(data = data, ggplot2::aes(linetype=linetype_categories), colour = "grey50", linewidth = line_width)
      } else {
        p <- p +
          ggplot2::geom_line(data = data, ggplot2::aes(colour=linetype_categories, linetype=linetype_categories), linewidth = line_width)
      }
    } else {
      p <- p +
        ggplot2::geom_line(data = data, ggplot2::aes(colour=colour_categories, linetype=linetype_categories), linewidth = line_width)
    }
  }

  # Manual scales
  if (template=="none") {
    if (missing(linetypes)) {
      p <- p +
        ggplot2::scale_linetype_manual(name = "Legend", values = rep(c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash"),10), labels=linetype_labels)
    } else {
      p <- p +
        ggplot2::scale_linetype_manual(name = "Legend", values = linetypes, labels=linetype_labels)
    }

    if (missing(colours) & missing(colour_labels)) {
      p <- p +
        ggplot2::scale_colour_manual(name = "Legend", values = fq_palette("alisecolours"), labels=linetype_labels)
    }
    if (!missing(colours) & missing(colour_labels)) {
      p <- p +
        ggplot2::scale_colour_manual(name = "Legend", values = colours, labels=linetype_labels)
    }
    if (!missing(colours) & !missing(colour_labels)) {
      p <- p +
        ggplot2::scale_colour_manual(name = "Legend", values = colours, labels=colour_labels)
    }
  }


  if (template=="coloured") {
    p <- p +
      ggplot2::scale_colour_manual(name = "Legend", values = colours, labels=colour_labels, guide = "none") +
      ggplot2::scale_linetype_manual(name = "Legend", values = linetypes, labels=linetype_labels, guide = "none") +
      ggplot2::scale_alpha_manual(name = "Legend", values = c(1, 0.6), labels=colour_labels)


    # Overlay black base line on top if coloured==TRUE
    if(result){
      p <- p +
        ggplot2::geom_line(data = data |> dplyr::filter(base=="Base"), ggplot2::aes(), colour="black", alpha=1, linetype="solid", linewidth = line_width)
    }
  }


  if (template=="greyscale") {
    p <- p +
      ggplot2::scale_colour_manual(name = "Legend", values = colours, labels=colour_labels) +
      ggplot2::scale_linetype_manual(name = "Legend", values = linetypes, labels=linetype_labels)

    # Overlay black base line on top if coloured==TRUE
    if(result){
      p <- p +
        ggplot2::geom_line(data = data |> dplyr::filter(base=="Base"), ggplot2::aes(), colour="black", alpha=1, linetype="solid", linewidth = line_width)
    }
  }


  # Add legend
  p <- p +
    ggplot2::theme(legend.position=legend_position, legend.text = ggplot2::element_text(size=text_size))

  if (!show_base_case & template=="none") {
    p <- p +
      ggplot2::theme(legend.position = "none")
  }

  # Add scenario labels
  if (show_scenario_labels) {
    p <- p +
      ggrepel::geom_text_repel(
        data = subset(data, year == max(data$year)),
        ggplot2::aes(label = subset(data, year == max(data$year))$scenario_labels,
                     colour= subset(data, year == max(data$year))$colour_categories),
        size = scenario_text_size,
        nudge_x = 3,
        segment.color = '#cccccc',
        segment.size = 0.5,
        show.legend  = FALSE,
        max.overlaps = Inf,
        force=force
      )
  }

  return(p)
}
