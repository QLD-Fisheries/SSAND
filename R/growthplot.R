# Copyright 2024 Fisheries Queensland

# This file is part of SSAND.
# SSAND is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
# SSAND is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
# You should have received a copy of the GNU General Public License along with SSAND. If not, see <https://www.gnu.org/licenses/>.

#' Growth plot
#'
#' @param data A data frame with variables age (int), value (num), lower (num), upper (num), sex (int), scenario (int)
#' @param xlab Label for x-axis (character). Default is "Age".
#' @param ylab Label for y-axis (character). Default is "Carapace length (cm, beginning of year)".
#' @param text_size Text size (num). Default is 12.
#' @param show_two_sex Set to TRUE to activate a feature that is relevant for two-sex models (logical).
#' @param scenarios A vector of scenario numbers to be shown on plot (numeric). This was already specified in prep file, but this is a manual override to save running the prep function again.
#' @param scenario_labels A vector of customised scenario names (character). Default is "Scenario 1", "Scenario 2", etc.
#' @param scenario_order A vector to reorder how scenarios are displayed (character). Use the label names defined in "scenario_labels".
#' If "scenario_labels" is left blank, the labels will be "Scenario 1", "Scenario 2" etc.
#' Any scenarios not included in "scenario_order" will be tacked on in the order they appear in the input data.
#' @param colours A vector of colours used (character).
#' @param scales Scales for ggplot2::facet_wrap(). Default is 'free', see ?ggplot2::facet_wrap for options.
#' @param ncol Number of columns for facet_wrap(). Default is 2.
#' @param variation_on_variation Set to TRUE to illustrate MCMC variation on the CV or SD values. Default (FALSE) sets the variation in growth to the median MCMC value.
#'
#' @return Growth plot
#' @export
#'
#' @examples
#' data <- growthplot_prep_SS(ss_mle)
#' growthplot(data)
#'
#' data <- growthplot_prep_DD(dd_mle)
#' growthplot(data)
growthplot <- function(data,
                       xlab = "Age",
                       ylab = "Carapace length (cm, beginning of year)",
                       text_size = 12,
                       show_two_sex=NULL,
                       scenarios = NULL,
                       scenario_labels = NULL,
                       scenario_order = NULL,
                       colours = NULL,
                       scales = 'free',
                       ncol = 2,
                       variation_on_variation = FALSE) {

  # ___________________
  # Data validation
  # ___________________

  # Identify MCMC or MLE
  MCMC <- "CV_lower" %in% names(data)

  # Data input warnings
  check_data_columns(data, c("age","value","lower","upper","sex","scenario"))
  if (MCMC) check_data_columns(data, c("CV_lower","CV_middle","CV_upper","growthCVtype"))

  data$xvar <- data$age

  # ___________________
  # Custom to this plot
  # ___________________

  if (missing(show_two_sex)) {
    tmp1 <- 1 %in% data$sex
    tmp2 <- 2 %in% data$sex
    show_two_sex <- tmp1 & tmp2
  }

  if (missing(colours)) {
    if (show_two_sex) {
      colours = c("#F4BB48","#248BB7","#6BA357") # yellow, blue, green
    } else {
      colours = "#9D9D9D" # grey colour for single sex plot
    }
  }

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

  y_axis <- build_y_axis(y = data$value,
                         ylab = ylab,
                         ylim = ylim,
                         ybreaks = ybreaks,
                         ylabels = ylabels,
                         lower = 0,
                         upper = data$upper)


  p <- ggplot2::ggplot(data) + get_theme_ssand()
  p <- add_x_scale_continuous(p, x_axis)
  p <- add_y_scale_continuous(p, y_axis)

  # ___________________
  # Build MLE plot
  # ___________________

  if (!MCMC) {
    if(show_two_sex) {
      dataF <- data[data$sex == 1, ]
      dataM <- data[data$sex == 2, ]
      p <- p +
        ggplot2::geom_line(data = dataF, ggplot2::aes(x=age, y=value, colour = "A", linetype = "A"), linewidth=1.05) +
        ggplot2::geom_line(data = dataF,ggplot2::aes(x=age, y=lower, colour = "A"), linetype="dotted") +
        ggplot2::geom_line(data = dataF,ggplot2::aes(x=age, y=upper, colour = "A"), linetype="dotted") +
        ggplot2::geom_ribbon(data = dataF,ggplot2::aes(x=age, ymin=lower, ymax=upper), fill=colours[1], alpha=0.2) +
        ggplot2::geom_line(data = dataM, ggplot2::aes(x=age, y=value, colour = "B", linetype = "B"),  linewidth=1.05) +
        ggplot2::geom_line(data = dataM, ggplot2::aes(x=age, y=lower, colour = "B"), linetype="dotted") +
        ggplot2::geom_line(data = dataM, ggplot2::aes(x=age, y=upper, colour = "B"), linetype="dotted") +
        ggplot2::geom_ribbon(data = dataM, ggplot2::aes(x=age, ymin=lower, ymax=upper), fill=colours[2], alpha=0.2) +
        ggplot2::scale_colour_manual(c("", ""),values=colours, labels = c("Female", "Male")) +
        ggplot2::scale_linetype_manual(c("", ""),values=c("solid","dashed"), labels = c("Female", "Male"))
    } else {
      p <- p +
        ggplot2::geom_line(ggplot2::aes(x=age, y=value), colour= colours, linewidth=1.05)

      if (prod(!is.na(data$lower))){
        p <- p +
          ggplot2::geom_line(ggplot2::aes(x=age, y=lower), colour=colours, linetype="dotted") +
          ggplot2::geom_line(ggplot2::aes(x=age, y=upper), colour=colours, linetype="dotted") +
          ggplot2::geom_ribbon(ggplot2::aes(x=age, ymin=lower, ymax=upper), fill=colours, alpha=0.2)
      }
    }
  }

  # ___________________
  # Build MCMC plot
  # ___________________

  if (MCMC) {
    if (!variation_on_variation) {
      p <- p +
        ggplot2::geom_ribbon(ggplot2::aes(x = age, ymin = value-1.96*CV_middle, ymax= value+1.96*CV_middle, fill = "Variation in growth using median standard deviation")) +
        ggplot2::geom_ribbon(ggplot2::aes(x = age, ymin = lower, ymax= upper, fill = "95% credible interval")) +
        ggplot2::geom_line(ggplot2::aes(x = age, y = value, linetype = "Median growth"), colour = "grey30") +
        ggplot2::scale_fill_manual("", values=c("grey65","grey85")) +
        ggplot2::scale_linetype_manual("", values=c("solid","dashed"))
    } else {
      p <- p +
        ggplot2::geom_ribbon(ggplot2::aes(x = age, ymin = value-1.96*CV_lower, ymax = value-1.96*CV_upper, fill = "95% credible interval for variation in growth")) +
        ggplot2::geom_ribbon(ggplot2::aes(x = age, ymin = value+1.96*CV_lower, ymax = value+1.96*CV_upper, fill = "95% credible interval for variation in growth")) +
        ggplot2::geom_line(ggplot2::aes(x = age, y = value-1.96*CV_middle, linetype="Variation in growth")) +
        ggplot2::geom_line(ggplot2::aes(x = age, y = value+1.96*CV_middle, linetype="Variation in growth")) +
        ggplot2::geom_ribbon(ggplot2::aes(x = age, ymin = lower, ymax= upper, fill = "95% credible interval for growth")) +
        ggplot2::geom_line(ggplot2::aes(x = age, y = value, linetype = "Growth"), colour = "grey30") +
        ggplot2::scale_fill_manual("", values=c("grey65","grey85")) +
        ggplot2::scale_linetype_manual("", values=c("solid","dashed"))
    }
  }

  # ___________________
  # Final layers
  # ___________________

  p <- add_scenario_facets(p, data, scales = scales, ncol = ncol)

  return(p)
}
