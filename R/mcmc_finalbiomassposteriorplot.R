# Copyright 2024 Fisheries Queensland

# This file is part of SSAND.
# SSAND is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
# SSAND is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
# You should have received a copy of the GNU General Public License along with SSAND. If not, see <https://www.gnu.org/licenses/>.

#' Posterior plot for final biomass ratio with risk area (below B20) highlighted
#'
#' @param data List output from mcmc_finalbiomassposterior_prep()
#' @param colours A vector of colours used (character).
#' @param xlab Label for x-axis (character). Default is "".
#' @param ylab Label for y-axis (character). Default is "".
#' @param legend_position Position of the legend ("none", "left", "right", "bottom", "top", or two-element numeric vector for x and y position). Default is "top".
#' @param line_type Linetype for vertical lines. Default is "dashed".
#' @param line_type_label Label for linetype legend. Default is "X% falls within this range" where X is the credible_interval specified in the input data.
#' @param CI_label_position Specify position for labels for 95% confidence interval percentages. Default "bottom", alternative is "top".
#' @return A posterior plot for final biomass ratio with risk area (below B20) highlighted
#' @export
#'
#' @examples
#' ss_mcmc_ens <- mcmc_ensemble_SS(ss_mcmc)
#' data <- mcmc_finalbiomassposterior_prep_SS(ss_mle,ss_mcmc_ens)
#' mcmc_finalbiomassposteriorplot(data)
mcmc_finalbiomassposteriorplot <- function (data,
                                            colours = c("#AD3D25","#FCC17E","#0085B8","#00925B"),
                                            xlab = NULL,
                                            ylab = NULL,
                                            legend_position = "top",
                                            CI_label_position = "bottom",
                                            line_type = "dashed",
                                            line_type_label = NULL) {
  density <- data$density
  quant_lower <- data$quant_lower
  quant_upper <- data$quant_upper
  risk <- data$risk
  end_year <- data$end_year
  credible_interval <- data$credible_interval

  if (missing(xlab)) {xlab = paste0("Biomass at end of ",end_year-1)}
  if (missing(line_type_label)) {line_type_label = paste0(credible_interval*100,"% falls within this range")}

  # Keep all levels in legend by making fill a factor
  density <- density |>
    dplyr::mutate(fill = dplyr::case_when(x<=20 ~ 'A',
                                          x>=20 & x<=40 ~ 'B',
                                          x>=40 & x<=60 ~ 'C',
                                          x>=6 ~ 'D'),
                  fill = factor(fill, levels = c('A','B','C','D')))

  if (CI_label_position == 'top'){
    limits <- c(0,max(signif(max(data$density$x),2)+10,100))
    breaks <- sort(c(seq(0,max(signif(max(data$density$x),2)+10,100),20)))
    labels <- scales::percent(sort(seq(0,max(signif(max(data$density$x),2)+10,100),20))/100)
  } else if (CI_label_position == 'bottom'){
    limits <- c(0,max(signif(max(data$density$x),2)+10,100))
    breaks <- sort(c(seq(0,max(signif(max(data$density$x),2)+10,100),20),quant_lower,quant_upper))
    labels <- scales::percent(sort(c(seq(0,max(signif(max(data$density$x),2)+10,100),20),
                                     round(quant_lower),round(quant_upper))/100))
  }


  p <- ggplot2::ggplot() +
    ggplot2::theme_bw() +
    ggplot2::geom_area(data=density |> dplyr::filter(x<=20), ggplot2::aes(x=x,y=y,fill=fill)) +
    ggplot2::geom_area(data=density |> dplyr::filter(x>=20 & x<=40), ggplot2::aes(x=x,y=y,fill=fill)) +
    ggplot2::geom_area(data=density |> dplyr::filter(x>=40 & x<=60), ggplot2::aes(x=x,y=y,fill=fill)) +
    ggplot2::geom_area(data=density |> dplyr::filter(x>=60), ggplot2::aes(x=x,y=y,fill=fill)) +
    ggplot2::geom_vline(ggplot2::aes(linetype="E", xintercept = quant_lower)) +
    ggplot2::geom_vline(ggplot2::aes(linetype="E", xintercept = quant_upper)) +
    ggplot2::scale_fill_manual(drop = FALSE,
                               name="",
                               labels = c(
                                 as.expression(list(bquote(.(risk[1]) * "% falls below " * B[20]))),
                                 as.expression(list(bquote(.(risk[2]) * "% falls between " * B[20] * " and " * B[40]))),
                                 as.expression(list(bquote(.(risk[3]) * "% falls between " * B[40] * " and " * B[60]))),
                                 as.expression(list(bquote(.(risk[4]) * "% falls above " * B[60])))
                               ),
                               values=colours)+
    ggplot2::scale_linetype_manual(name="",values=line_type, labels = line_type_label) +
    ggplot2::theme(legend.position = legend_position,
                   panel.grid.minor.x = ggplot2::element_blank()) +
    ggplot2::xlab(xlab) +
    ggplot2::ylab(ylab) +
    ggplot2::scale_x_continuous(limits = limits,
                                breaks = breaks,
                                labels = labels) +
    ggplot2::theme(axis.ticks.y=ggplot2::element_blank(),axis.text.y=ggplot2::element_blank()) +
    ggplot2::theme(legend.box="vertical", legend.margin=ggplot2::margin())

  if (CI_label_position=="top"){
    labs <- data.frame(x = c(quant_lower, quant_upper),
                       y = c(Inf, Inf),
                       label = scales::percent(sort(c(round(quant_lower),round(quant_upper))/100)))
    p <- p +
      ggplot2::annotate('text', x=quant_lower+4, y=max(density$y), label = scales::percent(round(quant_lower)/100), size = 3) +
      ggplot2::annotate('text', x=quant_upper+4, y=max(density$y), label = scales::percent(round(quant_upper)/100), size = 3)
  }
  return(p)
}
