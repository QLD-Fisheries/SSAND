# Copyright 2024 Fisheries Queensland

# This file is part of SSAND.
# SSAND is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
# SSAND is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
# You should have received a copy of the GNU General Public License along with SSAND. If not, see <https://www.gnu.org/licenses/>.

#' MCMC traceplot
#'
#' @param data Output from mcmc_posteriordensityplot_prep(). A list of dataframes
#' @param colours A vector of colours used (character).
#' @param show_opt Show maximum likelihood estimate from optim()
#' @param ncol Number of columns for facet_wrap(). Default is .
#' @param sample Number of samples to plot from each MCMC chain to ease burden of rendering dense plots (numeric).
#' @param xlab Label for x-axis (character). Default is "".
#' @param ylab Label for y-axis (character). Default is "".
#'
#' @return Trace plot
#' @export
#'
#' @examples
#' parameters <- extract_SS_parameters(ss_mcmc)[c(2:9),]
#' data <- mcmc_posteriordensityplot_prep_SS(ss_mle,
#'                                           ss_mcmc,
#'                                           scenario = 1,
#'                                           parameters,
#'                                           show_objective_function=TRUE)
#' mcmc_traceplot(data)
#'
#' \dontrun{
#' library(DDUST)
#' dd_sim <- simulate_DDUST(dd_mle,dd_mcmc)
#' data <- mcmc_posteriordensityplot_prep_DD(dd_mle, dd_mcmc,
#'                                           dd_sim, scenario = 1)
#' mcmc_traceplot(data)
#' }
mcmc_traceplot <- function(data,
                           colours = c("#b4d9eb", "#cc8c97", "#ddd3a0", "#7ed19a", "#988dd3"),
                           show_opt=FALSE,
                           ncol = 2,
                           sample = NULL,
                           xlab = "",
                           ylab = ""){

  # Data input warnings
  if (!"chain" %in% names(data[[1]])) {warning("Input data is missing chain column")}
  if (!"iter" %in% names(data[[1]])) {warning("Input data is missing iter column")}
  if (!"parameter" %in% names(data[[1]])) {warning("Input data is missing parameter column")}
  if (!"value" %in% names(data[[1]])) {warning("Input data is missing value column")}
  if (!"parameter" %in% names(data[[2]])) {warning("Input data is missing parameter column")}
  if (!"value" %in% names(data[[2]])) {warning("Input data is missing value column")}
  if (!"parameter" %in% names(data[[3]])) {warning("Input data is missing parameter column")}
  if (!"value" %in% names(data[[3]])) {warning("Input data is missing value column")}
  if (!"parameter" %in% names(data[[4]])) {warning("Input data is missing parameter column")}
  if (!"value" %in% names(data[[4]])) {warning("Input data is missing value column")}

  if (!missing(sample)) {
    itersubsample <- sample(unique(data[[1]]$iter), size=sample)
    data[[1]] <- data[[1]] |>
      dplyr::filter(iter %in% itersubsample)
  }


  mcmc_df_trace <- data[[1]]
  opt_df <- data[[2]]

  p <- ggplot2::ggplot(mcmc_df_trace) +
    ggplot2::geom_point(ggplot2::aes(x = iter, y = value, colour = chain), size = 0.5) +
    ggplot2::scale_colour_manual("Chain",values=colours) +
    ggplot2::labs(linetype = NULL) +
    ggplot2::facet_wrap(~parameter, scales = "free_y", labeller = ggplot2::label_parsed, ncol = ncol) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "bottom")+
    ggplot2::guides(colour = ggplot2::guide_legend(override.aes = list(size=2))) +
    ggplot2::xlab(xlab) +
    ggplot2::ylab(ylab)
  if (show_opt){
    p <- p +
      ggplot2::geom_hline(data = opt_df, ggplot2::aes(yintercept = value, linetype = "Optimised"), colour = "red")
  }

  if (length(unique(data[[1]]$chain))==1) {
    p <- p +
      ggplot2::guides(colour = "none")
  }


  return(p)
}
