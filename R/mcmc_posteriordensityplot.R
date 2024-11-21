# Copyright 2024 Fisheries Queensland

# This file is part of SSAND.
# SSAND is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
# SSAND is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
# You should have received a copy of the GNU General Public License along with SSAND. If not, see <https://www.gnu.org/licenses/>.

#' Posterior density plot for MCMC run
#'
#' @param data Output from mcmc_posterior_densityplot_prep(). List of data frames with columns parameter (character), value (numeric), year (numeric), chain (numeric).
#' @param add_lines Lines to add to density plots. e.g. c('optimised', 'median parameter', 'median trajectory')
#' @param ncol Number of columns for facet_wrap()
#' @param xlab Label for x-axis (character). Default is "".
#' @param ylab Label for y-axis (character). Default is "Density".
#' @param parameter_labels Vector of customised parameter labels for facet wrap, e.g. expression(xi)
#' @param xmax Named vector of maximum x-axis limit, e.g. c('R\\[0\\]' = 50).
#'
#' @return Posterior density plot
#' @export
#'
#' @examples
#' parameters <- extract_SS_parameters(ss_mcmc)[c(2:10,449),]
#' data <- mcmc_posteriordensityplot_prep_SS(ss_mle, ss_mcmc, scenario = 1, parameters)
#' mcmc_posteriordensityplot(data)
mcmc_posteriordensityplot <- function(data,
                                      add_lines = c('optimised', 'median parameter', 'median trajectory'),
                                      xlab = "",
                                      ylab = "Density",
                                      ncol = 3,
                                      parameter_labels = NULL,
                                      xmax = NULL) {
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


  mcmc_df_trace <- data[[1]]
  opt_df <- data[[2]]
  med_par_df <- data[[3]]
  med_traj_df <- data[[4]]


  if (!missing(parameter_labels)) {
    label_lookup <- data.frame(parameter = parameters,
                               new_label = parameter_labels)

    mcmc_df_trace <- mcmc_df_trace |>
      dplyr::left_join(label_lookup, by = "parameter") |>
      dplyr::mutate(parameter = new_label) |>
      dplyr::select(-new_label)

    opt_df <- opt_df |>
      dplyr::left_join(label_lookup, by = "parameter") |>
      dplyr::mutate(parameter = new_label) |>
      dplyr::select(-new_label)

    med_par_df <- med_par_df |>
      dplyr::left_join(label_lookup, by = "parameter") |>
      dplyr::mutate(parameter = new_label) |>
      dplyr::select(-new_label)

    med_traj_df <- med_traj_df |>
      dplyr::left_join(label_lookup, by = "parameter") |>
      dplyr::mutate(parameter = new_label) |>
      dplyr::select(-new_label)
  }

  if (!missing(xmax)){
    for (par in names(xmax)){
      mcmc_df_trace <- mcmc_df_trace |> filter(!(parameter == par & value >= xmax[par]))
    }
  }

  p <- ggplot2::ggplot(mcmc_df_trace) +
    ggplot2::geom_density(ggplot2::aes(x = value, fill = chain), linewidth = 0.5, alpha = 0.5) +
    ggplot2::labs(linetype = NULL) +
    ggplot2::facet_wrap(~parameter, scales = "free", labeller = ggplot2::label_parsed, ncol = ncol) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "bottom")+
    ggplot2::xlab(xlab) +
    ggplot2::ylab(ylab)

  if ('optimised' %in% add_lines){
    p <- p +
      ggplot2::geom_vline(data = opt_df, ggplot2::aes(xintercept = value, linetype = "Optimised"))
  }
  if ('median parameter' %in% add_lines){
    p <- p +
      ggplot2::geom_vline(data = med_par_df, ggplot2::aes(xintercept = value, linetype = "Median"))
  }
  if ('median trajectory' %in% add_lines){
    p <- p +
      ggplot2::geom_vline(data = med_traj_df, ggplot2::aes(xintercept = value, linetype = "Median Trajectory"))
  }

  if (length(unique(data[[1]]$chain))==1) {
    p <- p +
      ggplot2::guides(fill = "none")
  }

  return(p)
}


