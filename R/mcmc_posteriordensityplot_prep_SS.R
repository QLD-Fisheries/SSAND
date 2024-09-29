# Copyright 2024 Fisheries Queensland

# This file is part of SSAND.
# SSAND is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
# SSAND is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
# You should have received a copy of the GNU General Public License along with SSAND. If not, see <https://www.gnu.org/licenses/>.

#' Posterior Stock Synthesis density plot for MCMC run
#'
#' @param ss_mle A list of outputs from r4ss::SS_output() with one element per scenario. Will automatically reformat as a list if a single r4ss::SS_output() output (i.e. one scenario) is entered.
#' @param ss_mcmc A list of outputs from r4ss::SSgetMCMC() with one element per scenario. Only needed if MCMC was used. Will automatically reformat as a list if a single r4ss::SSgetMCMC() output (i.e. one scenario) is entered.
#' @param scenario A single number representing the scenario to plot (numeric). Shows first scenario if left blank. Can be overridden in the plotting function.
#' @param parameters A vector of parameter to include on plot. Hint: use SSAND::extract_SS_parameters() to find a list of options.
#' @param likelihoods A vector to specify additional likelihoods to include on plot
#' @param show_objective_function Set to TRUE to include objective function on posterior density plot.
#'
#' @return data to produce density plot, trace plot
#' @export
#'
#' @examples
#' parameters <- extract_SS_parameters(ss_mcmc)[c(2:10,449),]
#' data <- mcmc_posteriordensityplot_prep_SS(ss_mle, ss_mcmc, scenario = 1, parameters)
#' mcmc_posteriordensityplot(data)
mcmc_posteriordensityplot_prep_SS <- function(ss_mle,
                                              ss_mcmc,
                                              scenario = NULL,
                                              parameters,
                                              likelihoods = c("TOTAL"),
                                              show_objective_function = FALSE
) {

  if (check_scenarios(ss_mle,"SS","MLE")=="single scenario"){ss_mle <- list(ss_mle)}
  if (check_scenarios(ss_mcmc,"SS","MCMC")=="single scenario"){ss_mcmc <- list(ss_mcmc)}

    if (length(scenario)> 1) {warning("Please enter only one scenario.")}

  pos <- mcmc_median_position_SS(ss_mle,ss_mcmc)[scenario]

  ss_mle <- ss_mle[[scenario]]
  ss_mcmc <- ss_mcmc[[scenario]]

  if (show_objective_function) {parameters <- unique(c(parameters,"Objective_function"))}

  data <- list()

  if (!"chain" %in% names(ss_mcmc)) {ss_mcmc <- ss_mcmc |> dplyr::mutate(chain=1)}

  mcmc_df_trace <- ss_mcmc |>
    dplyr::select(dplyr::all_of(parameters), chain) |>
    dplyr::mutate(chain = as.factor(chain), # assign chain number (1 chain)
                  iter = 1:nrow(ss_mcmc))  |>
    tidyr::pivot_longer(!c('iter','chain'),names_to = "label", values_to = "value")

  # median of each parameter
  med_par_df <- mcmc_df_trace |>
    dplyr::group_by(label) |>
    dplyr::summarise(value=median(value))

  tmpdata1 <-  ss_mcmc |> dplyr::select(dplyr::contains("Bratio_"))



  med_traj_df <- ss_mcmc[pos, ] |>
    dplyr::select(dplyr::contains(parameters)) |>
    tidyr::pivot_longer(dplyr::everything(), names_to = "label", values_to = "value")

  # MLE Optimum values
  tab1 <- ss_mle$parameters |>
    dplyr::filter(Label %in% parameters) |>
    dplyr::select(label=Label, value = Value)

  tab2 <- ss_mle$likelihoods_used |>
    tibble::rownames_to_column(var="label") |>
    dplyr::filter(label %in% likelihoods) |>
    dplyr::rename(value = values) |>
    dplyr::mutate(label = dplyr::recode(label,
                                        "TOTAL" = "Objective_function")) |>
    dplyr::select(label, value)

  # Final B ratio
  tab3 <-  ss_mle$derived_quants |>
    dplyr::filter(Label %in% parameters) |>
    dplyr::select(label=Label, value=Value)

  if (show_objective_function) {
    opt_df  <- rbind(tab1, tab2, tab3)
  } else {
    opt_df  <- rbind(tab1, tab3)
  }

  # make label as factor
  fctlevel <- unique(mcmc_df_trace$label)

  mcmc_df_trace <- mcmc_df_trace |>
    dplyr::mutate(label = factor(label, levels = fctlevel)) |>
    dplyr::select(iter, chain, parameter = label, value) |>
    dplyr::mutate(parameter = stringr::str_replace_all(parameter,"%","_"))

  opt_df <- opt_df |>
    dplyr::mutate(label = factor(label, levels = fctlevel)) |>
    dplyr::select(parameter = label, value) |>
    dplyr::mutate(parameter = stringr::str_replace_all(parameter,"%","_"))

  med_par_df <- med_par_df |>
    dplyr::mutate(label = factor(label, levels = fctlevel)) |>
    dplyr::select(parameter = label, value) |>
    dplyr::mutate(parameter = stringr::str_replace_all(parameter,"%","_"))

  med_traj_df <- med_traj_df |>
    dplyr::mutate(label = factor(label, levels = fctlevel)) |>
    dplyr::select(parameter = label, value) |>
    dplyr::mutate(parameter = stringr::str_replace_all(parameter,"%","_"))

  rownames(mcmc_df_trace) <- NULL
  rownames(opt_df) <- NULL
  rownames(med_par_df) <- NULL
  rownames(med_traj_df) <- NULL
  data <- list(mcmc_df_trace,opt_df,med_par_df,med_traj_df)

  return(data)
}
