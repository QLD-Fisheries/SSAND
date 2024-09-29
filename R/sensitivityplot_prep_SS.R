# Copyright 2024 Fisheries Queensland

# This file is part of SSAND.
# SSAND is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
# SSAND is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
# You should have received a copy of the GNU General Public License along with SSAND. If not, see <https://www.gnu.org/licenses/>.

#' Prepare Stock Synthesis data for sensitivityplot()
#'
#' It is recommended you use SSAND::extract_SS_parameters() to find the parameter names you'd like to include on the plot.
#'
#' @param ss_mle A list of outputs from r4ss::SS_output() with one element per scenario. Will automatically reformat as a list if a single r4ss::SS_output() output (i.e. one scenario) is entered.
#' @param ss_mcmc A list of outputs from r4ss::SSgetMCMC() with one element per scenario. Only needed if MCMC was used. Will automatically reformat as a list if a single r4ss::SSgetMCMC() output (i.e. one scenario) is entered.
#' @param scenarios A vector of scenarios to plot (numeric). Shows all scenarios if left blank. Can be overridden in the plotting function.
#' @param parameters A vector of parameters to include on plot (character). Use SSAND::extract_SS_parameters() to find a list of parameter names used.
#' @param show_B_ratio Set to TRUE to show a panel for final biomass ratio (logical).
#' @param show_B_abs Set to TRUE to show a panel for final absolute biomass (logical).
#' @param show_F_final Set to TRUE to show final annual fishing mortality (logical).
#' @param show_LL Set to TRUE to show negative log likelhood or objective function (logical).
#' @param show_MSY Set to TRUE, to show MSY (logical).
#' @param show_B_MSY Set to TRUE to show B_MSY (logical).
#' @param show_F_MSY Set to TRUE to show F_MSY (logical).
#'
#' @return A data frame with scenario (int), name (fac), value (num), ub (num), lb (num) and fixed (boolean).
#' @export
#'
#' @examples
#' parm <- extract_SS_parameters(ss_mle)[c(3:6),]
#' data <- sensitivityplot_prep_SS(ss_mle,
#'                                 ss_mcmc,
#'                                 show_MSY = TRUE,
#'                                 show_LL = TRUE,
#'                                 show_B_ratio = TRUE,
#'                                 parameters = parm)
#' sensitivityplot(data)
sensitivityplot_prep_SS <- function(ss_mle,
                                    ss_mcmc = NULL,
                                    scenarios = NULL,
                                    parameters = NULL,
                                    show_B_ratio = TRUE,
                                    show_B_abs = FALSE,
                                    show_F_final = FALSE,
                                    show_MSY = FALSE,
                                    show_B_MSY = FALSE,
                                    show_F_MSY = FALSE,
                                    show_LL = TRUE
) {

  if (missing(ss_mcmc)) {MCMC = FALSE} else {MCMC = TRUE}
  if (check_scenarios(ss_mle,"SS","MLE")=="single scenario"){ss_mle <- list(ss_mle); warning("Assuming you are entering a single scenario, not a list of scenarios. Wrap ss_mle input inside a list() to avoid this warning.")}
  if (MCMC && check_scenarios(ss_mcmc,"SS","MCMC")=="single scenario"){ss_mcmc <- list(ss_mcmc); warning("Assuming you are entering a single scenario, not a list of scenarios. Wrap ss_mcmc input inside a list() to avoid this warning.")}

  if (missing(scenarios)){scenarios <- 1:length(ss_mle)}


  if (!MCMC) {
    # Extract all parameters
    pars <- data.frame()
    for (i in 1:length(ss_mle)) {
      pars <- pars |>
        rbind(
          ss_mle[[i]]$parameters  |>
            dplyr::filter(!grepl("Main_RecrDev", Label)) |>
            dplyr::filter(!grepl("ForeRecr", Label)) |>
            dplyr::select(Num, Label, Value, Active_Cnt, Phase, Init, Parm_StDev) |>
            dplyr::mutate(scenario = i)
        )
    }
    rownames(pars) <- NULL

    pars <- pars |>
      dplyr::mutate(fixed = Phase<0,
                    name = Label,
                    value = Value,
                    ub = value+1.96*Parm_StDev,
                    lb = value-1.96*Parm_StDev) |>
      dplyr::select(scenario, name, value, ub, lb, fixed)

    # Filter if specific list of parameters is given
    if (!missing(parameters)) {
      pars <- pars |>
        dplyr::filter(name %in% parameters)
    }

    # Extract final relative biomass
    if (show_B_ratio) {
      b_ratio <- data.frame()
      for (i in 1:length(ss_mle)) {
        tmp <- ss_mle[[i]]$derived_quants |>
          dplyr::filter(grepl("Bratio", Label)) |>
          tidyr::separate(Label, c("label", "year"), sep="_") |>
          dplyr::mutate(year = as.numeric(year)) |>
          dplyr::mutate(label = paste0("B[ratio",year,"]")) |>
          dplyr::filter(year == ss_mle[[i]]$endyr) |>
          dplyr::mutate(fixed = FALSE,
                        name = label,
                        value = Value,
                        ub = Value + 1.96*StdDev,
                        lb = Value - 1.96*StdDev,
                        scenario = i) |>
          dplyr::select(scenario, name, value, ub, lb, fixed)
        b_ratio <- rbind(b_ratio, tmp)
      }
    }

    # Extract final absolute biomass
    if (show_B_abs) {
      b_abs <- data.frame()
      for (i in 1:length(ss_mle)) {
        tmp <- ss_mle[[i]]$derived_quants |>
          dplyr::filter(grepl("SSB", Label)) |>
          dplyr::filter(substr(Label,5,5) %in% c("1","2")) |>
          tidyr::separate(Label, c("label", "year"), sep="_") |>
          dplyr::mutate(year = as.numeric(year)) |>
          dplyr::mutate(label = paste0("B[absolute",year,"]")) |>
          dplyr::filter(year == ss_mle[[i]]$endyr) |>
          dplyr::mutate(fixed = FALSE,
                        name = label,
                        value = Value,
                        ub = Value + 1.96*StdDev,
                        lb = Value - 1.96*StdDev,
                        scenario = i) |>
          dplyr::select(scenario, name, value, ub, lb, fixed)
        b_abs <- rbind(b_abs, tmp)
      }
    }

    # Extract F final
    if (show_F_final) {
      F_final <- data.frame()
      for (i in 1:length(ss_mle)) {
        tmp <- ss_mle[[i]]$derived_quants |>
          dplyr::filter(grepl("F_", Label)) |>
          tidyr::separate(Label, c("label", "year"), sep="_") |>
          dplyr::filter(label == "F") |>
          dplyr::mutate(year = as.numeric(year)) |>
          dplyr::mutate(label = paste0("F[",year,"]")) |>
          dplyr::filter(year == ss_mle[[i]]$endyr) |>
          dplyr::mutate(fixed = FALSE,
                        name = label,
                        value = Value,
                        ub = Value + 1.96*StdDev,
                        lb = Value - 1.96*StdDev,
                        scenario = i) |>
          dplyr::select(scenario, name, value, ub, lb, fixed)
        F_final <- rbind(F_final, tmp)
      }
    }


    # Extract MSY
    if (show_MSY) {
      MSY <- data.frame()
      for (i in 1:length(ss_mle)) {
        tmp <- ss_mle[[i]]$derived_quants |>
          dplyr::filter(Label == "Dead_Catch_MSY") |>
          dplyr::mutate(fixed = FALSE,
                        name = "MSY",
                        value = Value,
                        ub = Value + 1.96*StdDev,
                        lb = Value - 1.96*StdDev,
                        scenario = i) |>
          dplyr::select(scenario, name, value, ub, lb, fixed)
        MSY <- rbind(MSY, tmp)
      }
    }

    # Extract B_MSY
    if (show_B_MSY) {
      B_MSY <- data.frame()
      for (i in 1:length(ss_mle)) {
        tmp <- ss_mle[[i]]$derived_quants |>
          dplyr::filter(Label == "B_MSY/SSB_unfished") |>
          dplyr::mutate(fixed = FALSE,
                        name = "B[MSY]",
                        value = Value,
                        ub = Value + 1.96*StdDev,
                        lb = Value - 1.96*StdDev,
                        scenario = i) |>
          dplyr::select(scenario, name, value, ub, lb, fixed)
        B_MSY <- rbind(B_MSY, tmp)
      }
    }

    # Extract F_MSY
    if (show_F_MSY) {
      F_MSY <- data.frame()
      for (i in 1:length(ss_mle)) {
        tmp <- ss_mle[[i]]$derived_quants |>
          dplyr::filter(Label == "annF_MSY") |>
          dplyr::mutate(fixed = FALSE,
                        name = "F[MSY]",
                        value = Value,
                        ub = Value + 1.96*StdDev,
                        lb = Value - 1.96*StdDev,
                        scenario = i) |>
          dplyr::select(scenario, name, value, ub, lb, fixed)
        F_MSY <- rbind(F_MSY, tmp)
      }
    }

    # Extract LL
    if (show_LL) {
      LL <- data.frame()
      for (i in 1:length(ss_mle)) {
        tmp <- data.frame(fixed = FALSE,
                          name = "LL",
                          value = ss_mle[[1]]$likelihoods_used$values[1],
                          ub = ss_mle[[1]]$likelihoods_used$values[1],
                          lb = ss_mle[[1]]$likelihoods_used$values[1],
                          scenario = i)
        LL <- rbind(LL,tmp)
      }
    }

    data <- pars
    if(show_B_ratio) {data <- data |> rbind(b_ratio)}
    if(show_B_abs) {data <- data |> rbind(b_abs)}
    if(show_F_final) {data <- data |> rbind(F_final)}
    if(show_MSY) {data <- data |> rbind(MSY)}
    if(show_B_MSY) {data <- data |> rbind(B_MSY)}
    if(show_F_MSY) {data <- data |> rbind(F_MSY)}
    if(show_LL) {data <- data |> rbind(LL)}

  }

  if (MCMC & missing(parameters)) {
    warning("As you have selected MCMC, you need to specify a list of parameters to include for the sensitivity plot. Use SSAND::sensitivityplot_prep_params_SS(ss_mcmc) to get a complete list of parameters estimated in your model.")
  }

  if (MCMC) {

    pars <- NULL
    derivedquants <- NULL

    for (i in 1:length(ss_mcmc)) {
      # estimated parameters mcmc_ss_mle
      estparams <- parameters[parameters %in% names(ss_mcmc[[i]])]

      # fixed parameters
      fixparams <- parameters[!(parameters %in% names(ss_mcmc[[i]]))]

      # get quantile for estimated parameters
      tmp <- apply(ss_mcmc[[i]] |>
                     dplyr::select(all_of(estparams)), 2,quantile,probs = c(0.025, 0.5, 0.975))

      if (dim(tmp)[2]==1) {
        # if only one estimated parameter, rownames_to_column will return just a number.
        estparams <- data.frame(name = dimnames(tmp)[[2]],
                                value = tmp[rownames(tmp) == "50%",],
                                lb = tmp[rownames(tmp) == "2.5%",],
                                ub = tmp[rownames(tmp) == "97.5%",]) |>
          # tibble::rownames_to_column(var="name") |>
          dplyr::mutate(fixed = FALSE,
                        scenario = i)

      } else {
        estparams <- data.frame(value = tmp[rownames(tmp) == "50%",],
                                lb = tmp[rownames(tmp) == "2.5%",],
                                ub = tmp[rownames(tmp) == "97.5%",]) |>
          tibble::rownames_to_column(var="name") |>
          dplyr::mutate(fixed = FALSE,
                        scenario = i)
      }

      # get fixed param values from MLE
      fixparams <- ss_mle[[i]]$parameters |>
        dplyr::filter(Label %in% fixparams) |>
        dplyr::select(name=Label, value=Value) |>
        dplyr::mutate(lb = value,
                      ub = value,
                      fixed = TRUE,
                      scenario = i) |>
        tibble::remove_rownames()

      pars <- rbind(pars, estparams, fixparams) |>
        dplyr::select(scenario, name, value, ub, lb, fixed)

      # objective functions and status indicators
      dq1 <- apply(ss_mcmc[[i]] |>
                     dplyr::select(c("Objective_function",
                                     "Dead_Catch_MSY",
                                     paste0("Bratio_", ss_mle[[i]]$endyr+1),
                                     paste0("SSB_", ss_mle[[i]]$endyr+1),
                                     "B_MSY/SSB_unfished",
                                     paste0("F_", ss_mle[[i]]$endyr),
                                     "annF_MSY")) , 2,quantile,probs = c(0.025, 0.5, 0.975))

      dq2 = data.frame(
        value = dq1[rownames(dq1) == "50%",],
        lb = dq1[rownames(dq1) == "2.5%",],
        ub = dq1[rownames(dq1) == "97.5%",]) |>
        tibble::rownames_to_column(var="name") |>
        dplyr::mutate(fixed = FALSE,
                      scenario = i)

      derivedquants <- rbind(derivedquants, dq2)
    }

    data <- pars
    if(show_B_ratio) {data <- data |> rbind(derivedquants |> dplyr::filter(grepl("Bratio_", name)))}
    if(show_B_abs)   {data <- data |> rbind(derivedquants |> dplyr::filter(grepl("SSB_20", name)))}
    if(show_F_final) {data <- data |> rbind(derivedquants |> dplyr::filter(grepl("F_20", name)))}
    if(show_MSY)     {data <- data |> rbind(derivedquants |> dplyr::filter(name=="Dead_Catch_MSY"))}
    if(show_B_MSY)   {data <- data |> rbind(derivedquants |> dplyr::filter(name=="B_MSY/SSB_unfished"))}
    if(show_F_MSY)   {data <- data |> rbind(derivedquants |> dplyr::filter(name=="annF_MSY"))}
    if(show_LL)      {data <- data |> rbind(derivedquants |> dplyr::filter(name=="Objective_function"))}

    # Rename SSB_ and Bratio_ to match MLE
    data <- data |>
      dplyr::mutate(name = ifelse(grepl("Bratio_",name), paste0("B[ratio",substr(name,8,11),"]"),name)) |>
      dplyr::mutate(name = ifelse(grepl("SSB_20",name), paste0("B[absolute",substr(name,5,8),"]"),name))


  }

  # Overwrite forbidden characters
  data$name <- gsub("%", "", data$name)
  data$name <- gsub("_", "", data$name)
  data$name <- gsub("\\(", "", data$name)
  data$name <- gsub(")", "", data$name)

  data <- data |>
    dplyr::filter(scenario %in% scenarios)

  rownames(data) <- NULL
  return(data)
}
