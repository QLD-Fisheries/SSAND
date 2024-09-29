# Copyright 2024 Fisheries Queensland

# This file is part of SSAND.
# SSAND is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
# SSAND is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
# You should have received a copy of the GNU General Public License along with SSAND. If not, see <https://www.gnu.org/licenses/>.

#' Prepare Stock  Synthesis data for selectivity plot
#'
#' @param ss_mle A list of outputs from r4ss::SS_output() with one element per scenario. Will automatically reformat as a list if a single r4ss::SS_output() output (i.e. one scenario) is entered.
#' @param ss_mcmc_ens Output of mcmc_ensemble_SS(). A list containing one dataframe of all specified ss_mcmc objects combined, limited to columns that are common between ss_mcmc objects, duplicated as per the specified weighting.
#' An extra column "ensemble" is appended, for data checks in other functions.
#' @param scenarios A vector of scenarios to plot (numeric). Shows all scenarios if left blank. Can be overridden in the plotting function.
#' @param fleet A numeric representing the fleet to be included in MCMC data prep.
#'
#' @return A data frame with variables fleet (factor), year (int), sex (int), value (num), type (factor), selectivity (num)
#' @export
#'
#' @examples
#' data <- selectivityplot_prep_SS(ss_mle)
#' selectivityplot(data)
#'
#' # Ensemble model
#' ss_mcmc_ens <- mcmc_ensemble_SS(ss_mcmc)
#' data <- selectivityplot_prep_SS(ss_mle,ss_mcmc_ens,fleet = 1)
#' selectivityplot(data)
selectivityplot_prep_SS <- function(ss_mle,
                                    ss_mcmc_ens = NULL,
                                    scenarios,
                                    fleet = NULL) {

  if (missing(ss_mcmc_ens)) {MCMC = FALSE} else {MCMC = TRUE}
  if (check_scenarios(ss_mle,"SS","MLE")=="single scenario"){ss_mle <- list(ss_mle); warning("Assuming you are entering a single scenario, not a list of scenarios. Wrap ss_mle input inside a list() to avoid this warning.")}
  if (MCMC && check_scenarios(ss_mcmc_ens,"SS","MCMC")=="single scenario"){ss_mcmc_ens <- list(ss_mcmc_ens); warning("Assuming you are entering a single scenario, not a list of scenarios. Wrap ss_mcmc input inside a list() to avoid this warning.")}


  if (missing(scenarios)){scenarios <- 1:length(ss_mle)}

  if (!MCMC) {
    data <- data.frame()
    for (scenario in scenarios) {
      dataL <- ss_mle[[scenario]]$sizeselex |>
        dplyr::filter(Yr < ss_mle[[scenario]]$endyr+1)  |>
        dplyr::mutate(endyear = ss_mle[[scenario]]$endyr)  |>
        tidyr::pivot_longer(6:ncol(ss_mle[[scenario]]$sizeselex), names_to = "Length", values_to = "Selectivity") |>
        dplyr::select(-Label) |>
        tidyr::pivot_wider(names_from = Factor, values_from = Selectivity) |>
        dplyr::mutate (Disc = Lsel*(1-Ret)) |>
        tidyr::pivot_longer(Lsel:Disc, names_to = "Type", values_to = "Selectivity") |>
        dplyr::filter(Type != "Keep") |>
        dplyr::mutate(Type = dplyr::recode(Type, Lsel = "Selectivity (length)", Disc = "Discard", Mort = "Discard mortality", Ret = "Retention", Dead = "Dead" )) |>
        dplyr::mutate(Type = factor(Type, levels = c("Selectivity (length)", "Retention", "Discard mortality", "Discard", "Dead")),
                      Fleet=as.factor(Fleet),
                      Length=as.numeric(Length)) |>
        dplyr::select(fleet = Fleet, year = Yr, sex = Sex, value = Length, type = Type, selectivity = Selectivity, endyear)


      dataA <- ss_mle[[scenario]]$ageselex |>
        dplyr::filter(Yr < ss_mle[[scenario]]$endyr+1) |>
        dplyr::mutate(endyear = ss_mle[[scenario]]$endyr)  |>
        tidyr::pivot_longer(8:ncol(ss_mle[[scenario]]$ageselex), names_to = "Age", values_to = "Selectivity") |>
        dplyr::select(-Label) |>
        tidyr::pivot_wider(names_from = Factor, values_from = Selectivity) |>
        tidyr::pivot_longer(Asel:bodywt, names_to = "Type", values_to = "Selectivity") |>
        dplyr::filter(Type == "Asel")  |>
        dplyr::mutate(Type = dplyr::recode(Type, "Asel" = "Selectivity (age)")) |>
        dplyr::select(fleet = Fleet, year = Yr, sex = Sex, value = Age, type = Type, selectivity = Selectivity, endyear)


      tmp <- rbind(dataL, dataA) |>
        dplyr::mutate(sex = as.factor(sex),
                      scenario = as.factor(scenario),
                      value = as.numeric(value),
                      fleet = as.numeric(fleet))
      data <- rbind(data, tmp)
    }
  }


  if (MCMC) {
    if (missing(fleet)) {warning("Please specify fleet number.")}
    if (length(fleet)>1) {warning("Please only specify a single fleet.")}

    if (missing(ss_mcmc_ens)) {ss_mcmc_ens <- mcmc_ensemble_SS(ss_mcmc,scenarios)}
    ss_mcmc_ens <- ss_mcmc_ens$ensemble

    # Check which parameters were estimated
    # If any of the key ones are missing, grab them from the first ss_mle
    strings_to_include <- c("inflection","95%width")

    tmp <-  ss_mle[[scenarios[1]]]$parameters |>
      dplyr::filter(grepl(paste(strings_to_include, collapse = "|"),Label)) |>
      dplyr::filter(grepl(fleet,Label))

    fixed_parameters <- tmp |> dplyr::filter(Phase<0)
    ested_parameters <- tmp |> dplyr::filter(Phase>0)

    ss_mcmc_selectivity <- ss_mcmc_ens |>
      dplyr::select(ested_parameters$Label)

    # Append fixed parameters
    if (nrow(fixed_parameters)>0) {
      ss_mcmc_selectivity <- ss_mcmc_selectivity |>
        cbind(fixed_parameters |>
                dplyr::select(Label,Value) |>
                tidyr::pivot_wider(names_from = Label, values_from = Value) |>
                tidyr::uncount(nrow(ss_mcmc_ens)))
    }

    ss_mcmc_selectivity <- ss_mcmc_selectivity |>
      dplyr::rename(l50 = matches("inflection"),
                    lwidth = matches("95%width"))

    # Extract vector of lengths for x-axis
    lbins <- ss_mle[[scenarios[1]]]$biology$Len_lo
    lbinsmat <- matrix(lbins,
                       nrow=nrow(ss_mcmc_selectivity),
                       ncol=length(lbins),
                       byrow=TRUE)

    selfun <- function(lbins, a , b) {
      sel <- (1+exp(-log(19)*(lbins - a)/b))^(-1)
      return(sel)
    }

    sel <- selfun(lbinsmat,
                  a = ss_mcmc_selectivity$l50,
                  b = ss_mcmc_selectivity$lwidth)
    quants <- c(0.025,0.50, 0.975)
    seldat <- apply(sel , 2 , quantile , probs = quants , na.rm = TRUE )

    data <- data.frame(
      fleet = 1,
      year = NA,
      sex = 1,
      value = lbins,
      type = "Selectivity (length)",
      selectivity = seldat[rownames(seldat) == "50%",],
      lb = seldat[rownames(seldat) == "2.5%",],
      ub = seldat[rownames(seldat) == "97.5%",],
      scenario = "Ensemble"
    )
  }
  return(data)
}


