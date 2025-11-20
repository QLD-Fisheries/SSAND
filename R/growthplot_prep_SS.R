# Copyright 2025 Fisheries Queensland

# This file is part of SSAND.
# SSAND is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
# SSAND is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
# You should have received a copy of the GNU General Public License along with SSAND. If not, see <https://www.gnu.org/licenses/>.

#' Prepare Stock  Synthesis data for growth plot
#'
#' For MCMC:
#'  - Assumes if growth parameters are fixed in first scenario entered, they are fixed for all, and takes fixed values from first scenario listed.
#'  - Assumes age bins are consistent across scenarios
#'  - Assumes A1 and A2 are consistent across all scenarios
#'  - Assumes von Bertalanffy growth option selected
#'
#' @param ss_mle A list of outputs from r4ss::SS_output() with one element per scenario. Will automatically reformat as a list if a single r4ss::SS_output() output (i.e. one scenario) is entered.
#' @param ss_mcmc A list of outputs from r4ss::SSgetMCMC() with one element per scenario. Only needed if MCMC was used. Will automatically reformat as a list if a single r4ss::SSgetMCMC() output (i.e. one scenario) is entered.
#' @param scenarios A vector of scenarios to plot (numeric). Shows all scenarios if left blank. Can be overridden in the plotting function.
#'
#' @return A data frame with variables age (numeric), value (numeric), upper (numeric), and lower (numeric) and Sex (integer)
#' @export
#'
#' @importFrom stats "qlnorm" "qnorm"
#'
#' @examples
#' data <- growthplot_prep_SS(ss_mle)
#' growthplot(data)
#'
#' \dontrun{
#' # MCMC
#' data <- growthplot_prep_SS(ss_mle, ss_mcmc, scenarios = 1)
#' growthplot(data)
#'
#' # Ensemble model
#' ss_mcmc_ens <- mcmc_ensemble_SS(ss_mcmc)
#' data <- growthplot_prep_SS(ss_mle, ss_mcmc_ens)
#' growthplot(data)
#' }
growthplot_prep_SS <- function(ss_mle, ss_mcmc = NULL, scenarios = NULL) {

  if(missing(ss_mcmc)) {
    MCMC <- FALSE
  } else {
    MCMC <- TRUE
  }
  if(check_scenarios(ss_mle, "SS", "MLE") == "single scenario") {
    ss_mle <- list(ss_mle)
  }
  if(MCMC && check_scenarios(ss_mcmc, "SS", "MCMC") == "single scenario") {
    ss_mcmc <- list(ss_mcmc)
  }

  if(missing(scenarios)) {
    if(!MCMC) {
      scenarios <- 1:length(ss_mle)
    } else {
      scenarios <- 1:length(ss_mcmc)
    }
  }

  if(!MCMC) {

    data <- data.frame()
    for(scenario in scenarios) {
      seas    <- 1
      morphs  <- ss_mle[[scenario]]$mainmorphs
      growdat <- ss_mle[[scenario]]$endgrowth[ss_mle[[scenario]]$endgrowth$Seas == seas, ] |>
        dplyr::rename(Sd_Size = SD_Beg)
      if(ss_mle[[scenario]]$growthCVtype == "logSD=f(A)") {
        growdat$upper <- qlnorm(0.975, meanlog = log(growdat$Len_Beg), sdlog = growdat$Sd_Size)
        growdat$lower <- qlnorm(0.025, meanlog = log(growdat$Len_Beg), sdlog = growdat$Sd_Size)
      } else {
        growdat$upper <- qnorm(0.975, mean = growdat$Len_Beg, sd = growdat$Sd_Size)
        growdat$lower <- qnorm(0.025, mean = growdat$Len_Beg, sd = growdat$Sd_Size)
      }
      tmp <- growdat |>
        dplyr::select(Age_Beg, Len_Beg, upper, lower, Sex) |>
        dplyr::select(age = Age_Beg, value = Len_Beg, lower, upper, sex = Sex) |>
        dplyr::mutate(scenario = scenario)

      data <- rbind(data, tmp)
    }

  } else {

    data_all <- data.frame()

    growthCVtype <- c()
    for(i in 1:length(scenarios)) {
      growthCVtype[i] <- ss_mle[[scenarios[i]]]$growthCVtype
    }
    if(!length(unique(growthCVtype)) == 1) {
      stop("You have used different growthCVtypes in your SS models. This plot won't work as it is.")
    }

    growthCVtype <- substr(growthCVtype[1], 1, 2)  # CV or SD

    ngpatterns <- c()
    for(i in 1:length(scenarios)) {
      ngpatterns[i] <- ss_mle[[scenarios[i]]]$ngpatterns
    }
    if(!length(unique(ngpatterns)) == 1) {
      warning("Functionality not yet build for multiple growth patterns.")
    }

    for(scenario in scenarios) {
      nsexes <- ss_mle[[scenario]]$nsexes

      # Check which parameters were estimated
      # If any of the key ones are missing, grab them from the first ss_mle
      strings_to_include <- c(
        "L_at_Amin_Fem_GP_1", "L_at_Amax_Fem_GP_1", "VonBert_K_Fem_GP_1", "CV_young_Fem_GP_1",  "CV_old_Fem_GP_1",   "SD_young_Fem_GP_1",
        "SD_old_Fem_GP_1",    "L_at_Amin_Mal_GP_1", "L_at_Amax_Mal_GP_1", "VonBert_K_Mal_GP_1", "CV_young_Mal_GP_1", "CV_old_Mal_GP_1",
        "SD_young_Mal_GP_1",  "SD_old_Mal_GP_1"
      )

      tmp <- ss_mle[[scenario]]$parameters |>
        dplyr::filter(grepl(paste(strings_to_include, collapse = "|"), Label))

      fixed_parameters <- dplyr::filter(tmp, Phase < 0)
      ested_parameters <- dplyr::filter(tmp, Phase > 0)

      ss_mcmc_growth <- ss_mcmc[[scenario]] |>
        dplyr::select(ested_parameters$Label)

      # Append fixed parameters
      if(nrow(fixed_parameters) > 0) {
        fixed_parameter_component <- fixed_parameters |>
          dplyr::select(Label, Value) |>
          tidyr::pivot_wider(names_from = Label, values_from = Value) |>
          tidyr::uncount(nrow(ss_mcmc[[scenario]]))
        ss_mcmc_growth <- cbind(ss_mcmc_growth, fixed_parameter_component)
      }

      # Reorder
      existing_columns <- strings_to_include[strings_to_include %in% names(ss_mcmc_growth)]  # Accounts for CV/SD variation
      ss_mcmc_growth   <- ss_mcmc_growth[, existing_columns]

      # Extract vector of ages for x-axis
      age_bins <- ss_mle[[scenario]]$age_error_mean$age
      age_bins_matrix <- matrix(age_bins, nrow = nrow(ss_mcmc_growth), ncol = length(age_bins), byrow = TRUE)

      # Establish growth function (von Bert)
      growth_fun <- function(age_bins, L1, L2, K, A1, A2) {
        # Page 95 of SS3.30.22 manual
        # L1 is L_at_Amin, L2 is L_at_Amax, K is VonBert_K
        # A1 and A2 come from control file
        Linf <- L1 + (L2 - L1) / (1 - exp(-1 * K * (A2 - A1)))
        Lt   <- Linf + (L1 - Linf) * exp(-1 * K * (age_bins - A1))
        return(Lt)
      }

      # Calculate growth for females
      growth_fem <- growth_fun(
        age_bins_matrix,
        L1 = ss_mcmc_growth$L_at_Amin_Fem_GP_1,
        L2 = ss_mcmc_growth$L_at_Amax_Fem_GP_1,
        K  = ss_mcmc_growth$VonBert_K_Fem_GP_1,
        A1 = ss_mle[[scenario]]$Growth_Parameters$A1[1],  # From control file
        A2 = ss_mle[[scenario]]$Growth_Parameters$A2      # From control file
      )

      # Calculate growth for males if two-sex model
      if(nsexes == 2) {
        growth_mal <- growth_fun(
          age_bins_matrix,
          L1 = ss_mcmc_growth$L_at_Amin_Mal_GP_1,
          L2 = ss_mcmc_growth$L_at_Amax_Mal_GP_1,
          K  = ss_mcmc_growth$VonBert_K_Mal_GP_1,
          A1 = ss_mle[[scenario]]$Growth_Parameters$A1[2],  # From control file
          A2 = ss_mle[[scenario]]$Growth_Parameters$A2[2]   # From control file
        )
      }

      # Calculate quantiles for growth
      quants         <- c(0.025, 0.50, 0.975)
      growth_fem_dat <- apply(growth_fem, 2, quantile, probs = quants, na.rm = TRUE)
      if(nsexes == 2) {
        growth_mal_dat <- apply(growth_mal, 2, quantile, probs = quants, na.rm = TRUE)
      }

      data_fem <- data.frame(
        age      = age_bins,
        value    = growth_fem_dat[rownames(growth_fem_dat) == "50%",   ],
        lower    = growth_fem_dat[rownames(growth_fem_dat) == "2.5%",  ],
        upper    = growth_fem_dat[rownames(growth_fem_dat) == "97.5%", ],
        sex      = 1,
        scenario = "Ensemble"
      )

      if(nsexes == 2) {
        data_mal <- data.frame(
          age      = age_bins,
          value    = growth_mal_dat[rownames(growth_mal_dat) == "50%",   ],
          lower    = growth_mal_dat[rownames(growth_mal_dat) == "2.5%",  ],
          upper    = growth_mal_dat[rownames(growth_mal_dat) == "97.5%", ],
          sex      = 2,
          scenario = "Ensemble"
        )
      }

      # Variance
      sigma_var <- apply(ss_mcmc_growth, 2, quantile, probs = quants, na.rm = TRUE)

      sigma_fun <- function(
        data,
        sigma_var,
        A1 = ss_mle[[scenario]]$Growth_Parameters$A1,  # From control file
        A2 = ss_mle[[scenario]]$Growth_Parameters$A2   # From control file
      ) {
        data$CV_lower  <- NA
        data$CV_middle <- NA
        data$CV_upper  <- NA

        L1_lower  <- sigma_var[1, 1]
        L1_middle <- sigma_var[2, 1]
        L1_upper  <- sigma_var[3, 1]

        L2_lower  <- sigma_var[1, 2]
        L2_middle <- sigma_var[2, 2]
        L2_upper  <- sigma_var[3, 2]

        CV_young_lower  <- sigma_var[1, 4]
        CV_young_middle <- sigma_var[2, 4]
        CV_young_upper  <- sigma_var[3, 4]

        CV_old_lower  <- sigma_var[1, 5]
        CV_old_middle <- sigma_var[2, 5]
        CV_old_upper  <- sigma_var[3, 5]

        for(i in 1:length(data$age)) {
          if(data$age[i] <= A1) {
            data$CV_lower[i]  <- CV_young_lower
            data$CV_middle[i] <- CV_young_middle
            data$CV_upper[i]  <- CV_young_upper
          }

          if(A1 < data$age[i] & data$age[i] < A2) {
            data$CV_lower[i]  <- CV_young_lower  + (data$value[i] - L1_lower)  / (L2_lower  - L1_lower)  * (CV_old_lower  - CV_young_lower)
            data$CV_middle[i] <- CV_young_middle + (data$value[i] - L1_middle) / (L2_middle - L1_middle) * (CV_old_middle - CV_young_middle)
            data$CV_upper[i]  <- CV_young_upper  + (data$value[i] - L1_upper)  / (L2_upper  - L1_upper)  * (CV_old_upper  - CV_young_upper)
          }

          if(data$age[i] >= A2) {
            data$CV_lower[i]  <- CV_old_lower
            data$CV_middle[i] <- CV_old_middle
            data$CV_upper[i]  <- CV_old_upper
          }
        }
        return(data)
      }

      sigma_var_fem <- dplyr::select(data.frame(sigma_var), dplyr::contains("Fem"))
      data_fem <- sigma_fun(
        data_fem,
        sigma_var_fem,
        A1 = ss_mle[[scenario]]$Growth_Parameters$A1[1],  # From control file
        A2 = ss_mle[[scenario]]$Growth_Parameters$A2[1]   # From control file
      )

      if(nsexes == 2) {
        sigma_var_mal <- dplyr::select(data.frame(sigma_var), dplyr::contains("Mal"))
        data_mal <- sigma_fun(
          data_mal,
          sigma_var_fem,
          A1 = ss_mle[[scenario]]$Growth_Parameters$A1[2],  # From control file
          A2 = ss_mle[[scenario]]$Growth_Parameters$A2[2]   # From control file
        )
      }

      if(nsexes == 1) {
        data <- data_fem
      }
      if(nsexes == 2) {
        data <- rbind(data_fem, data_mal)
      }
      data$growthCVtype <- growthCVtype

      data$scenario <- scenario
      data_all <- rbind(data_all, data)
    }
    data <- data_all

  }

  rownames(data) <- NULL
  return(data)
}
