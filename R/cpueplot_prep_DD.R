# Copyright 2024 Fisheries Queensland

# This file is part of SSAND.
# SSAND is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
# SSAND is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
# You should have received a copy of the GNU General Public License along with SSAND. If not, see <https://www.gnu.org/licenses/>.

#' Prepare DDUST data for catch per unit effort (cpue) fit plot
#'
#' @param dd_mle A list of outputs from DDUST::makefullreport() with one list element per scenario. Will automatically reformat as a list if a single DDUST::makefullreport() output (i.e. one scenario) is entered
#' @param dd_mcmc A list of model fits from tmbstan::tmbstan() with one list element per scenario. Only needed if MCMC was used.
#' @param dd_sim A list of outputs from SSAND::simulate_DD() with one list element per scenario. Only required if MCMC was used. Will automatically reformat as a list if a single DDUST::makefullreport() output (i.e. one scenario) is entered
#' @param scenarios A vector of scenarios to plot (numeric). Shows all scenarios if left blank. Can be overridden in the plotting function.
#' @param sigma_I Set to TRUE to define error bars using sigma I (estimated error) rather than observed CPUE SD.
#' @param intervals A vector of credible interval values to be displayed on banded MCMC plot (numeric). For example, "0.9" denotes the 90% credible interval.
#'
#' @return A data frame with variables date, fleet, obs, exp, ub, lb, scenario
#' @export
#'
#' @examples
#' data <- cpueplot_prep_DD(dd_mle)
#' cpueplot(data)
cpueplot_prep_DD <- function(dd_mle,
                             dd_mcmc = NA,
                             dd_sim = NA,
                             scenarios = NULL,
                             intervals = c(0.2, 0.5, 0.7, 0.9, 0.95),
                             sigma_I = FALSE){

  if (missing(dd_mcmc)) {MCMC = FALSE} else {MCMC = TRUE}
  if (check_scenarios(dd_mle,"DD","MLE")=="single scenario"){dd_mle <- list(dd_mle)}
  if (MCMC && check_scenarios(dd_mcmc,"DD","MCMC")=="single scenario"){dd_mcmc <- list(dd_mcmc)}
  if (MCMC && check_scenarios(dd_sim,"DD","SIM")=="single scenario"){dd_sim <- list(dd_sim)}

  if (missing(scenarios)){scenarios <- 1:length(dd_mle)}

  if (!MCMC) {
    data.scenarios <- data.frame()

    for (scenario in scenarios) {
      numfleet <- nrow(dd_mle[[scenario]]$data$cpue)
      Nm <- dd_mle[[scenario]]$data$Number_months_per_timestep

      # data frame with all dates
      year_month_df <- data.frame(year = dd_mle[[scenario]]$data$first_year_catch, month = 1)
      year_month_df <- tidyr::complete(year_month_df,
                                       year = dd_mle[[scenario]]$data$first_year_catch:dd_mle[[scenario]]$data$last_year_catch,
                                       month = seq(1,12,Nm))

      data <- data.frame()


      for (i in 1:numfleet){
        if (dd_mle[[scenario]]$data$use_cpue_sd == 1) {
          sd <- dd_mle[[scenario]]$data$cpue_sd

          temp <- data.frame(year = sort(rep(dd_mle[[scenario]]$data$first_year_catch:dd_mle[[scenario]]$data$last_year_catch,12/Nm)),
                             month = seq(1,12,Nm),
                             fleet = i,
                             obs = dd_mle[[scenario]]$data$cpue[i,],
                             exp = matrix(dd_mle[[scenario]]$pred_cpue,nrow=numfleet)[i,],
                             sd = as.numeric(sd)) |>
            dplyr::mutate(ub = obs + 1.96*sd,
                   lb = obs - 1.96*sd) |>
            dplyr::select(-sd)


        } else {
          sd <- dd_mle[[scenario]]$sigmaI

          temp <- data.frame(year = sort(rep(dd_mle[[scenario]]$data$first_year_catch:dd_mle[[scenario]]$data$last_year_catch,12/Nm)),
                             month = seq(1,12,Nm),
                             fleet = i,
                             obs = dd_mle[[scenario]]$data$cpue[i,],
                             exp = matrix(dd_mle[[scenario]]$pred_cpue,nrow=numfleet)[i,],
                             ub = dd_mle[[scenario]]$data$cpue[i,] + 1.96*sd,
                             lb = dd_mle[[scenario]]$data$cpue[i,] - 1.96*sd)
        }

        # join all dates to fill missing months
        temp <- dplyr::right_join(temp, year_month_df, by = dplyr::join_by(year, month))
        temp <- dplyr::mutate(temp, date = as.Date(paste0('01/',month,'/',year), format = '%d/%m/%Y'),
                              obs = ifelse(obs == 0, NA, obs))
        temp <- dplyr::mutate(temp, ub = ifelse(is.na(obs), NA, ub), lb = ifelse(is.na(obs), NA, lb))
        # plotting must start and end with data (can't be empty) for each fleet - we need other NA's to stay in there
        minDate <- min(temp$date[!is.na(temp$obs)])
        maxDate <- max(temp$date[!is.na(temp$obs)])
        temp <- dplyr::filter(temp, date>=minDate, date<= maxDate)
        data <- rbind(data, temp)
      }
      data <- cbind(data, scenario=scenario)
      data.scenarios <- rbind(data.scenarios, data)

    }

    data <- dplyr::select(data.scenarios, c(date, fleet, obs, exp, ub, lb, scenario))

  } else {

    if (missing(dd_sim)){stop('Please provide a list of MCMC simulations: \n dd_sim <- SSAND::simulate_DD(dd_mle, dd_mcmc)')}

    data_all <- data.frame()

    for (scenario in scenarios){
      pos <- mcmc_median_position_DD(dd_mle, dd_mcmc, dd_sim)[scenario]

      numfleet <- nrow(dd_mle[[scenario]]$data$cpue)
      Nm <- dd_mle[[scenario]]$data$Number_months_per_timestep

      sim <- dd_sim[[scenario]]

      for (i in 1:numfleet){
        if (dd_mle[[scenario]]$data$use_cpue_sd == 1) {
          sd <- dd_mle[[scenario]]$data$cpue_sd

          all_mcmc <- data.frame()
          cpue_mcmc <- data.frame()

          for (rownum in 1:length(sim)) {
            temp <- data.frame(year = sort(rep(dd_mle[[scenario]]$data$first_year_catch:dd_mle[[scenario]]$data$last_year_catch,12/Nm)),
                               month = seq(1,12,Nm),
                               fleet = i,
                               obs = dd_mle[[scenario]]$data$cpue[i,],
                               exp = sim[[rownum]]$pred_cpue[i,],
                               sd = as.numeric(sd)) |>
              dplyr::mutate(ub = obs + 1.96*sd,
                            lb = obs - 1.96*sd) |>
              dplyr::select(-sd) |>
              dplyr::mutate(rownum = rownum)

            all_mcmc <- rbind(all_mcmc,temp)
          }

        } else {
          all_mcmc <- data.frame()
          cpue_mcmc <- data.frame()

          for (rownum in 1:length(sim)) {
            temp <- data.frame(year = sort(rep(dd_mle[[scenario]]$data$first_year_catch:dd_mle[[scenario]]$data$last_year_catch,12/Nm)),
                               month = seq(1,12,Nm),
                               fleet = i,
                               obs = dd_mle[[scenario]]$data$cpue[i,],
                               exp = sim[[rownum]]$pred_cpue[i,],
                               ub = dd_mle[[scenario]]$data$cpue[i,] + 1.96*dd_mle[[scenario]]$sigmaI,
                               lb = dd_mle[[scenario]]$data$cpue[i,] - 1.96*dd_mle[[scenario]]$sigmaI) |>
              dplyr::mutate(rownum = rownum, med="MCMC", interval=NA)

            all_mcmc <- rbind(all_mcmc,temp)
          }
        }

        # Find trajectory that produces the median final biomass
        median_trajectory <- all_mcmc |>
          dplyr::mutate(med = ifelse(rownum==pos,"trajectory","MCMC"),
                        rownum = ifelse(rownum==pos,0,rownum),
                        interval = NA)


        # Add median CPUE for each time step
        median_cpue <- all_mcmc |>
          dplyr::group_by(year, month) |>
          dplyr::summarise(exp = median(exp), .groups='drop') |>
          dplyr::mutate(obs = NA, ub = NA, lb = NA, fleet = i,rownum=0,med="median_cpue",interval=NA) |>
          dplyr::select(year,month,fleet,obs,exp,ub,lb,rownum,med,interval)

        # Banded plot prep
        intervals <- round(intervals,10)
        intervals_desc <- sort(intervals,decreasing = TRUE)

        banded_cpue <- data.frame()
        for (i in 1:length(intervals)) {
          temp <- all_mcmc |>
            dplyr::group_by(year, month) |>
            dplyr::summarise(lb = quantile(exp, na.rm = TRUE, probs = (1-intervals[i])/2),
                             ub = quantile(exp, na.rm = TRUE, probs = 1-(1-intervals[i])/2),
                             .groups='drop') |>
            dplyr::mutate(interval=intervals[i],exp = NA, obs = NA, fleet = i,rownum=0,med="band") |>
            dplyr::select(year,month,fleet,obs,exp,ub,lb,rownum,med,interval)

          banded_cpue <- rbind(banded_cpue,temp)
        }

        cpue_mcmc <- rbind(cpue_mcmc,all_mcmc,median_cpue,median_trajectory,banded_cpue)

      }

      # data frame with all dates
      year_month_df <- data.frame(year = dd_mle[[scenario]]$data$first_year_catch, month = 1)
      year_month_df <- tidyr::complete(year_month_df,
                                       year = dd_mle[[scenario]]$data$first_year_catch:dd_mle[[scenario]]$data$last_year_catch,
                                       month = seq(1,12,Nm))

      # join all dates to fill missing months
      temp <- cpue_mcmc |>
        dplyr::right_join(year_month_df, by = dplyr::join_by(year, month))
      temp <- temp |>
        dplyr::mutate(date = as.Date(paste0('01/',month,'/',year), format = '%d/%m/%Y'),
                      obs = ifelse(obs == 0, NA, obs))
      # plotting must start and end with data (can't be empty) for each fleet - we need other NA's to stay in there
      minDate <- min(temp$date[!is.na(temp$obs)])
      maxDate <- max(temp$date[!is.na(temp$obs)])
      temp <- temp |>
        dplyr::filter(date>=minDate, date<= maxDate)

      temp <- temp |>
        dplyr::mutate(scenario = scenario)

      data_all <- rbind(data_all, temp)
    }

    data <- data_all |> dplyr::mutate(scenario = as.factor(scenario))
  }
  rownames(data) <- NULL
  return(data)
}
