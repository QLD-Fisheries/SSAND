# Copyright 2024 Fisheries Queensland

# This file is part of SSAND.
# SSAND is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
# SSAND is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
# You should have received a copy of the GNU General Public License along with SSAND. If not, see <https://www.gnu.org/licenses/>.

#' Prepare data for a plot that illustrates the impact of catchability rescaling how the model perceives catch rates
#'
#' @param cont_dd_mle An output of DDUST::makedd_mle(report) for a model scenario that uses continuous catch rates for the entire assessment period
#' @param split_dd_mle An output of DDUST::makedd_mle(report) for a model scenario that uses fleets to represent changes in fishing behaviour (e.g. fishery closures)
#' @param offset_dd_mle An output of DDUST::makedd_mle(report) for a model scenario that uses uses offset continuous catch rates for the entire assessment period
#'
#' @return A data frame with variables Year (numerical), cpueadjust (numerical), scenario (character)
#' @export
#'
cpueqplot_prep_DD <- function(cont_dd_mle,
                              split_dd_mle,
                              offset_dd_mle = NULL) {

  warning("This plot prep is under construction. No sample data available to test yet.")

  # data <- cpueqplot_prep(cont_dd_mle = dd_mle[[2]], split_dd_mle = dd_mle[[1]])
  # data <- cpueqplot_prep(cont_dd_mle = dd_mle[[2]],
  #                        split_dd_mle = dd_mle[[1]],
  #                        offset_dd_mle = dd_mle[[3]])

  # if (missing(offset_dd_mle)) {
  #   scenario_list <- list(cont_dd_mle,split_dd_mle)
  #   scenario_name <- c("Continuous","Split catch rates")
  # } else {
  #   scenario_list <- list(cont_dd_mle,split_dd_mle,offset_dd_mle)
  #   scenario_name <- c("Continuous","Split catch rates","Offset catch rates")
  # }
  #
  # catchabilitylist <- list()
  # for (i in 1:length(scenario_name)) {
  #   q1 <- scenario_list[[i]]$q1
  #   q2 <- scenario_list[[i]]$q2
  #   log_q <- scenario_list[[i]]$log_q
  #   data <- data.frame(q = numeric(), month = numeric(), fleet = factor())
  #   for (j in 1:length(scenario_list[[i]]$log_q)){
  #     log_q <- scenario_list[[i]]$log_q[j]
  #     data <- rbind(data,data.frame(q = exp(log_q+q1*cos(2*pi*scenario_list[[i]]$data$m_seq/12)+q2*sin(2*pi*scenario_list[[i]]$data$m_seq/12)),
  #                                   month = 1:12,
  #                                   fleet = as.factor(j)))
  #   }
  #   data$label = scenario_name[i]
  #   catchabilitylist[[i]] <- data
  # }
  #
  # catchability <- do.call(rbind, catchabilitylist)
  #
  # catchability_annual <- catchability |>
  #   dplyr::group_by(label,fleet) |>
  #   dplyr::summarise(q = mean(q), .groups='drop') |>
  #   dplyr::filter(!is.na(q)) |>
  #   dplyr::mutate(fleet = as.factor(fleet))  |>
  #   dplyr::mutate(series = paste0(label,'_',fleet))
  #
  # plot_cpueq1list <- list()
  # for (i in 1:length(scenario_name)) {
  #   plot_cpueq1list[[i]] <- SSAND::cpuefitsplot_prep(scenario_list[[i]]) |>
  #     # Aggregate annually
  #     dplyr::mutate(Year = format(date, "%Y")) |>
  #     dplyr::group_by(Year, fleet) |> dplyr::summarise(obs = mean(obs, na.rm = TRUE), exp = mean(exp, na.rm = TRUE), .groups = 'drop') |>
  #     dplyr::mutate(scenario = scenario_name[i]) |>
  #     dplyr::mutate(series = paste0(scenario,'_',fleet)) |>
  #     dplyr::left_join(catchability_annual |> dplyr::select(series, q), by = dplyr::join_by(series))
  # }
  # plot_cpueq1 <- do.call(rbind, plot_cpueq1list)
  #
  # q_cont <- catchability_annual |>
  #   dplyr::mutate(cont_q = q[label=="Continuous"]) |>
  #   dplyr::select(scenario=label, cont_q)
  #
  # data <- plot_cpueq1 |>
  #   dplyr::left_join(q_cont |> unique(), by = dplyr::join_by(scenario)) |>
  #   dplyr::mutate(logq = log(q)) |>
  #   dplyr::mutate(B = log(obs) - log(q)) |>
  #   dplyr::mutate(cpueadjust = exp(B + log(cont_q))) |>
  #   dplyr::mutate(Year = as.numeric(Year)) |>
  #   dplyr::select(Year, cpueadjust, scenario)
  # return(data)
}
