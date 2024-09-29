# Copyright 2024 Fisheries Queensland

# This file is part of SSAND.
# SSAND is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
# SSAND is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
# You should have received a copy of the GNU General Public License along with SSAND. If not, see <https://www.gnu.org/licenses/>.

#' Prepare DDUST data for maturity plots
#'
#' This plot requires manual specification of the data as it is not built into DDUST.
#' Prepares data for knife-edge maturity.
#'
#' @param x_max Maximum length or age to plot.
#' @param x_mat Length or age at which animals mature.
#' @param x_increment Increment for plotting maturity.
#' @param maturity_type Either "length" or "age", to define the x-axis.
#' @param sex Indicate which sex to plot. Enter 1 for female or 2 for male.
#'
#' @return A data frame with variables value, maturity, sex, scenario and type (length or age)
#' @export
#'
#' @examples
#' data <- maturityplot_prep_DD(x_max=10,x_mat=2)
#' maturityplot(data)
maturityplot_prep_DD <- function(x_max = NULL,
                                 x_mat = NULL,
                                 x_increment = 1,
                                 maturity_type = "length",
                                 sex = 1) {
  if (missing(x_max)) {stop("Please enter a maximum value for the x-axis (either length or age).")}
  if (missing(x_mat)) {stop("Please enter a value for maturity (either length or age).")}

  value <- seq(0,x_max, x_increment)

  data <- data.frame(value = value,
                     sex = sex,
                     scenario = 1,
                     type = maturity_type) |>
    dplyr::mutate(maturity = ifelse(value<x_mat,0,1))

  return(data)
}
