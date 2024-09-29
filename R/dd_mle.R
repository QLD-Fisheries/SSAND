# Copyright 2024 Fisheries Queensland

# This file is part of SSAND.
# SSAND is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
# SSAND is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
# You should have received a copy of the GNU General Public License along with SSAND. If not, see <https://www.gnu.org/licenses/>.

#' Fake data to be used in DD_... prep functions.
#'
#' Fabricated data showing 6 scenarios of DDUST models run through
#' optim (maximum likelihood estimation) and tmbstan (Markov chain Monte Carlo) ...
#'
#' @format ## `dd_mle`
#' A list of 6 lists. Each lists contains 100 variables:
#' \describe{
#'   \item{annual_catch, annual_catch_sd}{Annual catch}
#'   \item{annual_recruitment, annual_recruitment_sd}{Annual recruitment}
#'   \item{B, B_sd}{Biomass}
#'   ...
#' }
"dd_mle"
