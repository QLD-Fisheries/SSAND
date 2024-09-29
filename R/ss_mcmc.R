# Copyright 2024 Fisheries Queensland

# This file is part of SSAND.
# SSAND is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
# SSAND is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
# You should have received a copy of the GNU General Public License along with SSAND. If not, see <https://www.gnu.org/licenses/>.

#' Fake data to be used in ..._SS prep functions.
#'
#' Fabricated data showing 6 scenarios of Stock Synthesis models run through
#' Stock Synthesis (SS_3.30.20) (maximum likelihood estimation) and r4ss::SS_output().
#'
#' @format ## `ss_mcmc`
#' A list of 6 Stock Synthesis MCMC output objects.
#' Each element of the list is a data frame where rows are saved iterations of the MCMC and
#' columns are parameter estimates and derived quantities.
"ss_mcmc"
