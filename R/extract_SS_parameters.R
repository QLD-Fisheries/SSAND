# Copyright 2024 Fisheries Queensland

# This file is part of SSAND.
# SSAND is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
# SSAND is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
# You should have received a copy of the GNU General Public License along with SSAND. If not, see <https://www.gnu.org/licenses/>.

#' Extract Stock Synthesis parameter names
#'
#' @param ss_dat A list of outputs from r4ss::SS_output() or r4ss::SSgetMCMC(), one element per scenario.
#'
#' @return A vector of parameters estimated or fixed in your Stock Synthesis models
#' @export
#'
#' @examples
#' extract_SS_parameters(ss_mle)
#' extract_SS_parameters(ss_mcmc)
extract_SS_parameters <- function(ss_dat) {

  if (length(ss_dat)>75){ss_dat <- list(ss_dat)}
  if (!is.list(ss_dat)) {stop("Please enter data as a list of outputs, one element per scenario.")}

  MCMC <- is.data.frame(ss_dat[[1]])

  if (!MCMC) {
    names <- NULL
    for (i in 1:length(ss_dat)) {
      tmp <- data.frame(rownames(ss_dat[[i]]$parameters  |>
                                   dplyr::filter(!grepl("Main_RecrDev", Label)) |>
                                   dplyr::filter(!grepl("ForeRecr", Label)) ))
      names <- rbind(names, tmp)
    }
    names <- unique(names)
  }

  if (MCMC) {
    names <- NULL
    for (i in 1:length(ss_dat)) {
      tmp <- data.frame(name = names(ss_mcmc[[i]])) |>
        dplyr::filter(!grepl("Main_RecrDev", name)) |>
        dplyr::filter(!grepl("ForeRecr", name))
      names <- rbind(names, tmp)
    }
    names <- unique(names)
  }

  return(names)
}

