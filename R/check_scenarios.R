# Copyright 2024 Fisheries Queensland

# This file is part of SSAND.
# SSAND is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
# SSAND is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
# You should have received a copy of the GNU General Public License along with SSAND. If not, see <https://www.gnu.org/licenses/>.

#' A function used within SSAND to check if SS or DD input into prep functions are a single scenario or list of scenarios
#'
#' @param x dd_mle or ss_mle
#' @param model "DD" or "SS"
#' @param type "MLE" or "MCMC"
#'
#' @return response determining if input is a list of scenarios or a single scenario
#' @export
#'
#' @examples
#' check_scenarios(ss_mle,"SS","MLE")
check_scenarios <- function(x,model,type) {

  if (type=="MLE") {
    if (is.list(x) && all(sapply(x, is.list))) {
      response <- "list of scenarios"
    } else {
      response <- "single scenario"
    }
  }

  if (type=="MCMC" & model=="SS") {
    if (is.list(x) && all(sapply(x, is.data.frame))) {

      if ("ensemble" %in% names(x)) {
        response <- "ensemble"
      } else {
        response <- "list of scenarios"
      }

    } else {
      response <- "single scenario"
    }
  }

  if (type=="MCMC" & model=="DD") {
    if (is.list(x) && all(sapply(x, inherits, "stanfit"))) {
      response <- "list of scenarios"
    } else {
      if (inherits(x,"stanfit")) {
        response <- "single scenario"
      } else {
        response <- "ensemble"
      }
    }
  }

  if (type=="SIM" & model=="DD") {
    if(length(names(x[[1]])) > 2) {
      response <- "single scenario"
    } else {
      response <- "list of scenarios"
    }
  }
  return(response)
}
