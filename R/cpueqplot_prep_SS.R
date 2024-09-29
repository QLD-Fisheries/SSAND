# Copyright 2024 Fisheries Queensland

# This file is part of SSAND.
# SSAND is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
# SSAND is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
# You should have received a copy of the GNU General Public License along with SSAND. If not, see <https://www.gnu.org/licenses/>.

#' Prepare data for a plot that illustrates the impact of catchability rescaling how the model perceives catch rates
#'
#' @param cont_ss_mle An output of r4ss::SS_output() for a model scenario that uses continuous catch rates for the entire assessment period
#' @param split_ss_mle An output of r4ss::SS_output() for a model scenario that uses fleets to represent changes in fishing behaviour (e.g. fishery closures)
#' @param offset_ss_mle An output of r4ss::SS_output() for a model scenario that uses uses offset continuous catch rates for the entire assessment period
#' @param yearsplit When were catch rates split into two fleets for split_ss_mle
#' @param parameters Name/s of catchability parameters from ss_mle$parameters
#' @param labels Labels for plot
#'
#' @return A data frame with variables Year (numerical), cpueadjust (numerical), scenario (character)
cpueqplot_prep_SS <- function(cont_ss_mle,
                              split_ss_mle,
                              offset_ss_mle = NULL,
                              yearsplit = 1988,
                              parameters = c("LnQ_base_Commercial","LnQ_base_CommercialPre(1)","LnQ_base_CommercialPost(2)","LnQ_base_Commercial"),
                              labels = c("Continuous","Split catch rates (before)","Split catch rates (after)","Offset catch rates")) {   # Catchability

  warning("This plot prep is under construction. No sample data available to test yet.")


  # # See how Stock Synthesis calculated q and transformed catch rates relative to each other.
  # cpue_pre  <- cpue_temp |> filter(Year<yearsplit)
  # cpue_post <- cpue_temp |> filter(Year>=yearsplit)
  #
  # # Extract q calculated by SS
  # con <- file(paste0('../../intermediate/prawns/', species[species_id], '/',scenarios[[species_id]][scenario_id],'/Report.sso'))
  # lines <- readLines(con)
  # library(stringr)
  # id <- which(str_detect(lines,"LnQ_base_"))
  # line <- lines[id]
  # lnQ <- as.numeric(word(trimws(line),3))
  # lnQpre <- lnQ[1]
  # lnQpost <- lnQ[2]
  # close(con)
  #
  # # Calculate biomass (proportional to catch rates) using q
  # # C = qEB  --> C/E = qB --> B = C/E / q (biomass is catch rate divided by q)
  # # q is in log space so put catch rates into log space and subtract
  #
  # Bpre <- tibble(Year=cpue_pre$Year, B=log(cpue_pre$cpue) - lnQpre, name="HTrawl")
  # Bpost <- tibble(Year=cpue_post$Year, B=log(cpue_post$cpue) - lnQpost, name="Logbooks")
  # Bsplit <- Bpre |> rbind(Bpost)
  # ggplot(Bsplit) + geom_point(aes(x=Year, y=B, colour=name)) + theme_bw() + ylab("log(B)")


  if (missing(offset_ss_mle)) {
    scenario_list <- list(cont_ss_mle,split_ss_mle)
    scenario_name <- c("Continuous","Split catch rates")
  } else {
    scenario_list <- list(cont_ss_mle,split_ss_mle,offset_ss_mle)
    scenario_name <- c("Continuous","Split catch rates","Offset catch rates")
  }

  cpue_cont <- data.frame(year = cont_ss_mle$cpue$Yr, cpue_cont = cont_ss_mle$cpue$Obs)
  cpue_pre  <- cpue_cont |> dplyr::filter(year<yearsplit)
  cpue_post <- cpue_cont |> dplyr::filter(year>=yearsplit)
  if (!missing(offset_ss_mle)) {
    cpue_offset <- data.frame(year = offset_ss_mle$cpue$Yr, cpue_offset = offset_ss_mle$cpue$Obs)
  }

  lnQcont   <- split_ss_mle$parameters  |> dplyr::filter(Label == parameters[1]) |> dplyr::select(Value) |> dplyr::pull() |> head()
  lnQpre    <- split_ss_mle$parameters  |> dplyr::filter(Label == parameters[2]) |> dplyr::select(Value) |> dplyr::pull() |> head()
  lnQpost   <- split_ss_mle$parameters  |> dplyr::filter(Label == parameters[3]) |> dplyr::select(Value) |> dplyr::pull() |> head()
  if (!missing(offset_ss_mle)) {
    lnQoffset <- offset_ss_mle$parameters |> dplyr::filter(Label == parameters[4]) |> dplyr::select(Value) |> dplyr::pull() |> head()
  }

  # Calculate biomass (proportional to catch rates) using q
  # C = qEB  --> C/E = qB --> B = C/E / q (biomass is catch rate divided by q)
  # q is in log space so put catch rates into log space and subtract

  Bcont   <- data.frame(year=cpue_cont$year   , B=log(cpue_cont$cpue)   - lnQcont  , scenario_name = labels[1])
  Bpre    <- data.frame(year=cpue_pre$year    , B=log(cpue_pre$cpue)    - lnQpre   , scenario_name = labels[2])
  Bpost   <- data.frame(year=cpue_post$year   , B=log(cpue_post$cpue)   - lnQpost  , scenario_name = labels[3])
  data <- rbind(Bcont, Bpre, Bpost)

  if (!missing(offset_ss_mle)) {
    Boffset <- data.frame(year=cpue_offset$year , B=log(cpue_offset$cpue) - lnQoffset, scenario_name = labels[4])
    data <- rbind(data, Boffset)
  }

  return(data)
}

