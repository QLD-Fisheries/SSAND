% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cpueqplot_prep_SS.R
\name{cpueqplot_prep_SS}
\alias{cpueqplot_prep_SS}
\title{Prepare data for a plot that illustrates the impact of catchability rescaling how the model perceives catch rates}
\usage{
cpueqplot_prep_SS(
  cont_ss_mle,
  split_ss_mle,
  offset_ss_mle = NULL,
  yearsplit = 1988,
  parameters = c("LnQ_base_Commercial", "LnQ_base_CommercialPre(1)",
    "LnQ_base_CommercialPost(2)", "LnQ_base_Commercial"),
  labels = c("Continuous", "Split catch rates (before)", "Split catch rates (after)",
    "Offset catch rates")
)
}
\arguments{
\item{cont_ss_mle}{An output of r4ss::SS_output() for a model scenario that uses continuous catch rates for the entire assessment period}

\item{split_ss_mle}{An output of r4ss::SS_output() for a model scenario that uses fleets to represent changes in fishing behaviour (e.g. fishery closures)}

\item{offset_ss_mle}{An output of r4ss::SS_output() for a model scenario that uses uses offset continuous catch rates for the entire assessment period}

\item{yearsplit}{When were catch rates split into two fleets for split_ss_mle}

\item{parameters}{Name/s of catchability parameters from ss_mle$parameters}

\item{labels}{Labels for plot}
}
\value{
A data frame with variables Year (numerical), cpueadjust (numerical), scenario (character)
}
\description{
Prepare data for a plot that illustrates the impact of catchability rescaling how the model perceives catch rates
}
