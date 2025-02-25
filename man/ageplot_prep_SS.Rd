% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ageplot_prep_SS.R
\name{ageplot_prep_SS}
\alias{ageplot_prep_SS}
\title{Prepare Stock Synthesis data for age plot}
\usage{
ageplot_prep_SS(ss_mle, scenarios = NULL)
}
\arguments{
\item{ss_mle}{A list of outputs from r4ss::SS_output() with one element per scenario. Will automatically reformat as a list if a single r4ss::SS_output() output (i.e. one scenario) is entered.}

\item{scenarios}{A vector of scenarios to plot (numeric). Shows all scenarios if left blank. Can be overridden in the plotting function.}
}
\value{
A data frame with variables year (int), fleet (int), bin (int), sex (chr), obs (num), exp (num), scenario (num)
}
\description{
Prepare Stock Synthesis data for age plot
}
\examples{
\dontrun{
data <- ageplot_prep_SS(ss_mle)
ageplot(data)
}
}
