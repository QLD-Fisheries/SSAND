% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/discardplot_prep_SS.R
\name{discardplot_prep_SS}
\alias{discardplot_prep_SS}
\title{Discard plot}
\usage{
discardplot_prep_SS(ss_mle, scenarios = NULL)
}
\arguments{
\item{ss_mle}{A list of outputs from r4ss::SS_output() with one element per scenario. Will automatically reformat as a list if a single r4ss::SS_output() output (i.e. one scenario) is entered.}

\item{scenarios}{A vector of scenarios to plot (numeric). Shows all scenarios if left blank. Can be overridden in the plotting function.}
}
\value{
A dataframe with year, fleet, obs, upper, lower, exp
}
\description{
Discard plot
}
\examples{
data <- discardplot_prep_SS(ss_mle)
discardplot(data)
}
