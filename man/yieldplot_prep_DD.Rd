% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/yieldplot_prep_DD.R
\name{yieldplot_prep_DD}
\alias{yieldplot_prep_DD}
\title{Prepare DDUST data for yield plot}
\usage{
yieldplot_prep_DD(dd_mle, dd_mcmc = NULL, dd_sim = NULL, scenarios = NULL)
}
\arguments{
\item{dd_mle}{A list of outputs from DDUST::makefullreport() with one list element per scenario. Will automatically reformat as a list if a single DDUST::makefullreport() output (i.e. one scenario) is entered}

\item{dd_mcmc}{A list of model fits from tmbstan::tmbstan() with one list element per scenario. Only needed if MCMC was used.}

\item{dd_sim}{A list of outputs from SSAND::simulate_DD() with one list element per scenario. Only required if MCMC was used. Will automatically reformat as a list if a single DDUST::makefullreport() output (i.e. one scenario) is entered}

\item{scenarios}{A vector of scenarios to plot (numeric). Shows all scenarios if left blank. Can be overridden in the plotting function.}
}
\value{
A dataframe with Final_bio (num), yield (num), data (chr), scenario (int)
}
\description{
Prepare DDUST data for yield plot
}
\examples{
data <- yieldplot_prep_DD(dd_mle)
yieldplot(data)
}
