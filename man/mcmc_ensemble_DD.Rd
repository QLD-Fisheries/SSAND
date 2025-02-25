% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mcmc_ensemble_DD.R
\name{mcmc_ensemble_DD}
\alias{mcmc_ensemble_DD}
\title{MCMC ensemble preparation for DDUST}
\usage{
mcmc_ensemble_DD(dd_mcmc, dd_sim, scenarios = NULL, weighting = NULL)
}
\arguments{
\item{dd_mcmc}{A list of model fits from tmbstan::tmbstan() with one list element per scenario. Only needed if MCMC was used.}

\item{dd_sim}{A list of outputs from DDUST::simulate_DD() with one list element per scenario. Only required if MCMC was used. Will automatically reformat as a list if a single DDUST::makefullreport() output (i.e. one scenario) is entered}

\item{scenarios}{A vector of scenarios to be included in the ensemble.}

\item{weighting}{A vector of integers to weight each scenario, one integer per scenario.
The MCMC iterations for each scenario are repeated the number of times indicated by the corresponding element in the weighting vector.
For example, for an ensemble with four scenarios, to double the weighting of the first scenario you would enter weighting=c(2,1,1,1).}
}
\value{
A list containing one dataframe of all specified dd_mcmc and dd_sim objects combined, limited to columns that are common between dd_mcmc objects, duplicated as per the specified weighting.
An extra column "ensemble" is appended, for datachecks in other functions.
}
\description{
MCMC ensemble preparation for DDUST
}
\examples{
\dontrun{
library(DDUST)
dd_sim <- simulate_DDUST(dd_mle,dd_mcmc)
dd_mcmc_ens <- mcmc_ensemble_DD(dd_mcmc,dd_sim,scenarios=c(1,2))$dd_mcmc
dd_sim_ens <- mcmc_ensemble_DD(dd_mcmc,dd_sim,scenarios=c(1,2))$dd_sim
biomassplot(data)
}
}
