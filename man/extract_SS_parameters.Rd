% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extract_SS_parameters.R
\name{extract_SS_parameters}
\alias{extract_SS_parameters}
\title{Extract Stock Synthesis parameter names}
\usage{
extract_SS_parameters(ss_dat)
}
\arguments{
\item{ss_dat}{A list of outputs from r4ss::SS_output() or r4ss::SSgetMCMC(), one element per scenario.}
}
\value{
A vector of parameters estimated or fixed in your Stock Synthesis models
}
\description{
Extract Stock Synthesis parameter names
}
\examples{
extract_SS_parameters(ss_mle)
extract_SS_parameters(ss_mcmc)
}
