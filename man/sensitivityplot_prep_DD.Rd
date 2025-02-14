% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sensitivityplot_prep_DD.R
\name{sensitivityplot_prep_DD}
\alias{sensitivityplot_prep_DD}
\title{Prepare DDUST data for sensitivityplot()}
\usage{
sensitivityplot_prep_DD(
  dd_mle,
  dd_mcmc = NULL,
  dd_sim = NULL,
  scenarios = NULL,
  parameters = NULL,
  show_B_ratio = TRUE,
  show_B_abs = FALSE,
  show_F_final = FALSE,
  show_LL = TRUE
)
}
\arguments{
\item{dd_mle}{A list of outputs from DDUST::makefullreport() with one list element per scenario. Will automatically reformat as a list if a single DDUST::makefullreport() output (i.e. one scenario) is entered}

\item{dd_mcmc}{A list of model fits from tmbstan::tmbstan() with one list element per scenario. Only needed if MCMC was used.}

\item{dd_sim}{A list of outputs from SSAND::simulate_DD() with one list element per scenario. Only required if MCMC was used. Will automatically reformat as a list if a single DDUST::makefullreport() output (i.e. one scenario) is entered}

\item{scenarios}{A list of scenarios to show - shows all scenarios if left blank}

\item{parameters}{A vector of parameters to include on plot (character). See names(dd_mle) for options.}

\item{show_B_ratio}{Set to TRUE to show a panel for final biomass ratio (logical).}

\item{show_B_abs}{Set to TRUE to show a panel for final absolute biomass (logical).}

\item{show_F_final}{Set to TRUE to show final annual fishing mortality (logical).}

\item{show_LL}{Set to TRUE to show negative log likelhood or objective function (logical).}
}
\value{
A data frame with scenario (int), name (fac), value (num), ub (num), lb (num) and fixed (boolean).
}
\description{
Prepare DDUST data for sensitivityplot()
}
\examples{
data <- sensitivityplot_prep_DD(dd_mle)
sensitivityplot(data)

\dontrun{
library(DDUST)
dd_sim <- simulate_DDUST(dd_mle,dd_mcmc)
data <- sensitivityplot_prep_DD(dd_mle, dd_mcmc, dd_sim)
sensitivityplot(data)
}
}
