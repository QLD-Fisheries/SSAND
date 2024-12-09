% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/growthplot_prep_DD.R
\name{growthplot_prep_DD}
\alias{growthplot_prep_DD}
\title{Prepare DDUST data for growth plot}
\usage{
growthplot_prep_DD(
  dd_mle,
  type = "weight",
  age_rec = 2,
  age_max = 10 * 12,
  W_r_minus1 = NA,
  W_r = NA,
  rho = NA,
  t0 = NA,
  kappa = NA,
  Linf = NA,
  scenarios = 1
)
}
\arguments{
\item{dd_mle}{A list of outputs from DDUST::makefullreport() with one list element per scenario. Will automatically reformat as a list if a single DDUST::makefullreport() output (i.e. one scenario) is entered}

\item{type}{'length' for Von Bert model or 'weight' for Schnute model.}

\item{age_rec}{Age of recruitment, in the unit of measurement used in the model.}

\item{age_max}{Maximum age to plot, in the unit of measurement used in the model.}

\item{W_r_minus1}{Weight at the age one timestep before recruitment. This can be left as NA to use the value used in the model.}

\item{W_r}{Weight at recruitment. This can be left as NA to use the value used in the model.}

\item{rho}{A parameter of the Schnute growth equation. This can be left as NA to use the value used in the model.}

\item{t0}{Von Bert growth parameter (annual)}

\item{kappa}{Von Bert growth parameter (annual)}

\item{Linf}{Von Bert growth parameter (in cm)}

\item{scenarios}{A vector of scenarios to plot (numeric). Shows all scenarios if left blank. Can be overridden in the plotting function.}
}
\value{
A data frame with variables age (numeric), value (numeric), upper (numeric), and lower (numeric) and Sex (integer)
}
\description{
Prepare DDUST data for growth plot
}
\examples{
data <- growthplot_prep_DD(dd_mle)
growthplot(data)
}
