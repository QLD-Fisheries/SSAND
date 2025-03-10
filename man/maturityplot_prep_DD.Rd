% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/maturityplot_prep_DD.R
\name{maturityplot_prep_DD}
\alias{maturityplot_prep_DD}
\title{Prepare DDUST data for maturity plots}
\usage{
maturityplot_prep_DD(
  x_max = NULL,
  x_mat = NULL,
  x_increment = 1,
  maturity_type = "length",
  sex = 1
)
}
\arguments{
\item{x_max}{Maximum length or age to plot.}

\item{x_mat}{Length or age at which animals mature.}

\item{x_increment}{Increment for plotting maturity.}

\item{maturity_type}{Either "length" or "age", to define the x-axis.}

\item{sex}{Indicate which sex to plot. Enter 1 for female or 2 for male.}
}
\value{
A data frame with variables value, maturity, sex, scenario and type (length or age)
}
\description{
This plot requires manual specification of the data as it is not built into DDUST.
Prepares data for knife-edge maturity.
}
\examples{
data <- maturityplot_prep_DD(x_max=10,x_mat=2)
maturityplot(data)
}
