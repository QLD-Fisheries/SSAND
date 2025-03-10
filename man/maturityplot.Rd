% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/maturityplot.R
\name{maturityplot}
\alias{maturityplot}
\title{Maturity plot}
\usage{
maturityplot(
  data,
  maturity_type = "length",
  xlab = NULL,
  ylab = "Maturity",
  text_size = 12,
  show_two_sex = NULL,
  scenarios = NULL,
  scenario_labels = NULL,
  scenario_order = NULL,
  colours = NULL,
  scales = "free",
  ncol = 2
)
}
\arguments{
\item{data}{A data frame with variables value (num), maturity (num), sex (int), scenario (int), type (chr)}

\item{maturity_type}{Either "length" or "age", to define the x-axis.}

\item{xlab}{Label for x-axis (character). Default is "Age".}

\item{ylab}{Label for y-axis (character). Default is "Carapace length (cm, beginning of year)".}

\item{text_size}{Text size (num). Default is 12.}

\item{show_two_sex}{Set to TRUE to activate a feature that is relevant for two-sex models (logical).}

\item{scenarios}{A vector of scenario numbers to be shown on plot (numeric). This was already specified in prep file, but this is a manual override to save running the prep function again.}

\item{scenario_labels}{A vector of customised scenario names (character). Default is "Scenario 1", "Scenario 2", etc.}

\item{scenario_order}{A vector to reorder how scenarios are displayed (character). Use the label names defined in "scenario_labels".
If "scenario_labels" is left blank, the labels will be "Scenario 1", "Scenario 2" etc.
Any scenarios not included in "scenario_order" will be tacked on in the order they appear in the input data.}

\item{colours}{A vector of colours used (character).}

\item{scales}{Scales for ggplot2::facet_wrap(). Default is 'free', see ?ggplot2::facet_wrap for options.}

\item{ncol}{Number of columns for facet_wrap(). Default is 2.}
}
\value{
Maturity plot
}
\description{
Maturity plot
}
\examples{
data <- maturityplot_prep_SS(ss_mle)
maturityplot(data)

data <- maturityplot_prep_DD(x_max=10,x_mat=2)
maturityplot(data)
}
