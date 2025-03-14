% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/selectivityplot.R
\name{selectivityplot}
\alias{selectivityplot}
\title{Selectivity plot}
\usage{
selectivityplot(
  data,
  fleets = NULL,
  fleet_names = NULL,
  selectivity_type = "Selectivity (length)",
  xlab = "Length (cm)",
  ylab = "Selectivity",
  text_size = 12,
  scenarios = NULL,
  scenario_labels = NULL,
  scenario_order = NULL,
  colours = NULL,
  scales = "free",
  ncol = 2,
  MLS = NULL,
  MLS_colour = "black",
  MLS_linetype = "dotted",
  MLS_position = 0.9,
  MLS_label = NULL,
  MLS_label_size = 3,
  show_ribbon = TRUE,
  time_blocks = FALSE,
  years = NULL
)
}
\arguments{
\item{data}{Output from selectivityplot_prep(). A data frame with variables fleet (num), year (int), sex (fac), value (num), type (fac), selectivity (num), scenario (fac)}

\item{fleets}{A numeric or vector of fleet numbers to plot}

\item{fleet_names}{A vector of customised fleet names for legend}

\item{selectivity_type}{A vector of types of selectivity information to show on the plot. Options are "Selectivity (length)", "Selectivity (age)", "Retention", "Discard mortality", "Dead", "Discard".}

\item{xlab}{Label for x-axis (character). Default is "Age".}

\item{ylab}{Label for y-axis (character). Default is "Carapace length (cm, beginning of year)".}

\item{text_size}{Text size (num). Default is 12.}

\item{scenarios}{A vector of scenario numbers to be shown on plot (numeric). This was already specified in prep file, but this is a manual override to save running the prep function again.}

\item{scenario_labels}{A vector of customised scenario names (character). Default is "Scenario 1", "Scenario 2", etc.}

\item{scenario_order}{A vector to reorder how scenarios are displayed (character). Use the label names defined in "scenario_labels".
If "scenario_labels" is left blank, the labels will be "Scenario 1", "Scenario 2" etc.
Any scenarios not included in "scenario_order" will be tacked on in the order they appear in the input data.}

\item{colours}{A vector of colours used (character).}

\item{scales}{Scales for ggplot2::facet_wrap(). Default is 'free', see ?ggplot2::facet_wrap for options.}

\item{ncol}{Number of columns for facet_wrap(). Default is 2.}

\item{MLS}{Number for the minimum legal size. If a value is entered, a dashed line for MLS will be added to the plot.}

\item{MLS_colour}{Colour of MLS vertical line.}

\item{MLS_linetype}{Linetype of MLS vertical line.}

\item{MLS_position}{Vertical position of MLS label}

\item{MLS_label}{Label for MLS line. Default is "MLS: X cm"}

\item{MLS_label_size}{Size of label for MLS line}

\item{show_ribbon}{Set to TRUE to display 95\% credible interval ribbon for MCMC results}

\item{time_blocks}{Set to TRUE to show time blocks. You might want to also set selectivity_type to something relevant such as "Discard"}

\item{years}{A vector of years to show. Relevant if time_blocks==TRUE.}
}
\value{
Selectivity plot
}
\description{
Selectivity plot
}
\examples{
data <- selectivityplot_prep_SS(ss_mle)
selectivityplot(data)
selectivityplot(data, selectivity_type = "Selectivity (length)")
selectivityplot(data, selectivity_type = "Selectivity (age)", xlab = "Age (years)")
selectivityplot(data,
                selectivity_type = c("Selectivity (length)",
                                     "Retention",
                                     "Dead",
                                     "Discard"),
                xlab = "Age (years)")

selectivityplot(data,
                selectivity_type = c("Discard"),
                time_blocks = TRUE,
                scenarios = 1,
                fleets = 2,
                years = c(1993,2022))
}
