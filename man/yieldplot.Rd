% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/yieldplot.R
\name{yieldplot}
\alias{yieldplot}
\title{Yield curve plot for MCMC runs}
\usage{
yieldplot(
  data,
  xlab = "Biomass (relative)",
  ylab = "Equilibrium dead catch (t)",
  text_size = 12,
  show_current_line = TRUE,
  show_msy_line = FALSE,
  scenarios = NULL,
  scenario_labels = NULL,
  scenario_order = NULL,
  scales = "free",
  ncol = 2
)
}
\arguments{
\item{data}{Output from yieldplot_prep() with columns Final_bio (num), yield (num), data (chr), scenario (fac)}

\item{xlab}{Label for x-axis (character). Default is "Biomass (relative)".}

\item{ylab}{Label for y-axis (character). Default is "Equilibrium dead catch (t)".}

\item{text_size}{Text size (num). Default is 12.}

\item{show_current_line}{Set to TRUE to include a line on the plot showing where the current fishing pressure sits (logical).}

\item{show_msy_line}{Set to TRUE to include a line that shows MSY (logical).}

\item{scenarios}{A vector of scenario numbers to be shown on plot (numeric). This was already specified in prep file, but this is a manual override to save running the prep function again.}

\item{scenario_labels}{A vector of customised scenario names (character). Default is "Scenario 1", "Scenario 2", etc.}

\item{scenario_order}{A vector to reorder how scenarios are displayed (character). Use the label names defined in "scenario_labels".
If "scenario_labels" is left blank, the labels will be "Scenario 1", "Scenario 2" etc.
Any scenarios not included in "scenario_order" will be tacked on in the order they appear in the input data.}

\item{scales}{Scales for ggplot2::facet_wrap(). Default is 'free', see ?ggplot2::facet_wrap for options.}

\item{ncol}{Number of columns for facet_wrap(). Default is 2.}
}
\value{
Plot of spawning biomass vs equilibrium catch
}
\description{
Yield curve plot for MCMC runs
}
\examples{
data <- yieldplot_prep_SS(ss_mle)
yieldplot(data, show_msy_line=TRUE)
}
