% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lifehistoryplot.R
\name{lifehistoryplot}
\alias{lifehistoryplot}
\title{Life history plot}
\usage{
lifehistoryplot(
  ss_mle = NULL,
  Linf = NULL,
  k = NULL,
  a0 = NULL,
  M = NULL,
  L50 = NULL,
  L95 = NULL,
  WLa = NULL,
  WLb = NULL,
  MLS = NULL,
  max_length = NULL,
  sex = 1,
  scenario = 1,
  nbreaks = 10,
  colours = c("grey70", "grey70", "black", "grey30"),
  line_width = 1,
  point_size = 2,
  label_spacing_VB = 0.07,
  label_spacing_WL = 0.1,
  label_spacing_M = 0.07,
  label_spacing_mat = 0.07,
  show_MLS_label = TRUE
)
}
\arguments{
\item{ss_mle}{A stock synthesis model output produced by \code{r4ss::SS_output()}}

\item{Linf}{Asymptotic length from VB growth model}

\item{k}{growth completion rate from VB growth model}

\item{a0}{Age-at-length-zero from VB growth model}

\item{M}{Natural mortality as a single estimate.}

\item{L50}{Length-at-50\%- mature in cm.}

\item{L95}{Length-at-95\%- mature in cm.}

\item{WLa}{Weight-Length parameter A. Should be from W-L relationships expressed in kg and cm (as per Stock Synthesis).}

\item{WLb}{Weight-Length parameter B. Should be from W-L relationships expressed in kg and cm (as per Stock Synthesis).}

\item{MLS}{Minimum Legal Size in cm.}

\item{max_length}{Used to set an axis limits for lengths when Linf is not available.}

\item{sex}{Used for SS models to extract a single sex. Has a default of 1 for females. Use 2 for males}

\item{scenario}{Specify scenario to use if inputting a list of scenarios. Default is 1.}

\item{nbreaks}{Approximate number of breaks along x axis.}

\item{colours}{Vectors of four colours to use: main lines, MLS rectangles, points , dotted reference lines}

\item{line_width}{Width of main lines}

\item{point_size}{Size of reference points}

\item{label_spacing_VB}{Spacing between labels for length vs age plot. Default is 0.07.}

\item{label_spacing_WL}{Spacing between labels for weight vs length plot. Default is 0.1.}

\item{label_spacing_M}{Spacing between labels for mortalityplot. Default is 0.07.}

\item{label_spacing_mat}{Spacing between labels for maturity plot. Default is 0.07.}

\item{show_MLS_label}{Set to TRUE to show "Legal-sized" label.}
}
\value{
A cowplot object with four panels displaying species biology
}
\description{
Life history plot
}
\examples{
lifehistoryplot(ss_mle)
lifehistoryplot(Linf = 38, k = 0.3, a0 = 0,
                M = 0.73, L50 = 18, L95 = 22,
                WLa = 0.0013, WLb = 1.949,
                MLS = 20, max_length = 45)
}
