% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/investigate_biologyplots.R
\name{rawbiological_heatplot}
\alias{rawbiological_heatplot}
\title{Heat plot for raw biological data}
\usage{
rawbiological_heatplot(
  data,
  x_var = "month",
  xlab = "Month",
  y_var = "year",
  ylab = "Year",
  fill_var = "n",
  facet_var = NULL,
  MLS_x = NULL,
  MLS_y = NULL,
  scales = "fixed",
  ncol = 1,
  legend_title = "Number of samples",
  legend_position = "right",
  log_transform = FALSE,
  show_values = TRUE,
  text_size = 2,
  extract_data = FALSE
)
}
\arguments{
\item{data}{A table of raw length and age data. Each row is a different sample. Columns are labelled according to your set up in the "_var" arguments of this function.}

\item{x_var}{Variable to be plotted on x-axis (e.g. "year")}

\item{xlab}{Label for x-axis (character)}

\item{y_var}{Variable to be plotted on x-axis (e.g. "fl")}

\item{ylab}{Label for y-axis (character)}

\item{fill_var}{Variable by which plot is to be filled (e.g. "sexcode")}

\item{facet_var}{Variable by which plot is to be faceted (e.g. "region")}

\item{MLS_x}{Optional. The value of the minimum legal size, if a horizontal line is to be added.}

\item{MLS_y}{Optional. The value of the minimum legal size, if a vertical line is to be added.}

\item{scales}{Scaled for facet wrap. Default is 'fixed'.}

\item{ncol}{Number of columns for facet wrap. Default is 2.}

\item{legend_title}{Title of legend}

\item{legend_position}{Legend position}

\item{log_transform}{Set to TRUE to log transform the fill scale}

\item{show_values}{Set to TRUE to show values on tiles}

\item{text_size}{Text size}

\item{extract_data}{Set to TRUE to return data instead of plot. Default is FALSE.}
}
\value{
Heat plot for raw biological data
}
\description{
Heat plot for raw biological data
}
\examples{
\dontrun{rawbiological_heatplot(mac3, facet_var = "region")}
}
