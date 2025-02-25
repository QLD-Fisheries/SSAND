% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/investigate_rawcpueplot.R
\name{rawcpueplot}
\alias{rawcpueplot}
\title{Raw CPUE plot}
\usage{
rawcpueplot(
  data,
  show_monthly = FALSE,
  species_of_interest = NULL,
  interesting_fishers = NULL,
  max_days = 1,
  xlab = "Year",
  ylab = "Raw catch per unit effort (kg/fishing day)",
  show_region = FALSE,
  show_latband = FALSE,
  show_latband_regions = FALSE,
  remove_NA_latband = FALSE,
  facet_labels = NULL,
  scales = "free",
  ncol = 2,
  colour_var = "region",
  facet_var = NULL,
  colours = rep(fq_palette("alisecolours"), 10),
  legend_position = "top",
  legend_title = "",
  extract_data = FALSE
)
}
\arguments{
\item{data}{Output of format_logbooks()}

\item{show_monthly}{Set to TRUE to display by month}

\item{species_of_interest}{Optional. The name of the species of interest, as listed in the 'species' column of data. Filters data to only include that species.}

\item{interesting_fishers}{Optional. A vector of fishers/operators by which to filter the dataset.}

\item{max_days}{Numeric. Filter to include only records that were less than or equal to this maximum number of fishing days (i.e. exclude multi-day trips)}

\item{xlab}{Label for x-axis of plot (character). Default is "".}

\item{ylab}{Label for y-axis of plot (character). Default is "".}

\item{show_region}{Set to TRUE to display by region}

\item{show_latband}{Set to TRUE to display by latitude band}

\item{show_latband_regions}{Set to TRUE to display by latitude band grouping}

\item{remove_NA_latband}{Set to TRUE to remove data without valid latitude band.}

\item{facet_labels}{A vector used to customise facets.}

\item{scales}{Scales used in facet_wrap. Deafult is 'free'}

\item{ncol}{Number of columns for facet wrap. Default is 2.}

\item{colour_var}{Variable by which plot is to be coloured (e.g. "region")}

\item{facet_var}{Variable by which plot is to be faceted (e.g. "region")}

\item{colours}{A vector of colours for the plot}

\item{legend_position}{Legend position}

\item{legend_title}{Legend text}

\item{extract_data}{Set to TRUE to return data instead of plot. Default is FALSE.}
}
\value{
Raw CPUE plot
}
\description{
Raw CPUE plot
}
\examples{
\dontrun{rawcpueplot(data = format_logbooks(raw_data))}
}
