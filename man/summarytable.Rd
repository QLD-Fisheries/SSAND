% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/summarytable.R
\name{summarytable}
\alias{summarytable}
\title{Summary table}
\usage{
summarytable(
  data,
  label,
  row_names = NULL,
  column_names = NULL,
  caption = "Stock status indicators",
  align = NULL
)
}
\arguments{
\item{data}{Dataframe output from summarytable_prep()}

\item{label}{A label for the table that can be referenced elsewhere in the report using LaTeX syntax}

\item{row_names}{An option to customise the text in the Indicator columns}

\item{column_names}{An optional vector of column names}

\item{caption}{Caption for table}

\item{align}{An optional vector of alignment for the table, input as a character string that is one character longer than the number of columns in the table. Default is centered.}
}
\value{
Code that can be used within a code chunk in a RMarkdown style document to produce a summary table
}
\description{
Summary table
}
\examples{
# <<table_summary, results='asis', echo=FALSE>>=
data <- summarytable_prep_SS(ss_mle[[1]],ss_mcmc[[1]])
summarytable(data, label="tab:summary")
# @
}
