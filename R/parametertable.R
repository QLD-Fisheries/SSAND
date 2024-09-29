# Copyright 2024 Fisheries Queensland

# This file is part of SSAND.
# SSAND is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
# SSAND is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
# You should have received a copy of the GNU General Public License along with SSAND. If not, see <https://www.gnu.org/licenses/>.

#' Parameter table
#'
#' @param data Dataframe output from parametertable_prep()
#' @param label A label for the table that can be referenced elsewhere in the report using LaTeX syntax
#' @param parameter_names An optional vector of customised parameter labels
#' @param column_names An optional vector of column names
#' @param caption Caption for table
#' @param align An optional vector of alignment for the table, input as a character string that is one character longer than the number of columns in the table. Default is centered.
#' @param round A vector of digits, one value per row of the table, used to round the parameter estimates
#'
#' @return Code that can be used within a code chunk in a RMarkdown style document to produce a parameter estimate table
#' @export
#'
#' @examples
#' # \rowcolors{2}{white}{light-gray}
#' # <<table_mcmc1, results='asis', echo=FALSE>>=
#' parameters <- extract_SS_parameters(ss_mle)[2:4,]
#' data <- parametertable_prep_SS(ss_mle=ss_mle, parameters=parameters, scenario=1)
#' parametertable(data, label="tab:param")
#' # @
parametertable <- function (data,
                            label,
                            parameter_names = NULL,
                            column_names = NULL,
                            caption = "Summary of parameter estimates",
                            align = NULL,
                            round = NULL) {

  if (missing(label)) {warning("Please specify a label for your table (e.g. label=\"tab:parameters\")" )  }


  if (names(data)[2] == "MCMC median") {MCMC<- TRUE} else {MCMC=FALSE}
  if (!missing(parameter_names) & length(parameter_names) != nrow(data)) {warning("If customising the parameter labels, you must specify the label for every parameter in the table.")}
  if (!missing(parameter_names)) {data$Symbol <- parameter_names}

  if (missing(align)) {align = paste(rep("c",ncol(data)+1), collapse = '')}

  if (!missing(column_names)) {colnames(data) <- column_names}


  if (missing(round)) {round <- rep(2,nrow(data))}
  if ("Value" %in% names(data)) {data$Value <- mapply(function(value, digits) round(value, digits), data$Value, round)}
  if ("MCMC median" %in% names(data)) {data$`MCMC median` <- mapply(function(value, digits) round(value, digits), data$`MCMC median`, round)}
  if ("MCMC 2.5\\%" %in% names(data)) {data$`MCMC 2.5\\%` <- mapply(function(value, digits) round(value, digits), data$`MCMC 2.5\\%`, round)}
  if ("MCMC 97.5\\%" %in% names(data)) {data$`MCMC 97.5\\%` <- mapply(function(value, digits) round(value, digits), data$`MCMC 97.5\\%`, round)}

  data <- data |> dplyr::mutate_all(~ gsub("_", "\\\\_", .))

  print(xtable::xtable(data,
                       caption =caption,
                       align=align,
                       label = label),
        include.rownames = FALSE,
        sanitize.colnames.function=function(x) paste('{\\textbf{',x,'}}', sep =''),
        sanitize.text.function= function(x) x,
        table.placement = "H")
}
