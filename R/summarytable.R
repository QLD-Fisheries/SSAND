# Copyright 2024 Fisheries Queensland

# This file is part of SSAND.
# SSAND is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
# SSAND is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
# You should have received a copy of the GNU General Public License along with SSAND. If not, see <https://www.gnu.org/licenses/>.

#' Summary table
#'
#' @param data Dataframe output from summarytable_prep()
#' @param label A label for the table that can be referenced elsewhere in the report using LaTeX syntax
#' @param row_names An option to customise the text in the Indicator columns
#' @param column_names An optional vector of column names
#' @param caption Caption for table
#' @param caption_placement Placement of table caption. Default is "top"
#' @param align An optional vector of alignment for the table, input as a character string that is one character longer than the number of columns in the table. Default is centered.
#'
#' @return Code that can be used within a code chunk in a RMarkdown style document to produce a summary table
#' @export
#'
#' @examples
#' # <<table_summary, results='asis', echo=FALSE>>=
#' data <- summarytable_prep_SS(ss_mle[[1]],ss_mcmc[[1]])
#' summarytable(data, label="tab:summary")
#' # @
summarytable <- function (data,
                          label,
                          row_names = NULL,
                          column_names = NULL,
                          caption = "Stock status indicators",
                          caption_placement = "top",
                          align = NULL) {

  if (missing(label)) {warning("Please specify a label for your table (e.g. label=\"tab:summary\")" )  }

  if (!missing(row_names) & length(row_names) != nrow(data)) {warning("If customising the row names, you must specify the label for every row in the table.")}

  if (missing(align)) {align = paste(rep("l",ncol(data)+1), collapse = '')}

  if (!missing(column_names)) {colnames(data) <- column_names}

  print(xtable::xtable(data,
                       caption = caption,
                       align = align,
                       label = label),
        include.rownames = FALSE,
        sanitize.colnames.function=function(x) paste('{\\textbf{',x,'}}', sep =''),
        sanitize.text.function= function(x) x,
        table.placement = "H",
        caption.placement = caption_placement)
}
