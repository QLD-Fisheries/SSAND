#' Biomass table
#'
#' @param data Dataframe output from biomasstable_prep()
#' @param label A label for the table that can be referenced elsewhere in the report using LaTeX syntax
#' @param row_names An option to customise the text in the Indicator columns
#' @param column_names An optional vector of column names
#' @param caption Caption for table
#' @param align An optional vector of alignment for the table, input as a character string that is one character longer than the number of columns in the table. Default is centered.
#'
#' @return Code that can be used within a code chunk in a RMarkdown style document to produce a biomass table
#' @export
#'
#' @examples
#' # <<table_biomass, results='asis', echo=FALSE>>=
#' data <- biomasstable_prep_SS(ss_mle,ss_mcmc)
#' biomasstable(data, label="tab:biomass")
#' # @
biomasstable <- function (data,
                          label,
                          row_names = NULL,
                          column_names = NULL,
                          caption = NULL,
                          align = NULL) {

  if (missing(label)) {warning("Please specify a label for your table (e.g. label=\"tab:summary\")" )  }

  if (!missing(row_names) & length(row_names) != nrow(data[[1]])) {warning("If customising the row names, you must specify the label for every row in the table.")}

  if (missing(align)) {align = paste(rep("c",ncol(data[[1]])+1), collapse = '')}

  if (missing(caption)) {caption = paste0("Summary of model outcomes for all scenarios.$B_{",
                                          data[[3]],"}$\\% is the most likely biomass in ",
                                          data[[3]]," relative to unfished in ",
                                          data[[2]]," with the 95\\% confidence interval for maximum likelihoods estimations and 95\\% credible interval for MCMC estimations.")
  }

  # Create xtable object
  xtable_obj <- xtable::xtable(data[[1]], caption = caption, label = label, align = align, collapse = "")

  # Capture the output of the print function
  tab <- utils::capture.output(
    print(xtable_obj,
          only.contents = TRUE,
          include.rownames = FALSE,
          include.colnames = TRUE,
          sanitize.colnames.function = function(x) paste('{\\textbf{', x, '}}', sep = ''),
          sanitize.text.function = function(x) x,
          table.placement = "H"
    )
  )

  # Convert the captured output to a single string
  tab <- paste(tab, collapse = "\n")

  # Add the table environment manually
  tab <- paste0("\\rowcolors{2}{light-gray}{white} \n",
                       "\\begin{table}[H] \n",
                       "\\small \n",
                       "\\centering \n",
                       "\\caption{",caption,"} \n",
                       "\\begin{tabular}{m{0.08\\textwidth}  m{0.1\\textwidth} m{0.1\\textwidth} m{0.1\\textwidth}  | m{0.1\\textwidth} m{0.1\\textwidth} m{0.1\\textwidth}  } \n",
                       "\\hline \n",
                       "\\rowcolor{white} \n",
                       "\\multicolumn{1}{l}{\\textbf{Scenario}} &  \\multicolumn{3}{c}{\\textbf{MLE}} & \\multicolumn{3}{c}{\\textbf{MCMC}}\\\\ \n",
                       "\\cmidrule(l){2-4} \\cmidrule(l){5-7} \n",

                       tab,
                       "\\end{tabular} \n",
                       "\\label{",label,"} \n",
                       "\\end{table} \n"
  )

  return(cat(tab))
}
