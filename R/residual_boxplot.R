#' Fitted values vs residuals boxplot.
#' 
#' Identify heteroscedasticity arising from a fitted `glm()` or `gam()`.
#'
#' @param model A fitted `gam` or `glm` object as produced by `gam()` or `glm()`.
#' @param covariate Character string indicating the covariate to be plotted on the x-axis. By default, residuals are plotted against fitted.
#' @param log TRUE to log-transform fitted values.
#' @param nbreaks The number of breaks to group fitted values. Each group contains an equal number of observations.
#' @param zero_line TRUE to add dotted line at `y = 0`.
#' @param type the type of residuals which should be returned. The alternatives are: "deviance" (default), "pearson", "working", "response", and "partial".
#' 
#' @return A boxplot made with ggplot2.
#' @export
#' 
#' @examples
#' raw_catch_rates <- data.frame(
#'     year = seq(5,100,5),
#'     effort = c(118,58,42,35,27,25,21,19,18,10,12,15,13,22,8,9,7,16,17,12),
#'     kilograms = c(75,81,72,69,35,26,21,18,16,13,12,12,11,9,10,11,15,9,8,5))
#' model <- glm(kilograms ~ year + effort, data = raw_catch_rates, family = Gamma)
#' residual_boxplot(model, log = TRUE, nbreaks = 5)
#' residual_boxplot(model, covariate = 'effort', nbreaks = 5)
 
residual_boxplot <- function(model, covariate = 'fitted', log = FALSE, nbreaks = 10, zero_line = TRUE, type = "deviance"){

  # 1. Extract model information
  if (covariate == 'fitted'){
    xdata <- fitted(model)
    if (log) {xdata <- log(xdata)}
  } else {
    xdata <- unlist(model$data[covariate])
  }
  breaks <- seq(0,length(xdata),length.out=nbreaks+1)
  residuals <- residuals(model, type = type)

  if (nbreaks > length(xdata)/2) {warning('Few observations per group. Recommend decreasing nbreaks')}
  if (length(xdata) > length(residuals)) {stop("glm() has truncated input data to remove missing observations. Re-run glm() with non-missing data.")}

  # 2. Assemble data frame for plotting
  df <- data.frame(xdata = xdata, residuals = residuals) |> 
    dplyr::arrange(xdata) |> 
    dplyr::mutate(observation_number = dplyr::row_number(),
            cut = cut(observation_number,breaks=breaks))

  # 3. Plot
  p <- ggplot2::ggplot(df) +
  ggplot2::geom_boxplot(ggplot2::aes(x=xdata, y = residuals, group = cut)) +
  ggplot2::theme_bw() +
  ggplot2::xlab(stringr::str_to_sentence(paste0(ifelse(log,'Log ', ''), covariate))) +
  ggplot2::ylab(paste0('Residual (',type,')'))

  # 4. Add zero line
  if (zero_line){
    p <- p + ggplot2::geom_hline(ggplot2::aes(yintercept=0), linetype = 2)
  }

  return(p)
}
