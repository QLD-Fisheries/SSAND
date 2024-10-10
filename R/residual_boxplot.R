#' Fitted values vs residuals boxplot.
#' 
#' Identify heteroscedasticity arising from a fitted `glm()` or `gam()`.
#'
#' @param model A fitted `gam` or `glm` object as produced by `gam()` or `glm()`.
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
#'    year = seq(5,100,5),
#'    effort = c(118,58,42,35,27,25,21,19,18,10,12,15,13,22,8,9,7,16,17,12),
#'    kilograms = c(75,81,72,69,35,26,21,18,16,13,12,12,11,9,10,11,15,9,8,5))
#' model <- glm(kilograms ~ year + effort, data = raw_catch_rates, family = Gamma)
#' residual_boxplot(model, log = TRUE, nbreaks = 5)
#' 
residual_boxplot <- function(model, log = FALSE, nbreaks = 10, zero_line = TRUE, type = "deviance"){

  # 1. Extract model information
  fitted <- fitted(model)
  if (log) {fitted <- log(fitted)}
  residuals <- residuals(model, type = type)
  breaks <- seq(0,length(fitted),length.out=nbreaks+1)

  if (nbreaks > length(fitted)/2) {warning('Few observations per group. Recommend decreasing nbreaks')}

  # 2. Assemble data frame for plotting
  df <- data.frame(fitted = fitted, residuals = residuals) |> 
    dplyr::arrange(fitted) |> 
    dplyr::mutate(observation_number = dplyr::row_number(),
            cut = cut(observation_number,breaks=breaks))

  # 3. Plot
  p <- ggplot2::ggplot(df) +
  ggplot2::geom_boxplot(ggplot2::aes(x=fitted, y = residuals, group = cut)) +
  ggplot2::theme_bw() +
  ggplot2::xlab(ifelse(log,'Log fitted value', 'Fitted value')) +
  ggplot2::ylab(paste0('Residual (',type,')'))

  # 4. Add zero line
  if (zero_line){
    p <- p + ggplot2::geom_hline(ggplot2::aes(yintercept=0), linetype = 2)
  }

  return(p)
}
