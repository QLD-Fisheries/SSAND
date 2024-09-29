# Copyright 2024 Fisheries Queensland

# This file is part of SSAND.
# SSAND is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
# SSAND is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
# You should have received a copy of the GNU General Public License along with SSAND. If not, see <https://www.gnu.org/licenses/>.

#' Correlation Plot
#'
#' @param data A data frame made from correlationplot_prep. If an MLE plot, variables are X1 (fac), X2 (fac), value (num), MCMC (log). If MCMC, variables are chain (fac), iter (int) followed by columns for each parameter to plot.
#' @param labels A vector of labels for the parameters shown on the plot
#' @param show_diagonal Set to TRUE to show values on the diagnoal for MCMC plot (logical). Default is FALSE.
#' @param colours A vector of colours used (character).
#' @param sample Number of samples to plot from each MCMC chain to ease burden of rendering dense plots (numeric).
#' @param columns A vector of columns to display on plot (character).
#'
#' @return Correlation plot
#' @export
#'
#' @examples
#' parameters <- extract_SS_parameters(ss_mle)[c(2,3,4,26),]
#' data <- correlationplot_prep_SS(ss_mle, scenario = 1, parameters = parameters)
#' correlationplot(data)
correlationplot <- function(data,
                            labels = NULL,
                            show_diagonal = FALSE,
                            columns = NULL,
                            colours = NULL,
                            sample = NULL){


  MCMC <- data$MCMC[1]

  if (MCMC==FALSE) {
    data <- data |> dplyr::select(-MCMC)

    if (missing(colours)) {colours <- c("#127B06","#FFE699","#EA9348","white")}

    p <- ggplot2::ggplot(data,ggplot2::aes(X1,X2))+
      ggplot2::geom_tile(ggplot2::aes(fill=value), na.rm = TRUE)+
      ggplot2::scale_fill_gradient2(low = colours[1], mid = colours[2], high = colours[3], na.value = colours[4]) +
      ggplot2::ylab('')+
      ggplot2::xlab('')+
      ggplot2::labs(fill="Correlation")+
      ggplot2::theme_bw()+
      ggplot2::geom_text(ggplot2::aes(label=round(value,2)), colour = 'grey30')


    if (!missing(labels)) {
      p <- p +
        ggplot2::scale_x_discrete(labels = labels) +
        ggplot2::scale_y_discrete(labels = labels)
    }
  }

  if (MCMC==TRUE) {

    if (!missing(sample)) {
      itersubsample <- sample(unique(data$iter), size=sample)
      data <- data |>
        dplyr::filter(iter %in% itersubsample)
    }

    data <- data |> dplyr::select(-MCMC) |> dplyr::select(-iter)

    if (missing(colours)) {colours = c("#b4d9eb", "#cc8c97", "#ddd3a0", "#7ed19a", "#988dd3")}
    if (missing(columns)){columns <- 2:ncol(data)}

    if (show_diagonal){
      colnames(data) <- sub(pattern = "%", replacement = "", colnames(data))

      p <- GGally::ggpairs(data,
                           columns = columns,
                           ggplot2::aes(col=chain),
                           labeller = "label_parsed") +
        ggplot2::theme_bw() +
        ggplot2::scale_colour_manual(values=colours) +
        ggplot2::scale_fill_manual(values=colours)
    } else {

      colnames(data) <- sub(pattern = "%", replacement = "", colnames(data))
      p <- GGally::ggpairs(data,
                           columns = columns,
                           diag = NULL,
                           ggplot2::aes(col=chain),
                           labeller = "label_parsed") +
        ggplot2::theme_bw() +
        ggplot2::scale_colour_manual(values=colours)
    }
  }
  return(p)
}
