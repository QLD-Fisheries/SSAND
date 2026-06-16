theme_ssand <- function() {
  # Set base theme and font family
  theme_bw() +
    # Overwrite base theme defaults
    theme(

    )
}


p <- p +
  ggplot2::scale_x_continuous(limits = as.numeric(xlim), breaks = xbreaks, labels = xlabels) +
  ggplot2::scale_y_continuous(limits = as.numeric(ylim), breaks = ybreaks, labels = ylabels) +
  ggplot2::theme_bw() +
  ggplot2::xlab(xlab) +
  ggplot2::ylab(ylab) +
  ggplot2::theme(legend.position=legend_position) +
  ggplot2::theme(legend.text = ggplot2::element_text(size=text_size)) +
  ggplot2::theme(text = ggplot2::element_text(size=text_size)) +
  ggplot2::theme(legend.box=legend_box) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = xangle, vjust = 0.5, hjust=ifelse(xangle==90,0,0.5)))


ssand_axes <- function(...) {
  if (missing(xbreaks)) {xbreaks <- pretty(xlim)}
  if (missing(ybreaks)) {ybreaks <- pretty(ylim)}
  if (missing(xlabels)) {xlabels <- xbreaks}
  if (missing(ylabels)) {ylabels <- ybreaks}

  p <- p +
    ggplot2::scale_x_continuous(limits = xlim, breaks = xbreaks, labels = xlabels) +
    ggplot2::scale_y_continuous(limits = ylim, breaks = ybreaks, labels = ylabels)
}
