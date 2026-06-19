# Define style ----
theme_ssand <- function(
    text_size = 12,
    legend_text_size = 12,
    text_colour = "black",
    legend_text_colour = "black",
    legend_position = "top",
    legend_box = "horizontal",
    legend_title_blank = TRUE,
    panel_border = TRUE,
    xangle = 0
) {
  th <- ggplot2::theme_bw() +
    ggplot2::theme(
      text = ggplot2::element_text(size = text_size, colour=text_colour),
      legend.text = ggplot2::element_text(size = legend_text_size, colour=legend_text_colour),
      legend.position = legend_position,
      legend.box = legend_box,
      legend.key = ggplot2::element_rect(colour = NA),
      axis.text.x = ggplot2::element_text(angle = xangle, vjust = 0.5, hjust = ifelse(xangle == 90, 0, 0.5))
    )

  if (legend_title_blank) {
    th <- th + ggplot2::theme(legend.title = ggplot2::element_blank())
  }

  if (panel_border) {
    th <- th + ggplot2::theme(
      panel.background = ggplot2::element_rect(fill = NA, colour = "black")
    )
  }

  th
}

# Use style functions ----
# Retrieves active style within each plotting function
get_theme_ssand <- function() {
  getOption("ssand.theme", theme_ssand())
}
# Use get_theme_ssand()  in each plot function

set_theme_ssand <- function(theme) {
  options(ssand.theme = theme)
}

reset_theme_ssand <- function() {
  options(ssand.theme = theme_ssand())
}

use_theme_ssand <- function(...) {
  set_theme_ssand(theme_ssand(...))
}


reset_theme_ssand <- function() {
  options(ssand.theme = theme_ssand())
}


# catchplot(data, fleet_names = "Commercial")
#
# # This is user-facing. Declare at the start of the code/report:
# # Need to ensure all arguments are documents
# set_theme_ssand(theme_ssand(text_colour = "green", legend_text_colour = "blue"))
# reset_theme_ssand()
#
#
# data <- cpueplot_prep_SS(ss_mle, scenarios=1)
# cpueplot(data)
# cpueplot(data, xlim=c(1954,2023))



