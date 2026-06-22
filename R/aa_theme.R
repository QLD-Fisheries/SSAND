compact_null <- function(x) {
  x[!vapply(x, is.null, logical(1))]
}

# This helper means your plotting functions never need to know how theme_ssand() works internally. They just pass through any plot-specific theme overrides.
add_ssand_theme <- function(p,
                            text_size = NULL,
                            legend_text_size = NULL,
                            text_colour = NULL,
                            legend_text_colour = NULL,
                            legend_position = NULL,
                            legend_box = NULL,
                            legend_title_blank = NULL,
                            panel_border = NULL,
                            panel_border_colour = NULL,
                            xangle = NULL,
                            theme_extra = NULL) {

  theme_args <- compact_null(
    list(
      text_size = text_size,
      legend_text_size = legend_text_size,
      text_colour = text_colour,
      legend_text_colour = legend_text_colour,
      legend_position = legend_position,
      legend_box = legend_box,
      legend_title_blank = legend_title_blank,
      panel_border = panel_border,
      panel_border_colour = panel_border_colour,
      xangle = xangle,
      theme_extra = theme_extra
    ))

  p + do.call(theme_ssand, theme_args)
}



# Where to declare the default text_size = 12
ssand_style_defaults <- function() {
  list(
    text_size = 12,
    legend_text_size = 12,
    text_colour = "black",
    legend_text_colour = "black",
    legend_position = "top",
    legend_box = "horizontal",
    panel_border_colour = "black",
    legend_key_border = NA,
    xangle = 0,
    theme_extra = ggplot2::theme()
  )
}

# Then get_ssand_style() merges defaults + user overrides
get_ssand_style <- function() {
  defaults <- ssand_style_defaults()
  user <- getOption("ssand.style")

  if (is.null(user)) return(defaults)

  utils::modifyList(defaults, user)
}

# Then theme_ssand() uses those defaults
theme_ssand <- function(
    text_size = NULL,
    legend_text_size = NULL,
    text_colour = NULL,
    legend_text_colour = NULL,
    legend_position = NULL,
    legend_box = NULL,
    legend_title_blank = TRUE,
    panel_border = TRUE,
    panel_border_colour = NULL,
    xangle = NULL,
    legend_key_border = NULL,
    theme_extra = NULL
) {

  style <- get_ssand_style()

  text_size           <- text_size           %||% style$text_size
  legend_text_size    <- legend_text_size    %||% style$legend_text_size
  text_colour         <- text_colour         %||% style$text_colour
  legend_text_colour  <- legend_text_colour  %||% style$legend_text_colour
  legend_position     <- legend_position     %||% style$legend_position
  legend_box          <- legend_box          %||% style$legend_box
  panel_border_colour <- panel_border_colour %||% style$panel_border_colour
  xangle              <- xangle              %||% style$xangle
  legend_key_border   <- legend_key_border   %||% style$legend_key_border
  theme_extra         <- theme_extra         %||% style$theme_extra

  th <- ggplot2::theme_bw(base_size = text_size) +
    ggplot2::theme(
      text = ggplot2::element_text(size = text_size, colour = text_colour),
      legend.text = ggplot2::element_text(size = legend_text_size, colour = legend_text_colour),
      legend.position = legend_position,
      legend.box = legend_box,
      legend.key = ggplot2::element_rect(colour = legend_key_border),
      axis.text.x = ggplot2::element_text(
        angle = xangle,
        vjust = 0.5,
        hjust = ifelse(xangle == 90, 0, 0.5)
      )
    )

  if (legend_title_blank) {
    th <- th + ggplot2::theme(legend.title = ggplot2::element_blank())
  }

  if (panel_border) {
    th <- th + ggplot2::theme(
      panel.background = ggplot2::element_rect(fill = NA, colour = panel_border_colour)
    )
  }

  th + theme_extra
}


#
#
#
# # Define style ----
# theme_ssand <- function(
    #     text_size = 12,
#     legend_text_size = 12,
#     text_colour = "black",
#     legend_text_colour = "black",
#     legend_position = "top",
#     legend_box = "horizontal",
#     legend_title_blank = TRUE,
#     panel_border = TRUE,
#     xangle = 0
# ) {
#   th <- ggplot2::theme_bw() +
#     ggplot2::theme(
#       text = ggplot2::element_text(size = text_size, colour=text_colour),
#       legend.text = ggplot2::element_text(size = legend_text_size, colour=legend_text_colour),
#       legend.position = legend_position,
#       legend.box = legend_box,
#       legend.key = ggplot2::element_rect(colour = NA),
#       axis.text.x = ggplot2::element_text(angle = xangle, vjust = 0.5, hjust = ifelse(xangle == 90, 0, 0.5))
#     )
#
#   if (legend_title_blank) {
#     th <- th + ggplot2::theme(legend.title = ggplot2::element_blank())
#   }
#
#   if (panel_border) {
#     th <- th + ggplot2::theme(
#       panel.background = ggplot2::element_rect(fill = NA, colour = "black")
#     )
#   }
#
#   th
# }
#
# # CAAL plot:
# # ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
# #                panel.grid.minor = ggplot2::element_blank(),
# #                legend.position = legend_position,
# #                legend.justification = c(0.5,1),
# #                legend.box = "horizontal",
# #                legend.box.just = "left",
# #                legend.key.size = ggplot2::unit(1,"lines"),
# #                legend.text.align = 0,
# #                legend.key = ggplot2::element_blank(),
# #                legend.title = ggplot2::element_blank(),
# #                legend.background = ggplot2::element_blank(),
# #                legend.text = ggplot2::element_text(size = text_size))
#
#
# # Use style functions ----
# # Retrieves active style within each plotting function
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





# _________________________________
# Within specific plotting functions:
# set plot-specific override ONLY if user didn't specify
# text_colour <- text_colour %||% "blue"
# if (is.null(text_colour)) text_colour <- "blue"
