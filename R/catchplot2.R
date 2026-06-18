catchplot2 <- function(
    data,
    xlab = "Year",
    ylab = NULL,
    xbreaks = NULL,
    ybreaks = NULL,
    xlabels = NULL,
    ylabels = NULL,
    xlim = NULL,
    ylim = NULL,
    xangle = NULL,
    financial_year = FALSE,
    fleet_names = NULL,
    colours = NULL,
    legend_position = "top",
    reverse = FALSE,
    scales = "free",
    ncol = 2,
    ...
) {
  check_cols(data, c("date", "value", "fleet", "scenario"))

  data <- apply_scenarios(data, ...)
  data <- apply_fleet_names(data, fleet_names)

  if (is.null(ylab)) {
    ylab <- default_catch_ylab(data)
  }

  x_axis <- build_x_axis(
    x = data$date,
    xlim = xlim,
    xbreaks = xbreaks,
    xlabels = xlabels,
    financial_year = financial_year,
    show_dates_on_axis = FALSE,
    expand_upper = 1,
    xangle = xangle,
    is_date = TRUE
  )

  y_axis <- build_y_axis(
    y = data$value,
    ylim = ylim,
    ybreaks = ybreaks,
    ylabels = ylabels,
    lower = 0
  )

  p <- ggplot2::ggplot(data) +
    ggplot2::geom_bar(
      ggplot2::aes(x = date, y = value, fill = as.factor(fleet)),
      stat = "identity",
      position = ggplot2::position_stack(reverse = reverse)
    ) +
    ggplot2::xlab(xlab) +
    ggplot2::ylab(ylab) +
    theme_ssand(legend_position = legend_position) +
    ggplot2::scale_fill_manual(values = colours)

  p <- add_x_scale_continuous(p, x_axis)  # or date version depending on representation
  p <- add_y_scale_continuous(p, y_axis)
  p <- add_scenario_facets(p, data, scales = scales, ncol = ncol)

  p
}
