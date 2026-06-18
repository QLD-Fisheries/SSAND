# Data input warnings ----
is_mcmc <- function(data) {"med" %in% names(data)}

check_data_columns <- function(variables) {
  for (i in 1:length(variables)) {
    if (!variables[i] %in% names(data)) {warning(paste0("Input data is missing '",variables[i],"' column"))}
  }
}

# Set up axes ----
build_x_axis <- function(
    x,
    xlim = NULL,
    xbreaks = NULL,
    xlabels = NULL,
    financial_year = FALSE,
    show_dates_on_axis = FALSE,
    expand_upper = 0,
    xangle = NULL,
    is_date = inherits(x, "Date")
) {
  if (is.null(xlim)) {
    if (is_date) {
      xlim <- c(min(x, na.rm = TRUE), max(x, na.rm = TRUE) + expand_upper)
    } else {
      xlim <- c(min(x, na.rm = TRUE), max(x, na.rm = TRUE) + expand_upper)
    }
  }

  if (is.null(xbreaks)) {
    xbreaks <- pretty(xlim)
  }

  if (is.null(xlabels)) {
    if (is_date) {
      if (financial_year && !show_dates_on_axis) {
        xlabels <- paste0(lubridate::year(xbreaks) - 1, "\u2013", lubridate::year(xbreaks))
      } else if (!financial_year && !show_dates_on_axis) {
        xlabels <- lubridate::year(xbreaks)
      } else {
        xlabels <- xbreaks
      }
    } else {
      if (financial_year) {
        xlabels <- paste0(xbreaks - 1, "\u2013", xbreaks)
      } else {
        xlabels <- xbreaks
      }
    }
  }

  if (is.null(xangle)) {
    xangle <- ifelse(financial_year, 90, 0)
  }

  list(
    limits = xlim,
    breaks = xbreaks,
    labels = xlabels,
    angle = xangle
  )
}


build_y_axis <- function(y, ylim = NULL, ybreaks = NULL, ylabels = NULL, lower = 0) {
  if (is.null(ylim)) {
    ylim <- c(lower, max(y, na.rm = TRUE))
  }
  if (is.null(ybreaks)) ybreaks <- pretty(ylim)
  if (is.null(ylabels)) ylabels <- ybreaks

  list(
    limits = ylim,
    breaks = ybreaks,
    labels = ylabels
  )
}



add_x_scale_date <- function(p, axis) {
  p +
    ggplot2::scale_x_date(
      limits = axis$limits,
      breaks = axis$breaks,
      labels = axis$labels
    ) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(
        angle = axis$angle,
        vjust = 0.5,
        hjust = ifelse(axis$angle == 90, 0, 0.5)
      )
    )
}

add_x_scale_continuous <- function(p, axis) {
  p +
    ggplot2::scale_x_continuous(
      limits = axis$limits,
      breaks = axis$breaks,
      labels = axis$labels
    ) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(
        angle = axis$angle,
        vjust = 0.5,
        hjust = ifelse(axis$angle == 90, 0, 0.5)
      )
    )
}

add_y_scale_continuous <- function(p, axis) {
  p +
    ggplot2::scale_y_continuous(
      limits = axis$limits,
      breaks = axis$breaks,
      labels = axis$labels
    )
}



# Set up scenarios ----

apply_scenarios <- function(data, scenarios = NULL, scenario_labels = NULL, scenario_order = NULL) {
  if (!is.null(scenarios)) {
    data <- dplyr::filter(data, scenario %in% scenarios)
  }

  if (is.null(scenario_labels)) {
    data <- dplyr::mutate(data, scenario_labels = as.factor(paste0("Scenario ", scenario)))
  } else {
    lookup <- data.frame(
      scenario = unique(data$scenario),
      scenario_labels = scenario_labels
    )
    data <- data |>
      dplyr::left_join(lookup, by = "scenario") |>
      dplyr::mutate(scenario_labels = as.factor(scenario_labels))
  }

  if (!is.null(scenario_order)) {
    scenario_order <- c(scenario_order, setdiff(levels(data$scenario_labels), scenario_order))
    data$scenario_labels <- factor(data$scenario_labels, levels = scenario_order)
  }

  data
}


# Set up fleets ----

relabel_factor <- function(data, var, labels) {
  var_name <- rlang::as_name(rlang::ensym(var))
  vals <- unique(data[[var_name]])
  lookup <- data.frame(
    value = vals,
    label = labels
  )

  data |>
    dplyr::left_join(
      stats::setNames(lookup, c(var_name, "label")),
      by = var_name
    ) |>
    dplyr::mutate(
      !!rlang::sym(var_name) := label
    ) |>
    dplyr::select(-label)
}


# Set up facets ----

add_scenario_facets <- function(p, data, scales = "fixed", ncol = 2) {
  if (length(unique(data$scenario)) > 1) {
    p + ggplot2::facet_wrap(~scenario_labels, scales = scales, ncol = ncol)
  } else {
    p
  }
}


# MCMC ----
sample_mcmc_runs <- function(data, sample = NULL, keep_med = "trajectory") {
if (is.null(sample)) return(data)

data_keep <- data |> dplyr::filter(med == keep_med)
data_sample <- data |>
  dplyr::filter(med == "MCMC") |>
  dplyr::filter(rownum %in% sample(unique(data$rownum), size = sample))

dplyr::bind_rows(data_keep, data_sample)
}

compute_ci <- function(data, value_var, group_vars, CI_range = 0.95) {
  value_var <- rlang::ensym(value_var)

  data |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) |>
    dplyr::summarise(
      upper = stats::quantile(!!value_var, probs = 1 - (1 - CI_range) / 2, na.rm = TRUE),
      lower = stats::quantile(!!value_var, probs = (1 - CI_range) / 2, na.rm = TRUE),
      .groups = "drop"
    )
}



