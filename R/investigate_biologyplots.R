# Copyright 2024 Fisheries Queensland

# This file is part of SSAND.
# SSAND is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
# SSAND is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
# You should have received a copy of the GNU General Public License along with SSAND. If not, see <https://www.gnu.org/licenses/>.

#' Box plot for raw biological data
#'
#' @param data A table of raw length and age data. Each row is a different sample. Columns are labelled according to your set up in the "_var" arguments of this function.
#' @param xlab Label for x-axis (character)
#' @param ylab Label for y-axis (character)
#' @param x_var Variable to be plotted on x-axis (e.g. "year")
#' @param y_var Variable to be plotted on x-axis (e.g. "fl")
#' @param facet_var Variable by which plot is to be faceted (e.g. "region")
#' @param MLS_x Optional. The value of the minimum legal size, if a horizontal line is to be added.
#' @param MLS_y Optional. The value of the minimum legal size, if a vertical line is to be added.
#' @param scales Scaled for facet wrap. Default is 'fixed'.
#' @param ncol Number of columns for facet wrap. Default is 2.
#' @param extract_data Set to TRUE to return data instead of plot. Default is FALSE.
#'
#' @return Box plot for raw biological data
#' @export
#'
#' @examples
#' \dontrun{rawbiological_boxplot(data, x_var = 'year', xlab='Year',
#'                                 y_var = 'fl', ylab = 'Fork length (cm)')}
#' \dontrun{rawbiological_boxplot(data, x_var = 'year', xlab='Year',
#'                     y_var = 'fl', ylab = 'Fork length (cm)',
#'                     facet_var='region')}
rawbiological_boxplot <- function(data,
                                  xlab = NULL,
                                  ylab = NULL,
                                  x_var = "year", # adjlen, agegrp, agemth, agedays
                                  y_var = "fl", # adjlen, agegrp, agemth, agedays
                                  facet_var = NULL, # sector, sex, LTMPSamplingRegion
                                  MLS_x = NULL,
                                  MLS_y = NULL,
                                  scales = 'fixed',
                                  ncol = 3,
                                  extract_data = FALSE) {

  p <- ggplot2::ggplot(data) +
    ggplot2::geom_boxplot(ggplot2::aes(x=.data[[x_var]],y=.data[[y_var]],group=.data[[x_var]])) +
    ggplot2::theme_bw() +
    ggplot2::ylim(0,NA)

  if (!missing(xlab)) {p <- p + ggplot2::xlab(xlab)}
  if (!missing(ylab)) {p <- p + ggplot2::ylab(ylab)}

  if (!missing(MLS_x)) {
    p <- p +
      ggplot2::geom_vline(ggplot2::aes(xintercept=MLS_x, colour="MLS", linetype="MLS")) +
      ggplot2::scale_linetype_manual("", values="dashed") +
      ggplot2::scale_colour_manual("", values="#AD3D25") +
      ggplot2::theme(legend.position = "top")
  }

  if (!missing(MLS_y)) {
    p <- p +
      ggplot2::geom_hline(ggplot2::aes(yintercept=MLS_y,colour="MLS", linetype="MLS")) +
      ggplot2::scale_linetype_manual("", values="dashed") +
      ggplot2::scale_colour_manual("", values="#AD3D25") +
      ggplot2::theme(legend.position = "top")
  }

  if (!missing(facet_var)) {
    if (length(facet_var)==1) {
      p <- p + ggplot2::facet_wrap(~.data[[facet_var]], scales=scales, ncol = ncol)
    } else {
      facet_formula <- as.formula(paste(facet_var[1], "~", facet_var[2]))
      p <- p + ggplot2::facet_grid(facet_formula, scales=scales)
    }
  }

  if (extract_data) {return(data)} else {return(p)}

}

#' A bar plot for raw biological data
#'
#' @param data A table of raw length and age data. Each row is a different sample. Columns are labelled according to your set up in the "_var" arguments of this function.
#' @param xlab Label for x-axis (character)
#' @param ylab Label for y-axis (character)
#' @param x_var Variable to be plotted on x-axis (e.g. "year")
#' @param y_var Variable to be plotted on x-axis (e.g. "fl")
#' @param facet_var Variable by which plot is to be faceted (e.g. "region")
#' @param fill_var Variable by which plot is to be filled (e.g. "sexcode")
#' @param MLS_x Optional. The value of the minimum legal size, if a horizontal line is to be added.
#' @param MLS_y Optional. The value of the minimum legal size, if a vertical line is to be added.
#' @param scales Scaled for facet wrap. Default is 'fixed'.
#' @param ncol Number of columns for facet wrap. Default is 2.
#' @param legend_title Title of legend
#' @param dodge Set to TRUE to offset bars, set to FALSE to stack them
#' @param axis_angle Angle of labels on x-axis. Default is 0.
#' @param extract_data Set to TRUE to return data instead of plot. Default is FALSE.
#' @param colours A vector of colours used to fill the plot
#'
#' @return Bar plot for raw biological data
#' @export
#'
#' @examples
#' \dontrun{rawbiological_barplot(data = data |> dplyr::filter(!is.na(agegrp)),
#' x_var = 'year', xlab='Year', y_var = 'n', ylab = 'Number of samples',
#' fill_var = 'sexcode')}
rawbiological_barplot <- function(data,
                                  xlab = NULL,
                                  ylab = NULL,
                                  x_var = "year", # adjlen, agegrp, agemth, agedays
                                  y_var = "fl", # adjlen, agegrp, agemth, agedays
                                  facet_var = NULL, # sector, sex, LTMPSamplingRegion
                                  fill_var = NULL, # sector, sex, LTMPSamplingRegion
                                  MLS_x = NULL,
                                  MLS_y = NULL,
                                  scales = 'fixed',
                                  ncol = 3,
                                  legend_title = "none",
                                  dodge = FALSE,
                                  axis_angle = NULL,
                                  extract_data = FALSE,
                                  colours = rep(fq_palette("alisecolours"),10)) {

  if (y_var %in% c("n","p")) {
    if (missing(fill_var)  && missing(facet_var))   {data <- data |> dplyr::group_by(.data[[x_var]])}
    if (!missing(fill_var) && missing(facet_var))  {data <- data |> dplyr::group_by(.data[[x_var]], .data[[fill_var]])}
    if (missing(fill_var)  && !missing(facet_var))  {
      if (length(facet_var)==1) {data <- data |> dplyr::group_by(.data[[x_var]], .data[[facet_var]])}
      if (length(facet_var)>1)  {data <- data |> dplyr::group_by(.data[[x_var]], .data[[facet_var[1]]], .data[[facet_var[2]]])}
    }
    if (!missing(fill_var) && !missing(facet_var)) {
      if (length(facet_var)==1) {data <- data |> dplyr::group_by(.data[[x_var]], .data[[fill_var]], .data[[facet_var]])}
      if (length(facet_var)>1)  {data <- data |> dplyr::group_by(.data[[x_var]], .data[[fill_var]], .data[[facet_var[1]]], .data[[facet_var[2]]])}
    }

    data <- data |>
      dplyr::summarise(n = dplyr::n(), .groups='drop') |>
      dplyr::group_by(.data[[x_var]]) |>
      dplyr::mutate(total = sum(n)) |>
      dplyr::ungroup() |>
      dplyr::mutate(p = n/total)
  }

  p <- ggplot2::ggplot(data)

  if (missing(fill_var)) {
    p <- p + ggplot2::geom_bar(ggplot2::aes(x=.data[[x_var]],y=.data[[y_var]]), stat='identity')
  } else {
    if (dodge) {
      p <- p + ggplot2::geom_bar(ggplot2::aes(x=.data[[x_var]],y=.data[[y_var]],fill=.data[[fill_var]]), stat='identity', position='dodge2')
    } else {
      p <- p + ggplot2::geom_bar(ggplot2::aes(x=.data[[x_var]],y=.data[[y_var]],fill=.data[[fill_var]]), stat='identity')
    }
  }

  p <- p +
    ggplot2::theme_bw() +
    ggplot2::ylim(0,NA)

  if (!missing(xlab)) {p <- p + ggplot2::xlab(xlab)}
  if (!missing(ylab)) {p <- p + ggplot2::ylab(ylab)}

  if (!missing(MLS_x)) {
    p <- p +
      ggplot2::geom_vline(ggplot2::aes(xintercept=MLS_x, colour="MLS", linetype="MLS")) +
      ggplot2::scale_linetype_manual("", values="dashed") +
      ggplot2::scale_colour_manual("", values="#AD3D25") +
      ggplot2::theme(legend.position = "top")
  }

  if (!missing(MLS_y)) {
    p <- p +
      ggplot2::geom_hline(ggplot2::aes(yintercept=MLS_y,colour="MLS", linetype="MLS")) +
      ggplot2::scale_linetype_manual("", values="dashed") +
      ggplot2::scale_colour_manual("", values="#AD3D25") +
      ggplot2::theme(legend.position = "top")
  }

  if (!missing(facet_var)) {
    if (length(facet_var)==1) {
      p <- p + ggplot2::facet_wrap(~.data[[facet_var]], scales=scales, ncol = ncol, dir='v')
    } else {
      facet_formula <- as.formula(paste(facet_var[1], "~", facet_var[2]))
      p <- p + ggplot2::facet_grid(facet_formula, scales=scales)
    }
  }


  if (legend_title == "none") {
    p <- p +
      ggplot2::theme(legend.title = ggplot2::element_blank()) +
      ggplot2::scale_fill_manual(name="", values=colours)

  } else {
    p <- p + ggplot2::theme(legend.title = legend_title) +
      ggplot2::scale_fill_manual(values=colours)
  }

  if (!missing(axis_angle)) {
    p <- p + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = axis_angle, vjust = 0.5, hjust=1))
  }

  if (extract_data) {return(data)} else {return(p)}
}



#' Scatter plot for raw biological data
#'
#' @param data A table of raw length and age data. Each row is a different sample. Columns are labelled according to your set up in the "_var" arguments of this function.
#' @param xlab Label for x-axis (character)
#' @param ylab Label for y-axis (character)
#' @param x_var Variable to be plotted on x-axis (e.g. "year")
#' @param y_var Variable to be plotted on x-axis (e.g. "fl")
#' @param facet_var Variable by which plot is to be faceted (e.g. "region")
#' @param MLS_x Optional. The value of the minimum legal size, if a horizontal line is to be added.
#' @param MLS_y Optional. The value of the minimum legal size, if a vertical line is to be added.
#' @param scales Scaled for facet wrap. Default is 'fixed'.
#' @param ncol Number of columns for facet wrap. Default is 2.
#' @param legend_title Title of legend
#' @param show_vonBert Set to TRUE to show von Bertalanffy fitted curves
#' @param show_vonBert_only Set to TRUE to show only von Bertalanffy fitted curves. This overrides the facetting function.
#' @param legend_title Legend title
#' @param legend_position Legend position
#' @param extract_data Set to TRUE to return data instead of plot. Default is FALSE.
#'
#' @return Scatter plot for raw biological data
#' @export
#' @importFrom stats as.formula nls nls.control
#'
#' @examples
#' \dontrun{rawbiological_scatterplot(mac3, x_var = 'agegrp',
#' xlab = 'Age group (years)', y_var = 'fl', ylab = 'Fork length (cm)',
#' facet_var='region', show_vonBert=TRUE, show_vonBert_only=TRUE, legend_position='top')}
rawbiological_scatterplot <- function (data,
                                       xlab = NULL,
                                       ylab = NULL,
                                       x_var = "agegrp",
                                       y_var = "fl",
                                       facet_var = NULL,
                                       MLS_x = NULL,
                                       MLS_y = NULL,
                                       scales = 'fixed',
                                       ncol = 1,
                                       show_vonBert = FALSE,
                                       show_vonBert_only = FALSE,
                                       legend_title = "none",
                                       legend_position = "none",
                                       extract_data = FALSE) {

  if (show_vonBert) {

    facets <- data[[facet_var]] |> unique() |> sort()

    vonBert <- data.frame()
    vonBert_params = data.frame()

    for (i in facets){

      tmp <- data

      tmp <- tmp |>
        dplyr::filter(tmp[[facet_var]] == i)

      tmp <- tmp |>
        dplyr::filter(!is.na(tmp[[x_var]]))

      x <- tmp[[x_var]]
      y <- tmp[[y_var]]

      model <- nls(y ~ L_0*exp(-kappa *x) + L_inf*(1-exp(-kappa*x)),
                   start = list(L_inf = 87.99, kappa = 0.078, L_0 = 18.618),
                   control = nls.control(maxiter= 10000, tol=0.00045, warnOnly=TRUE))

      tmp <- summary(model)$parameters |>
        as.data.frame() |>
        tibble::rownames_to_column("param") |>
        dplyr::select(param, value = Estimate, se = `Std. Error`) |>
        dplyr::mutate(region = i)

      vonBert <- rbind(vonBert,
                       data.frame(age = x, cm = y) |>
                         dplyr::mutate(fit = tmp[3,2]*exp(-tmp[2,2] *age) + tmp[1,2] *(1-exp(-tmp[2,2]*age))) |>
                         dplyr::mutate(region = as.factor(i)))
    }

    if (!show_vonBert_only) {
      p <- ggplot2::ggplot(data = vonBert, ggplot2::aes(x = age, y = cm, colour = region)) +
        ggplot2::geom_jitter(alpha = 0.2, width = 0.1, height = 10) +
        ggplot2::geom_line(ggplot2::aes(y = fit), linewidth = 1, colour = "black")
    } else {
      p <- ggplot2::ggplot(data = vonBert, ggplot2::aes(x = age, y = cm, colour = region)) +
        ggplot2::geom_line(ggplot2::aes(y = fit), linewidth = 1)
    }

  } else {
    p <- ggplot2::ggplot(data) +
      ggplot2::geom_point(ggplot2::aes(x=.data[[x_var]], y=.data[[y_var]]),colour=.data[[y_var]])
  }

  if (!missing(MLS_x)) {
    p <- p +
      ggplot2::geom_vline(ggplot2::aes(xintercept=MLS_x, colour="MLS", linetype="MLS")) +
      ggplot2::scale_linetype_manual("", values="dashed") +
      ggplot2::scale_colour_manual("", values="#AD3D25") +
      ggplot2::theme(legend.position = "top")
  }

  if (!missing(MLS_y)) {
    p <- p +
      ggplot2::geom_hline(ggplot2::aes(yintercept=MLS_y,colour="MLS", linetype="MLS")) +
      ggplot2::scale_linetype_manual("", values="dashed") +
      ggplot2::scale_colour_manual("", values="#AD3D25") +
      ggplot2::theme(legend.position = "top")
  }

  if (!show_vonBert_only) {
    if (!missing(facet_var)) {
      if (length(facet_var)==1) {
        p <- p + ggplot2::facet_wrap(~.data[[facet_var]], scales=scales, ncol = ncol, dir='v')
      } else {
        facet_formula <- as.formula(paste(facet_var[1], "~", facet_var[2]))
        p <- p + ggplot2::facet_grid(facet_formula, scales=scales)
      }
    }
  }

  if (legend_title == "none") {
    p <- p + ggplot2::theme(legend.title = ggplot2::element_blank())
  } else {
    p <- p + ggplot2::theme(legend.title = legend_title)
  }

  if (!missing(xlab)) {p <- p + ggplot2::xlab(xlab)}
  if (!missing(ylab)) {p <- p + ggplot2::ylab(ylab)}

  p <- p +
    ggplot2::theme_bw() +
    ggplot2::ylim(0,NA) +
    ggplot2::theme(legend.position = legend_position)

  if (extract_data) {return(data)} else {return(p)}
}




#' Heat plot for raw biological data
#'
#' @param data A table of raw length and age data. Each row is a different sample. Columns are labelled according to your set up in the "_var" arguments of this function.
#' @param xlab Label for x-axis (character)
#' @param ylab Label for y-axis (character)
#' @param x_var Variable to be plotted on x-axis (e.g. "year")
#' @param y_var Variable to be plotted on x-axis (e.g. "fl")
#' @param facet_var Variable by which plot is to be faceted (e.g. "region")
#' @param MLS_x Optional. The value of the minimum legal size, if a horizontal line is to be added.
#' @param MLS_y Optional. The value of the minimum legal size, if a vertical line is to be added.
#' @param scales Scaled for facet wrap. Default is 'fixed'.
#' @param ncol Number of columns for facet wrap. Default is 2.
#' @param legend_title Title of legend
#' @param legend_position Legend position
#' @param fill_var Variable by which plot is to be filled (e.g. "sexcode")
#' @param log_transform Set to TRUE to log transform the fill scale
#' @param show_values Set to TRUE to show values on tiles
#' @param text_size Text size
#' @param extract_data Set to TRUE to return data instead of plot. Default is FALSE.
#'
#' @return Heat plot for raw biological data
#' @export
#'
#' @examples
#' \dontrun{rawbiological_heatplot(mac3, facet_var = "region")}
rawbiological_heatplot <- function (data,
                                    x_var = "month",
                                    xlab = "Month",
                                    y_var = "year",
                                    ylab = "Year",
                                    fill_var = "n",
                                    facet_var = NULL,
                                    MLS_x = NULL,
                                    MLS_y = NULL,
                                    scales = 'fixed',
                                    ncol = 1,
                                    legend_title = "Number of samples",
                                    legend_position = "right",
                                    log_transform = FALSE,
                                    show_values = TRUE,
                                    text_size = 2,
                                    extract_data = FALSE
) {

  if (fill_var %in% c("n","p")) {
    if (missing(facet_var))  {data <- data |> dplyr::group_by(.data[[x_var]], .data[[y_var]])}
    if (!missing(facet_var)) {
      if (length(facet_var)==1) {data <- data |> dplyr::group_by(.data[[x_var]], .data[[y_var]], .data[[facet_var]])}
      if (length(facet_var)>1)  {data <- data |> dplyr::group_by(.data[[x_var]], .data[[y_var]], .data[[facet_var[1]]], .data[[facet_var[2]]])}
    }

    data <- data |>
      dplyr::summarise(n = dplyr::n(), .groups='drop') |>
      dplyr::group_by(.data[[x_var]],.data[[y_var]]) |>
      dplyr::mutate(total = sum(n)) |>
      dplyr::ungroup() |>
      dplyr::mutate(p = n/total)
  }

  p <- ggplot2::ggplot(data) +
    ggplot2::theme_bw()

  if (!log_transform) {
    p <- p +
      ggplot2::geom_tile(ggplot2::aes(x=.data[[x_var]],y=.data[[y_var]],fill=.data[[fill_var]]))
  } else {
    p <- p +
      ggplot2::geom_tile(ggplot2::aes(x=.data[[x_var]],y=.data[[y_var]],fill=log(.data[[fill_var]])))
  }


  if (show_values) {
    p <- p +
      ggplot2::geom_text(ggplot2::aes(x=.data[[x_var]],y=.data[[y_var]],label=.data[[fill_var]]), size=text_size, colour="white")
  }

  if (!missing(MLS_x)) {
    p <- p +
      ggplot2::geom_vline(ggplot2::aes(xintercept=MLS_x, colour="MLS", linetype="MLS")) +
      ggplot2::scale_linetype_manual("", values="dashed") +
      ggplot2::scale_colour_manual("", values="#AD3D25") +
      ggplot2::theme(legend.position = "top")
  }

  if (!missing(MLS_y)) {
    p <- p +
      ggplot2::geom_hline(ggplot2::aes(yintercept=MLS_y,colour="MLS", linetype="MLS")) +
      ggplot2::scale_linetype_manual("", values="dashed") +
      ggplot2::scale_colour_manual("", values="#AD3D25") +
      ggplot2::theme(legend.position = "top")
  }


  if (!missing(facet_var)) {
    if (length(facet_var)==1) {
      p <- p + ggplot2::facet_wrap(~.data[[facet_var]], scales=scales, ncol = ncol, dir='v')
    } else {
      facet_formula <- as.formula(paste(facet_var[1], "~", facet_var[2]))
      p <- p + ggplot2::facet_grid(facet_formula, scales=scales)
    }
  }

  if (legend_title == "none") {
    p <- p + ggplot2::theme(legend.title = ggplot2::element_blank())
  } else {
    p <- p + ggplot2::guides(fill=ggplot2::guide_legend(title=legend_title))
  }


  p <- p +
    ggplot2::theme(legend.position = legend_position)

  if (grepl(x_var,"month") && inherits(data[[x_var]],"numeric")) {
    p <- p +
      ggplot2::scale_x_continuous(breaks=1:12) +
      ggplot2::theme(panel.grid.minor = ggplot2::element_blank())
  }
  if (grepl(x_var,"mth") && inherits(data[[x_var]],"numeric")) {
    p <- p +
      ggplot2::scale_x_continuous(breaks=1:12) +
      ggplot2::theme(panel.grid.minor = ggplot2::element_blank())
  }
  if (grepl(x_var,"Month") && inherits(data[[x_var]],"numeric")) {
    p <- p +
      ggplot2::scale_x_continuous(breaks=1:12) +
      ggplot2::theme(panel.grid.minor = ggplot2::element_blank())
  }

  p <- p +
    ggplot2::xlab(xlab) +
    ggplot2::ylab(ylab)

  if (extract_data) {return(data)} else {return(p)}
}
