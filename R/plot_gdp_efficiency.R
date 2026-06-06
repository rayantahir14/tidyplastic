#' Plot GDP Per Capita vs Cleanup Efficiency
#'
#' Produces an interactive plotly scatter plot of GDP per capita
#' vs cleanup efficiency, colored by region, with optional
#' per-region linear smoothers.
#'
#' @param data A tibble returned by `join_gdp()`.
#' @param regions A character vector of regions to display.
#'   Defaults to NULL which shows all regions.
#' @param log_axes Logical. If TRUE, both axes are log scaled.
#'   Defaults to TRUE.
#' @param show_smoother Logical. If TRUE, adds a per-region
#'   linear smoother. Defaults to TRUE.
#'
#' @return A plotly object.
#'
#' @importFrom dplyr filter mutate group_by
#' @importFrom plotly plot_ly add_lines layout config
#' @importFrom stringr str_to_title
#' @export
plot_gdp_efficiency <- function(data, regions = NULL, log_axes = TRUE, show_smoother = FALSE) {
  validate_data_input(data, c("country", "region", "gdp_per_capita_nominal", "avg_efficiency"),
                      call_name = "plot_gdp_efficiency()")

  if (!is.null(regions)) {
    data <- data |>
      dplyr::filter(region %in% regions)
  }

  data <- data |>
    dplyr::filter(
      !is.na(gdp_per_capita_nominal),
      !is.na(avg_efficiency),
      !is.na(region)
    ) |>
    dplyr::mutate(region_label = str_to_title(region))

  smoother_data <- data |>
    dplyr::group_by(region_label) |>
    dplyr::group_modify(~ {
      if (nrow(.x) < 2) return(tibble::tibble())
      fit <- lm(log10(avg_efficiency) ~ log10(gdp_per_capita_nominal), data = .x)
      x_seq <- 10^seq(
        log10(min(.x$gdp_per_capita_nominal, na.rm = TRUE)),
        log10(max(.x$gdp_per_capita_nominal, na.rm = TRUE)),
        length.out = 100
      )
      tibble::tibble(
        gdp_per_capita_nominal = x_seq,
        avg_efficiency = 10^predict(fit, newdata = tibble::tibble(gdp_per_capita_nominal = x_seq))
      )
    }) |>
    dplyr::ungroup()

  axis_type <- if (log_axes) "log" else "linear"

  p <- plot_ly(
    data = data,
    x = ~gdp_per_capita_nominal,
    y = ~avg_efficiency,
    color = ~region_label,
    type = "scatter",
    mode = "markers",
    marker = list(size = 10, opacity = 0.8),
    hovertext = ~paste0(
      "<b>", country, "</b>",
      "<br>Region: ", region_label,
      "<br>GDP per capita: $", round(gdp_per_capita_nominal, 0),
      "<br>Total volunteers: ", total_volunteers,
      "<br>Cleanup efficiency: ", round(avg_efficiency, 2)
    ),
    hoverinfo = "text"
  )

  if (show_smoother && nrow(smoother_data) > 0) {
    for (reg in unique(smoother_data$region_label)) {
      reg_data <- smoother_data |> dplyr::filter(region_label == reg)
      p <- p |>
        add_lines(
          data = reg_data,
          x = ~gdp_per_capita_nominal,
          y = ~avg_efficiency,
          color = ~region_label,
          inherit = FALSE,
          line = list(width = 2),
          hoverinfo = "skip",
          showlegend = FALSE
        )
    }
  }

  p |>
    layout(
      title = "GDP, Volunteers, and Cleanup Efficiency by Region",
      xaxis = list(title = "GDP per Capita", type = axis_type),
      yaxis = list(title = "Cleanup Efficiency", type = axis_type)
    ) |>
    config(displayModeBar = FALSE, responsive = TRUE)
}
