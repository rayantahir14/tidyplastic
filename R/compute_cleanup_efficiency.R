#' Compute Cleanup Efficiency by Country and Year
#'
#' Aggregates the raw plastics dataset to the country-year level
#' and calculates cleanup efficiency as total plastic collected
#' per volunteer.
#'
#' @param data A tibble returned by `clean_plastic_data()`.
#'
#' @return A tibble with columns `country`, `year`, `region`, `total_plastic`,
#'   `total_volunteers`, and `avg_efficiency`.
#'
#' @importFrom dplyr group_by summarise filter mutate
#' @importFrom rlang .data
#' @export


compute_cleanup_efficiency <- function(data = clean_plastic_data()) {
  validate_data_input(data, c("country", "year", "volunteers", "grand_total"),
                      call_name = "compute_cleanup_efficiency()")
  data |>
    dplyr::group_by(.data$country, .data$year, .data$region) |>
    dplyr::summarise(
      total_plastic = sum(grand_total, na.rm = TRUE),
      total_volunteers = sum(volunteers, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::filter(
      total_volunteers > 0,
      is.finite(total_plastic / total_volunteers)
    ) |>
    dplyr::mutate(
      avg_efficiency = total_plastic / total_volunteers
    )
}

