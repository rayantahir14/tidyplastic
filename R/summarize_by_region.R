#' Summarize Cleanup Efficiency by Region
#'
#' Aggregates plastic waste and cleanup event data to the region level,
#' computing total plastic collected, total events, and average efficiency.
#'
#' @param data A tibble returned by `clean_plastic_data()`.
#'
#' @return A tibble with columns `region`, `total_plastic`, `total_events`,
#'   and `avg_efficiency`, arranged by descending efficiency.
#'
#' @importFrom dplyr filter group_by summarise arrange desc
#' @importFrom stringr str_to_title
#' @export
summarize_by_region <- function(data) {
  validate_data_input(data, c("region", "grand_total", "num_events"),
                      call_name = "summarize_by_region()")

  data |>
    filter(!is.na(region), !is.na(num_events), num_events > 0) |>
    group_by(region) |>
    summarise(
      total_plastic = sum(grand_total, na.rm = TRUE),
      total_events = sum(num_events, na.rm = TRUE),
      avg_efficiency = total_plastic / total_events,
      .groups = "drop"
    ) |>
    arrange(desc(avg_efficiency))
}
