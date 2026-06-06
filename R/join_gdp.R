#' Join GDP Per Capita Data to Cleanup Efficiency Tibble
#'
#' Enriches a cleanup efficiency tibble with GDP per capita data
#' from the bundled plotting data file in `inst/extdata/`.
#'
#' @param data A tibble returned by `compute_cleanup_efficiency()`.
#' @param year An integer year to fetch GDP data for. Defaults to 2019.
#'
#' @return The input tibble with two additional columns:
#'   `iso` (ISO3 country code) and `gdp_per_capita_nominal`.
#'
#' @importFrom dplyr mutate left_join distinct select
#' @importFrom readr read_csv
#' @importFrom countrycode countrycode
#' @export
join_gdp <- function(data, year = 2019) {
  validate_data_input(data, c("country", "total_volunteers", "total_plastic"),
                      call_name = "join_gdp()")

  gdp_path <- system.file("extdata", "plotting-data.csv", package = "tidyplastic")
  gdp_data <- readr::read_csv(gdp_path, show_col_types = FALSE) |>
    dplyr::distinct(iso, .keep_all = TRUE) |>
    dplyr::select(iso, gdp_per_capita_nominal)

  data |>
    dplyr::mutate(
      iso = countrycode::countrycode(country, "country.name", "iso3c", warn = FALSE)
    ) |>
    dplyr::left_join(gdp_data, by = "iso")
}
