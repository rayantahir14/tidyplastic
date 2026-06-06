#' Clean and Standardize Plastic Waste Data
#'
#' Standardizes country names and joins region and population data
#' from the REST Countries API.
#'
#' @param data A tibble returned by `load_data()`.
#'
#' @return A tibble with additional columns `country_clean`, `region`,
#'   and `population`.
#'
#' @importFrom dplyr mutate left_join select
#' @importFrom stringr str_squish
#' @export
clean_plastic_data <- function(data) {
  validate_data_input(data, c("country"), call_name = "clean_plastic_data()")

  plastic_data_clean <- data |>
    mutate(
      country_clean = str_squish(country),
      country_clean = dplyr::recode(
        country_clean,
        "USA" = "United States",
        "US" = "United States",
        "UK" = "United Kingdom",
        "Czech Republic" = "Czechia",
        "Republic of the Congo" = "Congo",
        "Democratic Republic of the Congo" = "DR Congo",
        "Cote D_ivoire" = "Cote d'Ivoire",
        "Taiwan_ Republic of China (ROC)" = "Taiwan"
      )
    )

  region_list <- c("africa", "americas", "asia", "europe", "oceania")

  country_region_lookup <- purrr::map_dfr(region_list, function(region) {
    url <- glue::glue(
      "https://restcountries.com/v3.1/region/{region}?fields=name,population,region"
    )
    res <- httr::GET(url)
    httr::stop_for_status(res)
    txt <- httr::content(res, as = "text", encoding = "UTF-8")
    df <- jsonlite::fromJSON(txt, flatten = TRUE)
    tibble::as_tibble(df) |>
      dplyr::transmute(
        country_clean = dplyr::coalesce(name.common, name.official),
        region = region,
        population = population
      )
  }) |>
    dplyr::mutate(
      country_clean = str_squish(country_clean),
      country_clean = dplyr::recode(
        country_clean,
        "United States of America" = "United States",
        "Russian Federation" = "Russia",
        "Korea (Republic of)" = "South Korea",
        "Côte d'Ivoire" = "Cote d'Ivoire"
      )
    )

  plastic_data_clean |>
    left_join(
      country_region_lookup |> select(country_clean, region, population),
      by = "country_clean"
    )
}
