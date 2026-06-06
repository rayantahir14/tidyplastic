#' Test Association Between GDP Group and Dominant Plastic Type
#'
#' Runs a chi-square test to assess whether GDP group and dominant plastic
#' type are statistically associated.
#'
#' @param data A tibble containing columns `gdp_group` and `dominant_type`.
#'
#' @return A tibble with columns `chi_square_statistic`, `degrees_of_freedom`,
#'   and `p_value`.
#'
#' @importFrom tibble tibble
#' @export
test_gdp_plastic_association <- function(data) {
  validate_data_input(data, c("gdp_group", "dominant_type"),
                      call_name = "test_gdp_plastic_association()")

  table_data <- table(data$gdp_group, data$dominant_type)
  test <- chisq.test(table_data)

  tibble::tibble(
    chi_square_statistic = round(test$statistic, 2),
    degrees_of_freedom = test$parameter,
    p_value = round(test$p.value, 4)
  )
}
