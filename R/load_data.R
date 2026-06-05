#' Load Plastic Waste Cleanup Data
#'
#' Loads the Break Free From Plastic dataset from a local CSV file.
#' The CSV should be placed in the `inst/extdata/` folder of the package.
#'
#' @return A tibble with columns including `country`, `year`, `volunteers`,
#'   `grand_total`, and plastic type columns (hdpe, ldpe, o, pet, pp, ps, pvc).
#'
#' @importFrom arrow read_parquet
#' @export
load_data <- function() {
  path <- system.file("extdata", "plastics.parquet", package = "tidyplastic")
  read_parquet(path)
}
