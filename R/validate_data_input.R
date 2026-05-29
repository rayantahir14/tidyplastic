#' Validate a Data Frame Input for Package Functions
#'
#' Checks that `data` is a data frame (or tibble) and that all required
#' columns are present. Used at the top of `compute_cleanup_efficiency()`,
#' `join_gdp()`, and `plot_cleanup_efficiency()` to give clear error messages.
#'
#' @param data The object to validate.
#' @param required_cols A character vector of column names that must be
#'   present in `data`.
#' @param call_name A string naming the calling function, used in the
#'   error message. Defaults to `"this function"`.
#'
#' @return Invisibly returns `TRUE` if all checks pass, otherwise stops
#'   with an informative error.
validate_data_input <- function(
    data,
    required_cols,
    call_name = "this function") {
  if (!is.data.frame(data)) {
    stop(
      call_name, " expects a data frame or tibble, ",
      "but received an object of class: ",
      paste(class(data), collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  missing_cols <- setdiff(required_cols, names(data))

  if (length(missing_cols) > 0) {
    stop(
      call_name, " is missing required column(s): ",
      paste(missing_cols, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  invisible(TRUE)
}
