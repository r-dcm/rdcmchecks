#' Check the type of a vector against a supplied scalar
#'
#' Determines if a vector is of the same type as a provided scalar.
#'
#' @param col A vector of values (typically a data frame column).
#' @param missing A scalar indicating how missing values are encoded
#'   (e.g., -99, ".", `NA_integer`).
#'
#' @returns A logical scalar.
#'
#' @noRd
#' @examples
#' missing_type(rnorm(10), "-99")
#'
#' missing_type(rnorm(10), NA_real_)
#'
#' missing_type(rnorm(10), NA_integer_)
missing_type <- function(col, missing) {
  (typeof(col) == typeof(missing)) ||
    (is.numeric(col) && is.numeric(missing))
}
