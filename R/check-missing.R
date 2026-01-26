#' Check that the missing data indicator is a scalar
#'
#' The indicator for missing data should be a length 1, non-NULL scalar.
#'
#' @inheritParams abort_bad_argument
#' @param x The provided missing data indicator to check.
#'
#' @returns The original value (if the checks pass).
#'
#' @export
#' @examples
#' check_missing(NA)
check_missing <- function(
  x,
  arg = rlang::caller_arg(x),
  call = rlang::caller_env()
) {
  if (is.null(x)) {
    abort_bad_argument(
      arg = arg,
      must = cli::format_message("not be {.code NULL}")
    )
  }

  if (length(x) != 1) {
    abort_bad_argument(
      arg = arg,
      must = "be of length 1",
      not = length(x)
    )
  }

  x
}
