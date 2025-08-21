#' Send an error message for an unexpected argument input
#'
#' @param arg The name of the argument.
#' @param must The requirement for input values that is not met.
#' @param not The current state of `argument` that is problematic.
#' @param footer Additional text to add to the error message.
#' @param custom A custom error message to override the default message of
#'   `must` + `not`.
#' @param call The call stack.
#'
#' @return An error message created by [cli::cli_abort()].
#' @export
#'
#' @examples
#' try(abort_bad_argument(arg = "my_arg", must = "be a character vector"))
abort_bad_argument <- function(
  arg,
  must = NULL,
  not = NULL,
  footer = NULL,
  custom = NULL,
  call = rlang::caller_env()
) {
  msg <- "{.arg {arg}} must {must}"
  if (!is.null(not)) {
    msg <- paste0(msg, "; not {not}")
  }
  if (!is.null(custom)) {
    msg <- custom
    footer <- NULL
  }

  cli::cli_abort(msg, footer = footer, call = call)
}
