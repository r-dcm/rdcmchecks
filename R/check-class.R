#' Check that an object has the expected class
#'
#' @inheritParams abort_bad_argument
#' @param x The provided object to check.
#' @param class The expected class of the object as a string.
#'
#' @returns Nothing. Called for side effects.
#' @export
#'
#' @examples
#' singer <- S7::new_class("singer", properties = list(
#'   name = S7::class_character,
#'   birthday = S7::class_Date
#' ))
#'
#' taylor <- singer(name = "Taylor Swift",
#'                  birthday = as.Date("1989-12-13"))
#' check_S7(taylor)
check_S7 <- function(x, class,
                     arg = rlang::caller_arg(x),
                     call = rlang::caller_env()) {
  if (!inherits(x, "S7_object") && !inherits(x, class)) {
    msg <- paste0("{.arg ", arg, "} must be an S7 object with class ",
                  "{.cls ", class, "}")
    abort_bad_argument(arg = arg, custom = msg)
  }

  return(invisible(NULL))
}
