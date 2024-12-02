#' Check that a Q-matrix follows the expected structure
#'
#' The Q-matrix should be 1 row per item and 1 column per attribute, with an
#' optional additional column to store item identifiers. Each value of the
#' Q-matrix should be a 0 or 1 to indicate measurement of the attribute by the
#' given item.
#'
#' @inheritParams abort_bad_argument
#' @param x The provided Q-matrix to check.
#' @param identifier The provided item identifier, as a character string. If no
#'   item identifier is present, the value should be `NULL` (the default).
#'
#' @returns A [tibble][tibble::tibble-package] with one row per item.
#' @export
#'
#' @examples
#' example_qmatrix <- tibble::tibble(item = paste0("item_", 1:5),
#'                                   att_1 = c(0, 0, 1, 1, 1),
#'                                   att_2 = c(1, 1, 1, 0, 0))
#' check_qmatrix(example_qmatrix, identifier = "item")
check_qmatrix <- function(x, identifier = NULL,
                          arg = rlang::caller_arg(x),
                          call = rlang::caller_env()) {
  if (!is.null(identifier)) identifier <- rlang::enquo(identifier)

  if (!inherits(x, "data.frame")) {
    abort_bad_argument(arg = arg, must = "be a data frame", not = typeof(x),
                       call = call)
  }

  if (!all(sapply(dplyr::select(x, -{{ identifier }}), is.numeric))) {
    abort_bad_argument(arg = arg,
                       must = paste0("contain only numeric values of 0 or 1 ",
                                     "in attribute columns"),
                       call = call)
  }
  x <- dplyr::mutate(x, dplyr::across(-{{ identifier }}, as.integer))

  if (!all(sapply(dplyr::select(x, -{{ identifier }}),
                  function(.x) all(.x %in% c(0L, 1L))))) {
    abort_bad_argument(arg = arg,
                       must = "contain only 0 or 1 in attribute columns",
                       call = call)
  }

  if (!tibble::is_tibble(x)) {
    tibble::as_tibble(x)
  } else {
    x
  }
}
