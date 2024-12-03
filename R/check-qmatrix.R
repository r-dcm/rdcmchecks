#' Check that a Q-matrix follows the expected structure
#'
#' The Q-matrix should be 1 row per item and 1 column per attribute, with an
#' optional additional column to store item identifiers. Each value of the
#' Q-matrix should be a 0 or 1 to indicate measurement of the attribute by the
#' given item. [clean_qmatrix()] calls [check_qmatrix()] to verify the expected
#' structure, and then performs additional data manipulation to provide standard
#' conventions. See details for additional information.
#'
#' @inheritParams abort_bad_argument
#' @param x The provided Q-matrix to check.
#' @param identifier The provided item identifier, as a character string. If no
#'   item identifier is present, the value should be `NULL` (the default).
#'
#' @details
#' In many instances, it's important to have standard conventions for a Q-matrix
#' so that we know what to expect (e.g., item identifiers, attribute names).
#' [clean_qmatrix()] provides this standardization. For the cleaned Q-matrix,
#' item identifiers and item names are removed. Additionally, all attributes are
#' renamed `att1`, `att2`, `att3`, etc. Finally, all columns are coerced to
#' integer values.
#'
#' To ensure downstream functions are able to identify the original
#' (pre-cleaned) values, [clean_qmatrix()] returns a list that includes the
#' cleaned Q-matrix, as well as metadata that includes look-ups from the
#' original to cleaned values.
#'
#'
#' @returns `check_qmatrix` return the original Q-matrix (if the checks pass) as
#'   a [tibble][tibble::tibble-package] with one row per item.
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



#' @returns `clean_qmatrix` returns a list with four elements:
#'  * `clean_qmatrix`: The cleaned Q-matrix
#'  * `attribute_names`: The real names of the attributes
#'  * `item_identifier`: The real name of the item identifier
#'  * `item_names`: The real names of the items
#'
#' @export
#' @rdname check_qmatrix
#' @examples
#'
#' example_qmatrix <- tibble::tibble(item = paste0("item_", 1:5),
#'                                   att_1 = c(0, 0, 1, 1, 1),
#'                                   att_2 = c(1, 1, 1, 0, 0))
#' clean_qmatrix(example_qmatrix, identifier = "item")
clean_qmatrix <- function(x, identifier = NULL,
                          arg = rlang::caller_arg(x),
                          call = rlang::caller_env()) {
  x <- check_qmatrix(x, identifier = identifier, arg = arg, call = call)

  if (is.null(identifier)) {
    x <- tibble::rowid_to_column(x, var = "item_id")
    identifier <- "item_id"
  }

  meta <- list(
    attribute_names = rlang::set_names(
      paste0("att", seq_len(ncol(x) - 1)),
      colnames(x)[-which(colnames(x) == identifier)]
    ),
    item_identifier = identifier,
    item_names = rlang::set_names(seq_len(nrow(x)), x[[identifier]])
  )

  clean_qmatrix <- x |>
    dplyr::select(-{{ identifier }}) |>
    dplyr::rename_with(.fn = \(x) meta$attribute_names[x]) |>
    dplyr::mutate(dplyr::across(dplyr::everything(), as.integer))

  c(list(clean_qmatrix = clean_qmatrix), meta)
}
