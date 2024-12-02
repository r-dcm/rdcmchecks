#' Title
#'
#' @inheritParams abort_bad_argument
#' @param x The provided Q-matrix to check.
#' @param identifier The provided item identifier, as a character string. If no
#'   item identifier is present, the value should be `NULL` (the default).
#' @param item_levels Expected
#'
#' @returns A [tibble][tibble::tibble-package] with one row per item.
#' @export
#'
#' @examples
check_qmatrix <- function(x, identifier = NULL, item_levels = NULL,
                          arg = rlang::caller_arg(x),
                          call = rlang::caller_env()) {
  if (!is.null(identifier)) identifier <- rlang::enquo(identifier)

  if (!inherits(x, "data.frame")) {
    abort_bad_argument(arg = arg, must = "be a data frame", not = typeof(x),
                       call = call)
  }

  if (nrow(x) != length(item_levels) && !is.null(item_levels)) {
    abort_bad_argument(arg = arg,
                       must = glue::glue("have the same number of rows ",
                                         "as columns of items in `data`"),
                       call = call)
  }

  #check that item ids match item levels
  x <- check_item_levels(x, identifier, item_levels, arg = arg, call = call)

  if (!all(sapply(dplyr::select(x, -"item_id"), is.numeric))) {
    abort_bad_argument(arg = arg, must = "contain only numeric columns",
                       call = call)
  }
  x <- dplyr::mutate(x, dplyr::across(-"item_id", as.integer))

  if (!all(sapply(dplyr::select(x, -"item_id"),
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


check_item_levels <- function(x, identifier, item_levels,
                              arg = rlang::caller_arg(x),
                              call = rlang::caller_env()) {
  if (is.null(identifier) && !is.null(item_levels)) {
    x <- x |>
      dplyr::mutate(item_id = item_levels,
                    item_id = factor(.data$item_id, levels = item_levels),
                    .before = 1)
  } else if (!is.null(item_levels)) {
    item_names <- dplyr::pull(x, !!identifier)
    if (!all(item_levels %in% item_names)) {
      abort_bad_argument(
        arg = arg,
        must = glue::glue("include all items in `data`.
                          Missing items: {setdiff(item_levels, item_names)}"),
        call = call
      )
    }
    if (!all(item_names %in% item_levels)) {
      abort_bad_argument(
        arg = arg,
        must = glue::glue("only include items found in `data`.
                          Extra items: {paste(setdiff(item_names, item_levels),
                                                  collapse = ', ')}"),
        call = call
      )
    }
    x <- x |>
      dplyr::rename(item_id = !!identifier) |>
      dplyr::mutate(item_id = factor(.data$item_id, levels = item_levels)) |>
      dplyr::arrange(.data$item_id)
  } else if (is.null(identifier) && is.null(item_levels)) {
    x <- x |>
      dplyr::mutate(item_id = seq_len(dplyr::n()),
                    item_id = factor(.data$item_id, levels = .data$item_id),
                    .before = 1) |>
      dplyr::arrange(.data$item_id)
  } else if (!is.null(identifier) && is.null(item_levels)) {
    x <- x |>
      dplyr::rename(item_id = !!identifier) |>
      dplyr::mutate(item_id = factor(.data$item_id, levels = .data$item_id)) |>
      dplyr::arrange(.data$item_id)
  }

  return(x)
}
