#' Check that data follows the expected structure
#'
#' The data should be 1 row per respondent and 1 column per item, with an
#' optional additional column to store respondent identifiers. Each value of the
#' data should be a 0 or 1 to indicate the response to the item by the given
#' respondent. [clean_data()] calls [check_data()] to verify the expected
#' structure, and then performs additional data manipulation to provide standard
#' conventions. See details for additional information.
#'
#' @inheritParams abort_bad_argument
#' @param x The provided data to check.
#' @param identifier The provided respondent identifier, as a character string.
#'   If no respondent identifier is present, the value should be `NULL` (the
#'   default).
#' @param missing A expression specifying how missing data in `x` is coded
#'   (e.g., `NA`, `"."`, `-99`). The default is `NA`.
#'
#' @details
#' In many instances, it's important to have standard conventions for a data
#' object so that we know what to expect (e.g., respondent and item identifiers,
#' data types). [clean_data()] provides this standardization. Cleaned data is
#' returned in long format, with one row per response. Respondent and item
#' columns are encoded as factors, and responses are coerced to integer values.
#'
#' To ensure downstream functions are able to identify the original
#' (pre-cleaned) values, [clean_data()] returns a list that includes the cleaned
#' data, as well as metadata that includes look-ups from the original to cleaned
#' values.
#'
#'
#' @returns `check_data` returns the original data (if the checks pass) as a
#'   [tibble][tibble::tibble-package], with missing data (i.e., `missing`)
#'   replaced with `NA`.
#' @export
#'
#' @examples
#' example_data <- tibble::tibble(person = 1:10,
#'                                item1 = sample(0:1, 10, replace = TRUE),
#'                                item2 = sample(0:1, 10, replace = TRUE),
#'                                item3 = sample(0:1, 10, replace = TRUE))
#' check_data(example_data, identifier = "person")
check_data <- function(x, identifier = NULL, missing = NA,
                       arg = rlang::caller_arg(x),
                       call = rlang::caller_env()) {
  if (!is.null(identifier) && !(identifier %in% colnames(x))) {
    abort_bad_argument(arg = arg,
                       custom = cli::format_message(
                         c("Specified {.arg identifier}, {.val {identifier}},",
                           "not found in {.arg {arg}}")
                       ),
                       call = call)
  } else if (!is.null(identifier)) {
    identifier <- rlang::enquo(identifier)
  }

  if (!inherits(x, "data.frame")) {
    abort_bad_argument(arg = arg, must = "be a data frame", not = typeof(x),
                       call = call)
  }

  # replace missing values with NA
  x <- x |>
    dplyr::mutate(
      dplyr::across(
        dplyr::where(~ typeof(.x) == typeof(missing)) & -{{ identifier }},
        \(x) dplyr::na_if(x, missing)
      )
    ) |>
    dplyr::mutate(dplyr::across(dplyr::where(\(x) is.character(x)),
                                \(x) readr::parse_guess(x)))

  if (!all(sapply(dplyr::select(x, -{{ identifier }}), is.numeric))) {
    abort_bad_argument(arg = arg,
                       must = paste0("contain only numeric values of 0 or 1 ",
                                     "in response columns"),
                       call = call)
  }
  x <- dplyr::mutate(x, dplyr::across(-{{ identifier }}, as.integer))

  if (!all(sapply(dplyr::select(x, -{{ identifier }}),
                  function(.x) all(.x %in% c(0L, 1L))))) {
    abort_bad_argument(arg = arg,
                       must = "contain only 0 or 1 in response columns",
                       call = call)
  }

  if (!tibble::is_tibble(x)) {
    tibble::as_tibble(x)
  } else {
    x
  }
}

#' @param cleaned_qmatrix A cleaned Q-matrix, from [clean_qmatrix()].
#' @param arg_qmatrix A character string with the name of the argument used to
#'   provide the Q-matrix.
#' @param valid_names An optional named vector of items (e.g., from a previous
#'   call to [clean_data()]). Used when checking new data objects for
#'   consistency with items used to estimate a model.
#'
#' @returns `clean_data` returns a list with five elements:
#'  * `clean_data`: The cleaned data
#'  * `item_identifier`: The real name of the item identifier
#'  * `item_names`: The real names of the items
#'  * `respondent_identifier`: The real name of the respondent identifier
#'  * `respondent_names`: The real names of the respondents
#'
#' @export
#' @rdname check_data
#' @examples
#' example_qmatrix <- tibble::tibble(item = paste0("item", 1:3),
#'                                   att_1 = c(0, 0, 1),
#'                                   att_2 = c(1, 1, 1))
#'
#' example_data <- tibble::tibble(person = 1:10,
#'                                item1 = sample(0:1, 10, replace = TRUE),
#'                                item2 = sample(0:1, 10, replace = TRUE),
#'                                item3 = sample(0:1, 10, replace = TRUE))
#'
#' qmatrix <- clean_qmatrix(example_qmatrix, identifier = "item")
#' clean_data(example_data, identifier = "person",
#'            cleaned_qmatrix = qmatrix)
clean_data <- function(x, identifier = NULL, missing = NA, cleaned_qmatrix,
                       arg_qmatrix = rlang::caller_arg(cleaned_qmatrix),
                       valid_names = NULL,
                       arg = rlang::caller_arg(x),
                       call = rlang::caller_env()) {
  arg <- arg
  x <- check_data(x, identifier = identifier, missing = missing,
                  arg = arg, call = call)

  if (is.null(identifier)) {
    x <- tibble::rowid_to_column(x, var = "resp_id")
    identifier <- "resp_id"
  }

  data_names <- rlang::set_names(seq_len(ncol(x) - 1),
                                 colnames(x)[-which(colnames(x) == identifier)])
  meta <- c(
    reconcile_item_names(data_names = data_names,
                         qmat_names = cleaned_qmatrix$item_names,
                         qmat_id = cleaned_qmatrix$item_identifier,
                         arg_data = arg,
                         arg_qmat = arg_qmatrix,
                         valid_names = valid_names,
                         call = call),
    list(
      respondent_identifier = identifier,
      respondent_names = rlang::set_names(seq_len(nrow(x)), x[[identifier]])
    )
  )

  clean_data <- x |>
    tidyr::pivot_longer(cols = -{{ identifier }}, names_to = "item_id",
                        values_to = "score") |>
    dplyr::filter(!is.na(.data$score)) |>
    dplyr::rename(resp_id = {{ identifier }}) |>
    dplyr::mutate(
      resp_id = factor(.data$resp_id, levels = names(meta$respondent_names)),
      item_id = factor(.data$item_id, levels = names(meta$item_names)),
      score = as.integer(.data$score)
    ) |>
    dplyr::arrange(.data$resp_id, .data$item_id)

  c(list(clean_data = clean_data), meta)
}


#' Check for consistent item names
#'
#' Item names can be provided in two places: As an identifier column in the
#' Q-matrix, and as column names in the data. [reconcile_item_names()] ensures
#' that the information from both sources (if provided) is consistent.
#'
#' @inheritParams abort_bad_argument
#' @param data_names Item names provided as column names in the data.
#' @param qmat_names Item names provided as identifiers in the Q-matrix.
#' @param qmat_id The name of the identifier column in the Q-matrix.
#' @param arg_data A character string with the name of the argument used to
#'   provide the data.
#' @param arg_qmat A character string with the name of the argument used to
#'   provide the Q-matrix.
#'
#' @returns A list with two elements:
#'  * `item_identifier`: The real name of the item identifier
#'  * `item_names`: The real names of the items
#' @noRd
reconcile_item_names <- function(data_names, qmat_names, qmat_id,
                                 arg_data, arg_qmat, valid_names, call) {

  if ((length(data_names) != length(qmat_names)) && is.null(valid_names)) {
    abort_bad_argument(
      arg = arg_data,
      must = cli::format_message(c("have the same number of response columns",
                                   "as there are items in the Q-matrix",
                                   "specified by {.arg {arg_qmat}}")),
      footer = c(
        cli::format_message(
          c(`!` = "Response columns found in: {length(data_names)}")
        ),
        cli::format_message(
          c(`!` = "Items found in Q-matrix: {length(qmat_names)}")
        )
      ),
      call = call
    )
  }

  if (is.null(qmat_id) && is.null(valid_names)) {
    qmat_id <- "item_id"
    qmat_names <- rlang::set_names(qmat_names, names(data_names))
  } else if (is.null(qmat_id) && !is.null(valid_names)) {
    qmat_id <- "item_id"
    qmat_names <- valid_names
  }
  extr_data <- setdiff(names(data_names), names(qmat_names))
  miss_data <- setdiff(names(qmat_names), names(data_names))

  if (length(extr_data) || (length(miss_data) && is.null(valid_names))) {
    extr_msg <- if (length(extr_data)) {
      cli::format_message(
        c(`i` = "Items found in {.arg {arg_data}} but not {.arg {arg_qmat}}:",
          "{.val {cli::cli_vec(extr_data)}}")
      )
    }
    miss_msg <- if (length(miss_data) && is.null(valid_names)) {
      cli::format_message(
        c(`i` = "Items found in {.arg {arg_qmat}} but not {.arg {arg_data}}:",
          "{.val {cli::cli_vec(miss_data)}}")
      )
    }

    abort_bad_argument(
      arg = arg_data,
      must = cli::format_message(c("contain items that match those in the",
                                   "Q-matrix specified by {.arg {arg_qmat}}")),
      footer = c(extr_msg, miss_msg),
      call = call
    )
  }

  list(item_identifier = qmat_id,
       item_names = qmat_names)
}
