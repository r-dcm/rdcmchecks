test_that("check data", {
  err <- rlang::catch_cnd(check_data("a", identifier = NULL))
  expect_s3_class(err, "rlang_error")
  expect_match(err$message, "data frame")

  test_d <- data.frame(
    resp = paste0("I", 1:5),
    item1 = sample(0:1, 5, replace = TRUE),
    item2 = sample(0:1, 5, replace = TRUE)
  )
  err <- rlang::catch_cnd(check_data(test_d, identifier = NULL))
  expect_s3_class(err, "rlang_error")
  expect_match(err$message, "only numeric values of 0 or 1")

  test_d <- data.frame(
    resp = sample(0:1, 5, replace = TRUE),
    item2 = sample(1:2, 5, replace = TRUE),
    item3 = sample(2:3, 5, replace = TRUE)
  )
  err <- rlang::catch_cnd(check_data(test_d, "check1", identifier = NULL))
  expect_s3_class(err, "rlang_error")
  expect_match(err$message, "only 0 or 1")

  test_d <- data.frame(
    resp = paste0("I", 1:5),
    item1 = sample(0:1, 5, replace = TRUE),
    item2 = sample(0:1, 5, replace = TRUE)
  )
  err <- rlang::catch_cnd(check_data(test_d, identifier = "resp_id"))
  expect_s3_class(err, "rlang_error")
  expect_match(err$message, "not found in")

  test1_d <- tibble::tibble(
    resp = paste0("I_", 1:5),
    item1 = c(0, 1, 1, 0, 1),
    item2 = c(1, 0, 0, 1, 0),
    item3 = c(1, 1, 1, 0, 0)
  )
  test2_d <- tibble::tibble(
    resp = paste0("I_", 1:5),
    item1 = c(0L, 1L, 1L, 0L, 1L),
    item2 = c(1L, 0L, 0L, 1L, 0L),
    item3 = c(1L, 1L, 1L, 0L, 0L)
  )
  test3_d <- tibble::tibble(
    item1 = c(0L, 1L, 1L, 0L, 1L),
    item2 = c(1L, 0L, 0L, 1L, 0L),
    item3 = c(1L, 1L, 1L, 0L, 0L)
  )
  test4_d <- data.frame(
    resp = paste0("I_", 1:5),
    item1 = c(0, 1, 1, 0, 1),
    item2 = c(1, 0, 0, 1, 0),
    item3 = c(1, 1, 1, 0, 0)
  )
  test5_d <- data.frame(
    resp = paste0("I_", 1:5),
    item1 = c(0L, 1L, 1L, 0L, 1L),
    item2 = c(1L, 0L, 0L, 1L, 0L),
    item3 = c(1L, 1L, 1L, 0L, 0L)
  )
  test6_d <- data.frame(
    item1 = c(0L, 1L, 1L, 0L, 1L),
    item2 = c(1L, 0L, 0L, 1L, 0L),
    item3 = c(1L, 1L, 1L, 0L, 0L)
  )
  check_d <- tibble::tibble(
    resp = paste0("I_", 1:5),
    item1 = c(0L, 1L, 1L, 0L, 1L),
    item2 = c(1L, 0L, 0L, 1L, 0L),
    item3 = c(1L, 1L, 1L, 0L, 0L)
  )
  check_d_null <- tibble::tibble(
    item1 = c(0L, 1L, 1L, 0L, 1L),
    item2 = c(1L, 0L, 0L, 1L, 0L),
    item3 = c(1L, 1L, 1L, 0L, 0L)
  )
  expect_identical(check_data(test1_d, identifier = "resp"), check_d)
  expect_identical(check_data(test2_d, identifier = "resp"), check_d)
  expect_identical(check_data(test3_d, identifier = NULL), check_d_null)
  expect_identical(check_data(test4_d, identifier = "resp"), check_d)
  expect_identical(check_data(test5_d, identifier = "resp"), check_d)
  expect_identical(check_data(test6_d, identifier = NULL), check_d_null)
  expect_identical(check_data(check_d, identifier = "resp"), check_d)
  expect_identical(check_data(check_d_null, identifier = NULL), check_d_null)
})

test_that("clean data", {
  # dtmr -----------------------------------------------------------------------
  cleaned_q <- clean_qmatrix(dcmdata::dtmr_qmatrix, identifier = "item")
  cleaned_d <- clean_data(
    dcmdata::dtmr_data,
    identifier = "id",
    cleaned_qmatrix = cleaned_q
  )

  expect_equal(length(cleaned_d), 5)
  expect_equal(
    names(cleaned_d),
    c(
      "clean_data",
      "item_identifier",
      "item_names",
      "respondent_identifier",
      "respondent_names"
    )
  )

  expect_equal(ncol(cleaned_d$clean_data), 3)
  expect_equal(
    nrow(cleaned_d$clean_data),
    nrow(dcmdata::dtmr_data) * (ncol(dcmdata::dtmr_data) - 1)
  )
  expect_equal(colnames(cleaned_d$clean_data), c("resp_id", "item_id", "score"))
  expect_equal(
    vapply(cleaned_d$clean_data, typeof, character(1)),
    c(resp_id = "integer", item_id = "integer", score = "integer")
  )
  expect_equal(
    vapply(cleaned_d$clean_data, is.factor, logical(1)),
    c(resp_id = TRUE, item_id = TRUE, score = FALSE)
  )

  expect_equal(cleaned_d$item_identifier, colnames(dcmdata::dtmr_qmatrix)[1])
  expect_equal(names(cleaned_d$item_names), dcmdata::dtmr_qmatrix$item)
  expect_equal(cleaned_d$respondent_identifier, colnames(dcmdata::dtmr_data)[1])
  expect_equal(
    names(cleaned_d$respondent_names),
    as.character(dcmdata::dtmr_data$id)
  )

  # ecpe -----------------------------------------------------------------------
  cleaned_q <- clean_qmatrix(dcmdata::ecpe_qmatrix, identifier = "item_id")
  cleaned_d <- clean_data(dcmdata::ecpe_data[, -1], cleaned_qmatrix = cleaned_q)

  expect_equal(length(cleaned_d), 5)
  expect_equal(
    names(cleaned_d),
    c(
      "clean_data",
      "item_identifier",
      "item_names",
      "respondent_identifier",
      "respondent_names"
    )
  )

  expect_equal(ncol(cleaned_d$clean_data), 3)
  expect_equal(
    nrow(cleaned_d$clean_data),
    nrow(dcmdata::ecpe_data) * (ncol(dcmdata::ecpe_data) - 1)
  )
  expect_equal(colnames(cleaned_d$clean_data), c("resp_id", "item_id", "score"))
  expect_equal(
    vapply(cleaned_d$clean_data, typeof, character(1)),
    c(resp_id = "integer", item_id = "integer", score = "integer")
  )
  expect_equal(
    vapply(cleaned_d$clean_data, is.factor, logical(1)),
    c(resp_id = TRUE, item_id = TRUE, score = FALSE)
  )

  expect_equal(cleaned_d$item_identifier, colnames(dcmdata::ecpe_qmatrix)[1])
  expect_equal(names(cleaned_d$item_names), dcmdata::ecpe_qmatrix$item_id)
  expect_equal(cleaned_d$respondent_identifier, "resp_id")
  expect_equal(
    names(cleaned_d$respondent_names),
    as.character(seq_len(nrow(dcmdata::ecpe_data)))
  )

  # mdm ------------------------------------------------------------------------
  cleaned_q <- clean_qmatrix(dcmdata::mdm_qmatrix[, -1])
  cleaned_d <- clean_data(
    dcmdata::mdm_data,
    identifier = "respondent",
    cleaned_qmatrix = cleaned_q
  )

  expect_equal(length(cleaned_d), 5)
  expect_equal(
    names(cleaned_d),
    c(
      "clean_data",
      "item_identifier",
      "item_names",
      "respondent_identifier",
      "respondent_names"
    )
  )

  expect_equal(ncol(cleaned_d$clean_data), 3)
  expect_equal(
    nrow(cleaned_d$clean_data),
    nrow(dcmdata::mdm_data) * (ncol(dcmdata::mdm_data) - 1)
  )
  expect_equal(colnames(cleaned_d$clean_data), c("resp_id", "item_id", "score"))
  expect_equal(
    vapply(cleaned_d$clean_data, typeof, character(1)),
    c(resp_id = "integer", item_id = "integer", score = "integer")
  )
  expect_equal(
    vapply(cleaned_d$clean_data, is.factor, logical(1)),
    c(resp_id = TRUE, item_id = TRUE, score = FALSE)
  )

  expect_equal(cleaned_d$item_identifier, "item_id")
  expect_equal(names(cleaned_d$item_names), colnames(dcmdata::mdm_data[, -1]))
  expect_equal(cleaned_d$respondent_identifier, colnames(dcmdata::mdm_data)[1])
  expect_equal(
    names(cleaned_d$respondent_names),
    as.character(dcmdata::mdm_data$respondent)
  )
})

test_that("reconcile item names", {
  # different numbers of items -------------------------------------------------
  cleaned_q <- clean_qmatrix(dcmdata::dtmr_qmatrix, identifier = "item")
  my_dat <- dcmdata::mdm_data

  err <- rlang::catch_cnd(
    clean_data(
      x = my_dat,
      identifier = "respondent",
      cleaned_qmatrix = cleaned_q
    )
  )
  expect_s3_class(err, "rlang_error")
  expect_match(err$message, "same number of response columns")

  # name mismatch --------------------------------------------------------------
  cleaned_q <- clean_qmatrix(dcmdata::ecpe_qmatrix, identifier = "item_id")
  my_dat <- dcmdata::ecpe_data |>
    dplyr::rename(J1 = "E1", J13 = "E13")

  err <- rlang::catch_cnd(
    clean_data(x = my_dat, identifier = "resp_id", cleaned_qmatrix = cleaned_q)
  )
  expect_s3_class(err, "rlang_error")
  expect_match(err$message, "contain items that match those in the Q-matrix")
  expect_match(
    err$footer[1],
    paste0("Items found in `my_dat` but not ", "`cleaned_q`:.*J1.* and .*J13.*")
  )
  expect_match(
    err$footer[2],
    paste0("Items found in `cleaned_q` but not ", "`my_dat`:.*E1.* and .*E13.*")
  )
})

test_that("clean new data", {
  # with Q-matrix identifiers --------------------------------------------------
  ## errors with new items -----------------------------------------------------
  cleaned_q <- clean_qmatrix(dcmdata::dtmr_qmatrix, identifier = "item")
  new_data <- dplyr::mutate(dcmdata::dtmr_data, new_item = 0)

  err <- rlang::catch_cnd(
    clean_data(
      new_data,
      identifier = "id",
      cleaned_qmatrix = cleaned_q,
      valid_names = cleaned_q$item_names
    )
  )
  expect_s3_class(err, "rlang_error")
  expect_match(
    err$message,
    "must contain items that match those in the Q-matrix"
  )
  expect_match(err$footer, "Items found in `new_data` but not `cleaned_q`:")

  ## passes with missing items -------------------------------------------------
  cleaned_q <- clean_qmatrix(dcmdata::ecpe_qmatrix, identifier = "item_id")
  new_data <- dplyr::select(dcmdata::ecpe_data, 1:5)
  cleaned_d <- clean_data(
    new_data,
    identifier = "resp_id",
    cleaned_qmatrix = cleaned_q,
    valid_names = cleaned_q$item_names
  )

  expect_equal(length(cleaned_d), 5)
  expect_equal(
    names(cleaned_d),
    c(
      "clean_data",
      "item_identifier",
      "item_names",
      "respondent_identifier",
      "respondent_names"
    )
  )

  expect_equal(ncol(cleaned_d$clean_data), 3)
  expect_equal(
    nrow(cleaned_d$clean_data),
    nrow(dcmdata::ecpe_data) * (ncol(new_data) - 1)
  )
  expect_equal(colnames(cleaned_d$clean_data), c("resp_id", "item_id", "score"))
  expect_equal(
    vapply(cleaned_d$clean_data, typeof, character(1)),
    c(resp_id = "integer", item_id = "integer", score = "integer")
  )
  expect_equal(
    vapply(cleaned_d$clean_data, is.factor, logical(1)),
    c(resp_id = TRUE, item_id = TRUE, score = FALSE)
  )

  expect_equal(cleaned_d$item_identifier, colnames(dcmdata::ecpe_qmatrix)[1])
  expect_equal(names(cleaned_d$item_names), dcmdata::ecpe_qmatrix$item_id)
  expect_equal(cleaned_d$respondent_identifier, "resp_id")
  expect_equal(
    names(cleaned_d$respondent_names),
    as.character(seq_len(nrow(new_data)))
  )

  # without Q-matrix identifiers -----------------------------------------------
  ## errors with new items -----------------------------------------------------
  cleaned_q <- clean_qmatrix(dcmdata::mdm_qmatrix[, -1])
  orig_data <- clean_data(
    dcmdata::mdm_data,
    identifier = "respondent",
    cleaned_qmatrix = cleaned_q
  )
  new_data <- dplyr::mutate(dcmdata::mdm_data, mdm5 = 0)

  err <- rlang::catch_cnd(
    clean_data(
      new_data,
      identifier = "respondent",
      cleaned_qmatrix = cleaned_q,
      valid_names = orig_data$item_names
    )
  )
  expect_s3_class(err, "rlang_error")
  expect_match(
    err$message,
    "must contain items that match those in the Q-matrix"
  )
  expect_match(err$footer, "Items found in `new_data` but not `cleaned_q`:")

  ## passes with missing items -------------------------------------------------
  cleaned_q <- clean_qmatrix(dcmdata::dtmr_qmatrix[, -1])
  orig_data <- clean_data(
    dcmdata::dtmr_data,
    identifier = "id",
    cleaned_qmatrix = cleaned_q
  )
  new_data <- dcmdata::dtmr_data |>
    dplyr::select(1:7) |>
    dplyr::slice_sample(n = 10)
  cleaned_d <- clean_data(
    new_data,
    identifier = "id",
    cleaned_qmatrix = cleaned_q,
    valid_names = orig_data$item_names
  )

  expect_equal(length(cleaned_d), 5)
  expect_equal(
    names(cleaned_d),
    c(
      "clean_data",
      "item_identifier",
      "item_names",
      "respondent_identifier",
      "respondent_names"
    )
  )

  expect_equal(ncol(cleaned_d$clean_data), 3)
  expect_equal(
    nrow(cleaned_d$clean_data),
    nrow(new_data) * (ncol(new_data) - 1)
  )
  expect_equal(colnames(cleaned_d$clean_data), c("resp_id", "item_id", "score"))
  expect_equal(
    vapply(cleaned_d$clean_data, typeof, character(1)),
    c(resp_id = "integer", item_id = "integer", score = "integer")
  )
  expect_equal(
    vapply(cleaned_d$clean_data, is.factor, logical(1)),
    c(resp_id = TRUE, item_id = TRUE, score = FALSE)
  )

  expect_equal(cleaned_d$item_identifier, "item_id")
  expect_equal(names(cleaned_d$item_names), names(orig_data$item_names))
  expect_equal(cleaned_d$respondent_identifier, colnames(new_data)[1])
  expect_equal(names(cleaned_d$respondent_names), as.character(new_data$id))
})
