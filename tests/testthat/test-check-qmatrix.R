test_that("check qmatrix", {
  err <- rlang::catch_cnd(check_qmatrix("a", identifier = NULL))
  expect_s3_class(err, "rlang_error")
  expect_match(err$message, "data frame")

  test_q <- data.frame(
    item = paste0("I", 1:5),
    att1 = sample(0:1, 5, replace = TRUE),
    att2 = sample(0:1, 5, replace = TRUE)
  )
  err <- rlang::catch_cnd(check_qmatrix(test_q, identifier = NULL))
  expect_s3_class(err, "rlang_error")
  expect_match(err$message, "only numeric values of 0 or 1")

  test_q <- data.frame(
    item = sample(0:1, 5, replace = TRUE),
    att2 = sample(1:2, 5, replace = TRUE),
    att3 = sample(2:3, 5, replace = TRUE)
  )
  err <- rlang::catch_cnd(check_qmatrix(test_q, "check1", identifier = NULL))
  expect_s3_class(err, "rlang_error")
  expect_match(err$message, "only 0 or 1")

  test_q <- data.frame(
    item = paste0("I", 1:5),
    att1 = sample(0:1, 5, replace = TRUE),
    att2 = sample(0:1, 5, replace = TRUE)
  )
  err <- rlang::catch_cnd(check_qmatrix(test_q, identifier = "item_id"))
  expect_s3_class(err, "rlang_error")
  expect_match(err$message, "not found in")

  test1_q <- tibble::tibble(
    item = paste0("I_", 1:5),
    att1 = c(0, 1, 1, 0, 1),
    att2 = c(1, 0, 0, 1, 0),
    att3 = c(1, 1, 1, 0, 0)
  )
  test2_q <- tibble::tibble(
    item = paste0("I_", 1:5),
    att1 = c(0L, 1L, 1L, 0L, 1L),
    att2 = c(1L, 0L, 0L, 1L, 0L),
    att3 = c(1L, 1L, 1L, 0L, 0L)
  )
  test3_q <- tibble::tibble(
    att1 = c(0L, 1L, 1L, 0L, 1L),
    att2 = c(1L, 0L, 0L, 1L, 0L),
    att3 = c(1L, 1L, 1L, 0L, 0L)
  )
  test4_q <- data.frame(
    item = paste0("I_", 1:5),
    att1 = c(0, 1, 1, 0, 1),
    att2 = c(1, 0, 0, 1, 0),
    att3 = c(1, 1, 1, 0, 0)
  )
  test5_q <- data.frame(
    item = paste0("I_", 1:5),
    att1 = c(0L, 1L, 1L, 0L, 1L),
    att2 = c(1L, 0L, 0L, 1L, 0L),
    att3 = c(1L, 1L, 1L, 0L, 0L)
  )
  test6_q <- data.frame(
    att1 = c(0L, 1L, 1L, 0L, 1L),
    att2 = c(1L, 0L, 0L, 1L, 0L),
    att3 = c(1L, 1L, 1L, 0L, 0L)
  )
  check_q <- tibble::tibble(
    item = paste0("I_", 1:5),
    att1 = c(0L, 1L, 1L, 0L, 1L),
    att2 = c(1L, 0L, 0L, 1L, 0L),
    att3 = c(1L, 1L, 1L, 0L, 0L)
  )
  check_q_null <- tibble::tibble(
    att1 = c(0L, 1L, 1L, 0L, 1L),
    att2 = c(1L, 0L, 0L, 1L, 0L),
    att3 = c(1L, 1L, 1L, 0L, 0L)
  )
  expect_identical(check_qmatrix(test1_q, identifier = "item"), check_q)
  expect_identical(check_qmatrix(test2_q, identifier = "item"), check_q)
  expect_identical(check_qmatrix(test3_q, identifier = NULL), check_q_null)
  expect_identical(check_qmatrix(test4_q, identifier = "item"), check_q)
  expect_identical(check_qmatrix(test5_q, identifier = "item"), check_q)
  expect_identical(check_qmatrix(test6_q, identifier = NULL), check_q_null)
  expect_identical(check_qmatrix(check_q, identifier = "item"), check_q)
  expect_identical(check_qmatrix(check_q_null, identifier = NULL), check_q_null)
})

test_that("clean qmatrix", {
  # dtmr -----------------------------------------------------------------------
  cleaned <- clean_qmatrix(dcmdata::dtmr_qmatrix, identifier = "item")

  expect_equal(dim(cleaned$clean_qmatrix), dim(dcmdata::dtmr_qmatrix[, -1]))
  expect_equal(colnames(cleaned$clean_qmatrix), paste0("att", 1:4))
  expect_true(all(vapply(cleaned$clean_qmatrix, is.integer, logical(1))))

  expect_equal(
    names(cleaned$attribute_names),
    colnames(dcmdata::dtmr_qmatrix)[-1]
  )
  expect_equal(cleaned$item_identifier, colnames(dcmdata::dtmr_qmatrix)[1])
  expect_equal(names(cleaned$item_names), dcmdata::dtmr_qmatrix$item)

  # ecpe -----------------------------------------------------------------------
  cleaned <- clean_qmatrix(dcmdata::ecpe_qmatrix, identifier = "item_id")

  expect_equal(dim(cleaned$clean_qmatrix), dim(dcmdata::ecpe_qmatrix[, -1]))
  expect_equal(colnames(cleaned$clean_qmatrix), paste0("att", 1:3))
  expect_true(all(vapply(cleaned$clean_qmatrix, is.integer, logical(1))))

  expect_equal(
    names(cleaned$attribute_names),
    colnames(dcmdata::ecpe_qmatrix)[-1]
  )
  expect_equal(cleaned$item_identifier, colnames(dcmdata::ecpe_qmatrix)[1])
  expect_equal(names(cleaned$item_names), dcmdata::ecpe_qmatrix$item_id)

  # mdm ------------------------------------------------------------------------
  cleaned <- clean_qmatrix(dcmdata::mdm_qmatrix[, -1])

  expect_equal(dim(cleaned$clean_qmatrix), dim(dcmdata::mdm_qmatrix[, -1]))
  expect_equal(colnames(cleaned$clean_qmatrix), paste0("att", 1))
  expect_true(all(vapply(cleaned$clean_qmatrix, is.integer, logical(1))))

  expect_equal(
    names(cleaned$attribute_names),
    colnames(dcmdata::mdm_qmatrix)[-1]
  )
  expect_equal(cleaned$item_identifier, NULL)
  expect_equal(names(cleaned$item_names), as.character(1:4))
})
