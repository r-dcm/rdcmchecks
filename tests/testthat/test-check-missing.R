test_that("check missing", {
  err <- rlang::catch_cnd(check_missing(NULL))
  expect_s3_class(err, "rlang_error")
  expect_match(err$message, "not be `NULL`")

  err <- rlang::catch_cnd(check_missing(c(9, -99)))
  expect_s3_class(err, "rlang_error")
  expect_match(err$message, "be of length 1")

  err <- rlang::catch_cnd(check_missing(integer()))
  expect_s3_class(err, "rlang_error")
  expect_match(err$message, "be of length 1")

  expect_identical(check_missing("."), ".")
  expect_identical(check_missing(-99), -99)
  expect_identical(check_missing(9L), 9L)
  expect_identical(check_missing(NA), NA)
  expect_identical(check_missing(NA_real_), NA_real_)
  expect_identical(check_missing(NA_integer_), NA_integer_)
  expect_identical(check_missing(NA_character_), NA_character_)
})
