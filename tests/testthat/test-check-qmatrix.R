test_that("check qmatrix", {
  err <- rlang::catch_cnd(check_qmatrix("a", identifier = NULL,
                                        item_levels = NA))
  expect_s3_class(err, "rlang_error")
  expect_match(err$message, "data frame")

  test_q <- data.frame(item = paste0("I", 1:5),
                       att1 = sample(0:1, 5, replace = TRUE),
                       att2 = sample(0:1, 5, replace = TRUE))
  err <- rlang::catch_cnd(check_qmatrix(test_q, identifier = NULL,
                                        item_levels = paste0("I", 1:5)))
  expect_s3_class(err, "rlang_error")
  expect_match(err$message, "only numeric columns")

  test_q <- data.frame(item = sample(0:1, 5, replace = TRUE),
                       att2 = sample(1:2, 5, replace = TRUE),
                       att3 = sample(2:3, 5, replace = TRUE))
  expect_error(check_qmatrix(test_q, "check1", identifier = NULL,
                             item_levels = c(1:5)),
               regexp = "only 0 or 1")

  test_q <- data.frame(item = paste0("I", 1:5),
                       att1 = sample(0:1, 5, replace = TRUE),
                       att2 = sample(0:1, 5, replace = TRUE))
  err <- rlang::catch_cnd(check_qmatrix(test_q, identifier = "item",
                                        item_levels = paste0("I", 1:6)))
  expect_s3_class(err, "rlang_error")
  expect_match(err$message, "have the same number of rows as columns of items")

  test_q <- data.frame(item = paste0("I", 1:5),
                       att1 = sample(0:1, 5, replace = TRUE),
                       att2 = sample(0:1, 5, replace = TRUE))
  err <- rlang::catch_cnd(check_qmatrix(test_q, identifier = "item",
                                        item_levels = paste0("I", c(1:4, 6))))
  expect_s3_class(err, "rlang_error")
  expect_match(err$message, "Missing items: I6")

  test_q <- data.frame(item = paste0("I", 1:5),
                       att1 = sample(0:1, 5, replace = TRUE),
                       att2 = sample(0:1, 5, replace = TRUE))
  err <- rlang::catch_cnd(check_qmatrix(test_q, identifier = "item",
                                        item_levels = paste0("I", c(1:4, 4))))
  expect_s3_class(err, "rlang_error")
  expect_match(err$message, "Extra items: I5")

  test1_q <- tibble::tibble(item = paste0("I_", 1:5),
                            att1 = c(0, 1, 1, 0, 1),
                            att2 = c(1, 0, 0, 1, 0),
                            att3 = c(1, 1, 1, 0, 0))
  test2_q <- tibble::tibble(item = paste0("I_", 1:5),
                            att1 = c(0L, 1L, 1L, 0L, 1L),
                            att2 = c(1L, 0L, 0L, 1L, 0L),
                            att3 = c(1L, 1L, 1L, 0L, 0L))
  test3_q <- tibble::tibble(att1 = c(0L, 1L, 1L, 0L, 1L),
                            att2 = c(1L, 0L, 0L, 1L, 0L),
                            att3 = c(1L, 1L, 1L, 0L, 0L))
  test4_q <- data.frame(item = paste0("I_", 1:5),
                        att1 = c(0, 1, 1, 0, 1),
                        att2 = c(1, 0, 0, 1, 0),
                        att3 = c(1, 1, 1, 0, 0))
  test5_q <- data.frame(item = paste0("I_", 1:5),
                        att1 = c(0L, 1L, 1L, 0L, 1L),
                        att2 = c(1L, 0L, 0L, 1L, 0L),
                        att3 = c(1L, 1L, 1L, 0L, 0L))
  test6_q <- data.frame(att1 = c(0L, 1L, 1L, 0L, 1L),
                        att2 = c(1L, 0L, 0L, 1L, 0L),
                        att3 = c(1L, 1L, 1L, 0L, 0L))
  check_q <- tibble::tibble(item_id = factor(paste0("I_", 1:5)),
                            att1 = c(0L, 1L, 1L, 0L, 1L),
                            att2 = c(1L, 0L, 0L, 1L, 0L),
                            att3 = c(1L, 1L, 1L, 0L, 0L))
  check_q_null <- tibble::tibble(item_id = factor(paste0(1:5)),
                                 att1 = c(0L, 1L, 1L, 0L, 1L),
                                 att2 = c(1L, 0L, 0L, 1L, 0L),
                                 att3 = c(1L, 1L, 1L, 0L, 0L))
  expect_identical(check_qmatrix(test1_q, identifier = "item",
                                 item_levels = paste0("I_", 1:5)), check_q)
  expect_identical(check_qmatrix(test2_q, identifier = "item",
                                 item_levels = paste0("I_", 1:5)), check_q)
  expect_identical(check_qmatrix(test3_q, identifier = NULL,
                                 item_levels = paste0("I_", 1:5)), check_q)
  expect_identical(check_qmatrix(test4_q, identifier = "item",
                                 item_levels = paste0("I_", 1:5)), check_q)
  expect_identical(check_qmatrix(test5_q, identifier = "item",
                                 item_levels = paste0("I_", 1:5)), check_q)
  expect_identical(check_qmatrix(test6_q, identifier = NULL,
                                 item_levels = paste0("I_", 1:5)), check_q)
  expect_identical(check_qmatrix(check_q, identifier = "item_id",
                                 item_levels = paste0("I_", 1:5)), check_q)
  expect_identical(check_qmatrix(test1_q, identifier = "item",
                                 item_levels = NULL), check_q)
  expect_identical(check_qmatrix(test4_q, identifier = "item",
                                 item_levels = NULL), check_q)
  expect_identical(check_qmatrix(test3_q, identifier = NULL,
                                 item_levels = NULL), check_q_null)
  expect_identical(check_qmatrix(test6_q, identifier = NULL,
                                 item_levels = NULL), check_q_null)
})
