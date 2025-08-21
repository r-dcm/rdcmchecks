test_that("abort_bad_argument() is informative.", {
  expect_error(
    abort_bad_argument("size", "be an integer"),
    regexp = "be an integer",
    class = "rlang_error"
  )
  expect_snapshot(error = TRUE, {
    abort_bad_argument("size", "be an integer", not = "character")
    abort_bad_argument(
      "size",
      must = "be an integer",
      not = "character",
      footer = c(i = "please")
    )
    abort_bad_argument(
      "size",
      must = "be an integer",
      not = "character",
      footer = "required",
      custom = "A new error"
    )
  })
})

test_that("abort_bad_argument() can add inline markup to footer", {
  expect_snapshot(error = TRUE, {
    a_local_variable <- "local var"
    footer <- cli::format_message(c("x" = "Look at {.val {a_local_variable}}"))
    abort_bad_argument("size", "be an integer", footer = footer)
  })
})
