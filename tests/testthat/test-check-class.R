test_that("check S7", {
  dog <- S7::new_class(
    "dog",
    properties = list(
      name = S7::class_character,
      breed = S7::class_character
    )
  )
  larry <- dog(
    name = "Larry",
    breed = c("chihuahua", "pekingese", "japanese chin", "poodle")
  )

  err <- rlang::catch_cnd(check_S7("larry", class = "dog"))
  expect_s3_class(err, "rlang_error")
  expect_match(err$message, "S7 object with class .*dog.*")

  err <- rlang::catch_cnd(check_S7(13, class = "taylor"))
  expect_s3_class(err, "rlang_error")
  expect_match(err$message, "S7 object with class .*taylor.*")

  expect_no_error(check_S7(larry, "dog"))
  expect_null(check_S7(larry, "dog"))
})
