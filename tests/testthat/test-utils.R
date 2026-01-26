test_that("missing type works", {
  expect_true(missing_type(rnorm(10), missing = 9))
  expect_true(missing_type(rnorm(10), missing = 9L))
  expect_true(missing_type(rnorm(10), missing = NA_real_))
  expect_true(missing_type(rnorm(10), missing = NA_integer_))

  expect_true(missing_type(letters, missing = "-99"))
  expect_true(missing_type(letters, missing = NA_character_))

  expect_false(missing_type(letters, missing = 9))
  expect_false(missing_type(state.name, missing = NA))
  expect_false(missing_type(rnorm(10), missing = "."))
})
