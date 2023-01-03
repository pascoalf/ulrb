test_that("Number of clusters", {
  expect_type(define_rb(nice_tidy)$Level, type = "integer")
})
test_that("Function runs without error", {
  expect_no_error(define_rb(nice_tidy))
})
