test_that("function works", {
  # get sample names
  sample_names <- colnames(nice_clean[,1:9])

  expect_no_error(prepare_tidy_data(nice_clean, sample_names = sample_names))
})
