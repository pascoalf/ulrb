test_that("function works", {
  # get sample names
  sample_names <- c("ERR2044662", "ERR2044663", "ERR2044664",
                    "ERR2044665", "ERR2044666", "ERR2044667",
                    "ERR2044668", "ERR2044669", "ERR2044670")

  expect_no_error(prepare_tidy_data(nice, sample_names = sample_names))
})
