test_that("function works", {
  # get sample names
  sample_names <- c("ERR2044662", "ERR2044663", "ERR2044664",
                    "ERR2044665", "ERR2044666", "ERR2044667",
                    "ERR2044668", "ERR2044669", "ERR2044670")

  expect_no_error(prepare_tidy_data(nice, sample_names = sample_names))
})

test_that("if sample_names is missing the function doesn't work", {
  # get sample names
  expect_error(prepare_tidy_data(nice))
})

test_that("if sample_names is empty function doesn't work", {
  # get sample names
  expect_error(prepare_tidy_data(nice, sample_names = c()))
})

## samples in rows part

test_that("for samples in rows, colnames of data must contain sample_names",{
  expect_error(prepare_tidy_data(nice, sample_names = c("not a sample name"))) ## not sure if this is specific enough
})

test_that("for samples in cols, number of rows must have the same size of sample_names",{
  expect_error(prepare_tidy_data(nice, sample_names = c("not a sample name")))
})
