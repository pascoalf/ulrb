test_that("function works", {
  # get sample names
  sample_names <- get_samples()

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

## samples in cols part

test_that("function works with samples in rows option",{
  # Make version of nice data with samples in rows, without the other variables
  nice_rows <- nice %>% select(contains("ERR")) %>% t() %>% as.data.frame()
  #
  prepare_tidy_data(nice_rows, sample_names = sample_names, samples_in = "rows")
})




