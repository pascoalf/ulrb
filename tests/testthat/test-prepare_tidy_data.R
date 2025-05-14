test_that("function works", {
  # get sample names
 # sample_names <- get_samples()

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

## samples in cols part

test_that("for samples in rows, colnames of data must contain sample_names",{
  expect_error(prepare_tidy_data(nice, sample_names = c("not a sample name"))) ## not sure if this is specific enough
})

## samples in rows part

test_that("function works with samples in rows option",{
  #
  expect_no_error(prepare_tidy_data(nice_rows, sample_names = sample_names, samples_in = "rows"))
})

test_that("for samples in rows, function stops if sample_names is not of the same lenght as the nubmer of rows",{
#  sample_names <- get_samples()
  #
  expect_error(prepare_tidy_data(nice_rows, sample_names = sample_names[-1], samples_in = "rows"))
  expect_error(prepare_tidy_data(nice_rows, sample_names = c(sample_names,sample_names), samples_in = "rows"))
})

test_that("for samples in rows, function gives warning if rownames of input data do not correspond to sample_names vector provided", {
 # sample_names <- get_samples()
  expect_message(prepare_tidy_data(nice_rows, sample_names = seq_along(sample_names), samples_in = "rows"))
})

test_that("if samples are in rows, a message warns that colnames were assumed to be taxonomic units", {
  # sample_names <- get_samples()
  expect_message(prepare_tidy_data(nice_rows, sample_names = sample_names, samples_in = "rows"))
})

