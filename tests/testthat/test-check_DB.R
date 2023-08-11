test_that("No error with default arguments", {
  sample_ERR2044662 <- "ERR2044662"
  expect_no_error(check_DB(nice_tidy, sample_id = sample_ERR2044662))
})

test_that("No error if pre-processing of data is added before function", {
  sample_ERR2044662 <- "ERR2044662"
  nice_no_absents <- nice_tidy %>% filter(Abundance > 0)
  expect_no_error(check_DB(nice_no_absents, sample_id = sample_ERR2044662))
})

test_that("Test if it is ok to change range of values", {
  sample_ERR2044662 <- "ERR2044662"
  expect_no_error(check_DB(nice_tidy, sample_id = sample_ERR2044662, range = 20:30))
})

test_that("Expect error for k = 1", {
  sample_ERR2044662 <- "ERR2044662"
  expect_error(check_DB(nice_tidy, sample_id = sample_ERR2044662, range = 1:10))
})

test_that("Should throw error if max k is reached", {
  sample_ERR2044662 <-"ERR2044662"
  max_k_of_ERR2044662 <- length(
    unique(
      pull(
        filter(nice_tidy, Sample == sample_ERR2044662),
        Abundance)))
  #
  expect_error(check_DB(nice_tidy, sample_id = sample_ERR2044662, range = 2:(max_k_of_ERR2044662+1)))
})

test_that("no error for plot option", {
  sample_ERR2044662 <- "ERR2044662"
  expect_no_error(check_DB(nice_tidy, sample_id = sample_ERR2044662, with_plot = TRUE))
})

test_that("Throws an error if input is a vector", {
  sample_ERR2044662 <- nice_tidy %>% filter(Sample == "ERR2044662") %>% pull(Abundance)

  expect_error(check_DB(sample_ERR2044662, sample_id = sample_ERR2044662))
})

test_that("Throws an error is Abundance column is not numeric, example with character", {
  nice_tidy_wrong <- nice_tidy %>% mutate(Abundance = as.character(Abundance))
  expect_error(check_DB(nice_tidy_wrong, sample_id = "ERR2044662"))
})

test_that("Throws an error is Abundance column is not numeric, example with factor", {
  nice_tidy_wrong <- nice_tidy %>% mutate(Abundance = as.factor(Abundance))
  expect_error(check_DB(nice_tidy_wrong, sample_id = "ERR2044662"))
})

