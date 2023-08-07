test_that("No error", {
  sample_ERR2044662 <- filter(nice_tidy, Sample == "ERR2044662")
  expect_no_error(suggest_k(sample_ERR2044662))
})

test_that("No error after removing absent species", {
  sample_ERR2044662 <- filter(nice_tidy, Sample == "ERR2044662", Abundance > 0)
  expect_no_error(suggest_k(sample_ERR2044662))
})

test_that("Test if it is ok to cange range of values", {
  sample_ERR2044662 <- filter(nice_tidy, Sample == "ERR2044662", Abundance > 0)
  expect_no_error(suggest_k(sample_ERR2044662, range = 20:30))
})

test_that("Expect error for k = 1", {
  sample_ERR2044662 <- filter(nice_tidy, Sample == "ERR2044662")
  expect_error(suggest_k(sample_ERR2044662, range = 1:10))
})

test_that("Should work for all possible k's without errors", {
  sample_ERR2044662 <- filter(nice_tidy, Sample == "ERR2044662", Abundance > 0)
  max_k_of_ERR2044662 <- length(unique(sample_ERR2044662$Abundance))
  expect_no_error(suggest_k(sample_ERR2044662, range = 2:(max_k_of_ERR2044662-1)))
})

test_that("Should throw error if max k is reached", {
  sample_ERR2044662 <- filter(nice_tidy, Sample == "ERR2044662", Abundance >0)
  max_k_of_ERR2044662 <- length(unique(sample_ERR2044662$Abundance))
  expect_error(suggest_k(sample_ERR2044662, range = 2:(max_k_of_ERR2044662+1)))
})

### Repeat tests, but for all samples of dataset

test_that("No error", {
  expect_no_error(suggest_k(nice_tidy))
})

test_that("No error after removing absent species", {
  nice_tidy_filtered <- filter(nice_tidy, Abundance > 0)
  expect_no_error(suggest_k(nice_tidy_filtered))
})

test_that("Test if it is ok to change range of values", {
  nice_tidy_filtered <- filter(nice_tidy, Abundance > 0)
  expect_no_error(suggest_k(nice_tidy_filtered, range = 20:30))
})

test_that("Expect error for k = 1", {
  expect_error(suggest_k(nice_tidy, range = 1:10))
})

test_that("Should work for all possible k's without errors", {
  nice_tidy_filtered <- filter(nice_tidy, Abundance > 0)
  # get all possible k's from a group of samples
  # the smallest maximum k across samples
  # will be the maximum k for the entire dataset
  max_k <-  nice_tidy_filtered %>%
    group_by(.data$Sample) %>%
    summarise(max_k = length(unique(Abundance))) %>%
    pull(max_k) %>%
    min()
  #
  expect_no_error(suggest_k(nice_tidy_filtered, range = 2:(max_k-1)))
})

test_that("Should throw error if max k is reached", {
  nice_tidy_filtered <- filter(nice_tidy, Abundance >0)
  # get all possible k's from a group of samples
  # the smallest maximum k across samples
  # will be the maximum k for the entire dataset
  max_k <-  nice_tidy_filtered %>%
    group_by(.data$Sample) %>%
    summarise(max_k = length(unique(Abundance))) %>%
    pull(max_k) %>%
    min()
  #
  expect_error(suggest_k(nice_tidy_filtered, range = 2:(max_k+1)))
})

