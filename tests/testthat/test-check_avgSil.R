test_that("No error", {
  sample_ERR2044662 <- filter(nice_tidy, Sample == "ERR2044662")[, "Abundance"]
  expect_no_error(check_avgSil(sample_ERR2044662$Abundance))
})

test_that("No error after removing absent species", {
  sample_ERR2044662 <- filter(nice_tidy, Sample == "ERR2044662", Abundance > 0)[, "Abundance"]
  expect_no_error(check_avgSil(sample_ERR2044662$Abundance))
})

test_that("Test if it is ok to cange range of values", {
  sample_ERR2044662 <- filter(nice_tidy, Sample == "ERR2044662", Abundance > 0)[, "Abundance"]
  expect_no_error(check_avgSil(sample_ERR2044662$Abundance, range = 20:30))
})

test_that("Expect error for k = 1", {
  sample_ERR2044662 <- filter(nice_tidy, Sample == "ERR2044662")[, "Abundance"]
  expect_error(check_avgSil(sample_ERR2044662$Abundance, range = 1:10))
})

test_that("Should work for all possible k's without errors", {
  sample_ERR2044662 <- filter(nice_tidy, Sample == "ERR2044662")[, "Abundance"]
  max_k_of_ERR2044662 <- length(unique(sample_ERR2044662$Abundance))
  expect_no_error(check_avgSil(sample_ERR2044662$Abundance, range = 2:(max_k_of_ERR2044662-1)))
})

test_that("Should throw error if max k is reached", {
  sample_ERR2044662 <- filter(nice_tidy, Sample == "ERR2044662")[, "Abundance"]
  max_k_of_ERR2044662 <- length(unique(sample_ERR2044662$Abundance))
  expect_error(check_avgSil(sample_ERR2044662$Abundance, range = 2:(max_k_of_ERR2044662+1)))
})

test_that("Input vector as one dimension", {
  sample_ERR2044662 <- filter(nice_tidy, Sample == "ERR2044662")[, "Abundance"]
  max_k_of_ERR2044662 <- length(unique(sample_ERR2044662$Abundance))
  ## a data.frame, instead of a vector, should throw an error
  expect_error(check_avgSil(sample_ERR2044662, range = 2:(max_k_of_ERR2044662+1)))
})

