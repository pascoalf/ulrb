### modify all tests, input should be a data.frame ###


test_that("No error", {
  sample_ERR2044662 <- filter(nice_tidy, Sample == "ERR2044662")
  expect_no_error(evaluate_k(sample_ERR2044662))
})
test_that("No error after removing absent species", {
  sample_ERR2044662 <- filter(nice_tidy, Sample == "ERR2044662", Abundance > 0)
  expect_no_error(evaluate_k(sample_ERR2044662))
})

test_that("Test if it is ok to cange range of values", {
  sample_ERR2044662 <- filter(nice_tidy, Sample == "ERR2044662", Abundance > 0)
  expect_no_error(evaluate_k(sample_ERR2044662, range = 20:30))
})

test_that("Expect error for k = 1", {
  sample_ERR2044662 <- filter(nice_tidy, Sample == "ERR2044662")
  expect_error(evaluate_k(sample_ERR2044662, range = 1:10))
})

test_that("Should work for all possible k's without errors", {
  sample_ERR2044662 <- filter(nice_tidy, Sample == "ERR2044662", Abundance > 0)
  max_k_of_ERR2044662 <- length(unique(sample_ERR2044662$Abundance))
  expect_no_error(evaluate_k(sample_ERR2044662, range = 2:(max_k_of_ERR2044662+1)))
})

test_that("Should throw error if max k is reached", {
  sample_ERR2044662 <- filter(nice_tidy, Sample == "ERR2044662", Abundance >0)
  max_k_of_ERR2044662 <- length(unique(sample_ERR2044662$Abundance))
  expect_error(evaluate_k(sample_ERR2044662, range = 2:(max_k_of_ERR2044662+2)))
})


