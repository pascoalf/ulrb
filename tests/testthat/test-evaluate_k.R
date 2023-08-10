### modify all tests, input should be a data.frame ###

test_that("No error", {
  expect_no_error(evaluate_k(nice_tidy))
})

test_that("No error after removing absent species", {
  nice_no_zeros <- filter(nice_tidy, Abundance > 0)
  expect_no_error(evaluate_k(nice_no_zeros))
})

test_that("Test if it is ok to change range of values", {
  expect_no_error(evaluate_k(nice_tidy, range = 20:30))
})

test_that("Expect error for k = 1", {
  expect_error(evaluate_k(nice_tidy, range = 1:10))
})

test_that("Should work for all possible k's without errors", {
  ## max k across all samples
  max_k <- nice_tidy %>%
    filter(Abundance > 0, !is.na(Abundance)) %>%
    group_by(Sample) %>%
    summarise(topK = length(unique(Abundance))) %>%
    ungroup() %>%
    pull(topK) %>%
    min()
  #
  expect_no_error(evaluate_k(nice_tidy, range = 2:(max_k)))
})

test_that("Should throw error if max k is reached", {
  ## max k across all samples
  max_k <- nice_tidy %>%
    filter(Abundance > 0, !is.na(Abundance)) %>%
    group_by(Sample) %>%
    summarise(topK = length(unique(Abundance))) %>%
    ungroup() %>%
    pull(topK) %>%
    min()
  #
  expect_error(evaluate_k(nice_tidy, range = 2:(max_k+1)))
})


