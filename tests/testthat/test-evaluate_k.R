test_that("No error", {
  expect_no_error(evaluate_k(nice_tidy))
})

test_that("No error after removing absent species", {
  nice_no_zeros <- filter(nice_tidy, Abundance > 0)
  expect_no_error(evaluate_k(nice_no_zeros))
})

test_that("Test if it is ok to change range of values", {
  expect_no_error(evaluate_k(nice_tidy, range = 20:25))
})

test_that("Expect error for k = 1", {
  expect_error(evaluate_k(nice_tidy, range = 1:4))
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
  expect_error(evaluate_k(nice_tidy, range = max_k:(max_k+1)))
})

test_that("no error for plot option", {
  expect_no_error(evaluate_k(nice_tidy, with_plot = TRUE))
})

test_that("Throws an error if input is a vector", {
  sample_ERR2044662 <- nice_tidy %>% filter(Sample == "ERR2044662") %>% pull(Abundance)
  expect_error(evaluate_k(sample_ERR2044662, sample_id = sample_ERR2044662))
})

test_that("Throws an error is Abundance column is not numeric, example with character", {
  nice_tidy_wrong <- nice_tidy %>% mutate(Abundance = as.character(Abundance))
  expect_error(evaluate_k(nice_tidy_wrong))
})

test_that("Throws an error is Abundance column is not numeric, example with factor", {
  nice_tidy_wrong <- nice_tidy %>% mutate(Abundance = as.factor(Abundance))
  expect_error(evaluate_k(nice_tidy_wrong))
})


