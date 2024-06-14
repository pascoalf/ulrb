## tests with a single sample
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


test_that("Should throw error if max k is reached", {
  sample_ERR2044662 <- filter(nice_tidy, Sample == "ERR2044662", Abundance >0)
  max_k_of_ERR2044662 <- length(unique(sample_ERR2044662$Abundance))
  expect_error(suggest_k(sample_ERR2044662, range = max_k_of_ERR2044662:(max_k_of_ERR2044662+1)))
})

# tests for all samples of dataset

test_that("No error for all samples", {
  expect_no_error(suggest_k(nice_tidy))
})

test_that("No error for detailed = TRUE", {
  expect_no_error(suggest_k(nice_tidy, detailed = TRUE))
})

test_that("No error for specific index, Davies-Bouldin", {
  expect_no_error(suggest_k(nice_tidy, detailed = FALSE, index = "Davies-Bouldin"))
})

test_that("No error for specific index, Calinski-Harabasz", {
  expect_no_error(suggest_k(nice_tidy, detailed = FALSE, index = "Calinski-Harabasz"))
})

test_that("No error after removing absent species", {
  nice_tidy_filtered <- filter(nice_tidy, Abundance > 0)
  expect_no_error(suggest_k(nice_tidy_filtered))
})

test_that("Test if it is ok to change range of values", {
  nice_tidy_filtered <- filter(nice_tidy, Abundance > 0)
  expect_no_error(suggest_k(nice_tidy_filtered, range = 20:25))
})

test_that("Expect error for k = 1", {
  expect_error(suggest_k(nice_tidy, range = 1:10))
})


test_that("Should throw error if max k is reached", {
  nice_tidy_filtered <- filter(nice_tidy, Abundance >0)
  # get maximum k for the entire dataset
  max_k <-  nice_tidy_filtered %>%
    group_by(.data$Sample) %>%
    summarise(max_k = length(unique(Abundance))) %>%
    pull(max_k) %>%
    min()
  #
  expect_error(suggest_k(nice_tidy_filtered, range = max_k:(max_k+1)))
})



## test input values

test_that("Throws an error if input is a vector", {
  sample_ERR2044662 <- nice_tidy %>% filter(Sample == "ERR2044662") %>% pull(Abundance)
  expect_error(suggest_k(sample_ERR2044662, sample_id = sample_ERR2044662))
})

test_that("Throws an error is Abundance column is not numeric, example with character", {
  nice_tidy_wrong <- nice_tidy %>% mutate(Abundance = as.character(Abundance))
  expect_error(suggest_k(nice_tidy_wrong))
})

test_that("Throws an error is Abundance column is not numeric, example with factor", {
  nice_tidy_wrong <- nice_tidy %>% mutate(Abundance = as.factor(Abundance))
  expect_error(suggest_k(nice_tidy_wrong))
})


