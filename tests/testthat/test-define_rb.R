test_that("Function runs without error", {
  expect_no_error(define_rb(nice_tidy))
})
test_that("Check if result is reproducible",{
  expect_equal(define_rb(nice_tidy),
               define_rb(nice_tidy))
})
test_that("Levels output is integer", {
  expect_type(define_rb(nice_tidy)$Level, type = "integer")
})
test_that("Number of levels corresponds to
          size of classification vector", {
            test_vector <- c("Rare", "Undetermined","Abundant")
            expect_length(
                unique(
                  define_rb(nice_tidy,
                            classification_vector = test_vector)$Level),
              length(
                unique(test_vector)))
            })
test_that("Number of levels corresponds to
          size of alternative classification vector", {
            test_vector <- c("Rare","Abundant")
            expect_length(
                unique(
                  define_rb(nice_tidy,
                            classification_vector = test_vector)$Level),
              length(
                unique(test_vector)))
          })
test_that("Works with a classification vector of a single classification,
          or a single cluster", {
            test_vector <- c("Rare")
            expect_no_error(
                  define_rb(nice_tidy,
                            classification_vector = test_vector)$Level)
          })
#
test_that("The median abundance of clusters is type double",{
  expect_type(define_rb(nice_tidy)$Cluster_median_abundance,
              type = "double")
})
#
test_that("The classification column is type integer",{
  expect_type(define_rb(nice_tidy)$Classification,
              type = "integer")
})
test_that("The number of classifications corresponds to classification vector",{
  classification_vector <- c("Rare", "Undertermined", "Abundant")
  expect_length(
      unique(
        define_rb(nice_tidy, classification_vector = classification_vector)$Classification),
    length(
      unique(classification_vector)))
})
test_that("It works with alternative classification vectors",{
  classification_vector <- c("Rare", "Abundant")
  expect_no_error(define_rb(nice_tidy,
                            classification_vector = classification_vector))
})
test_that("It works with single classification",{
  classification_vector <- c("Rare")
  expect_no_error(define_rb(nice_tidy,
                            classification_vector = classification_vector))
})
test_that("It needs at least one classification to work",{
  classification_vector <- c()
  expect_error(define_rb(nice_tidy,
                            classification_vector = classification_vector))
})
test_that("Classification vector can be numbers instead of strings",{
  classification_vector <- c(1:10)
  expect_no_error(define_rb(nice_tidy,
                         classification_vector = classification_vector))
})
test_that("Largest possible vector is equal to the number of observations of the sample with least observations",{
            # Remove zeros and NAs, if any, to get only the valid observations
            data_cleaned <- filter(nice_tidy, Abundance > 0, !is.na(Abundance))

            # Calculate maximum number of valid observations per sample
            total_clusters <-
              dplyr::summarise(group_by(data_cleaned,Sample),
                               Observation = length(Abundance>0))

            # Get maximum number of clusters
            maximum_possible_clusters <- min(total_clusters$Observation)-1

            # Make largest classification vector that will work
            largest_classification_vector <- c(1:(maximum_possible_clusters))

            expect_no_error(define_rb(nice_tidy, classification_vector = largest_classification_vector))
})
test_that("The definition does not work for classification vectors with more than the maximum
          number of possible clusters.",{

            # Remove zeros and NAs, if any, to get only the valid observations
            data_cleaned <- filter(nice_tidy, Abundance > 0, !is.na(Abundance))

            # Calculate maximum number of valid observations per sample
            total_clusters <-
              dplyr::summarise(group_by(data_cleaned,Sample),
                               Observation = length(Abundance>0))

            # Get maximum number of clusters
            maximum_possible_clusters <- min(total_clusters$Observation)-1

            # Make classification vector that will not work
            bad_classification_vector <- c(1:(maximum_possible_clusters+1))

            expect_error(define_rb(nice_tidy, classification_vector = bad_classification_vector))
          })
test_that("User can give any col names to the data", {

  # Modify colnames
  data_modified <- nice_tidy
  colnames(data_modified) <- c("a", "b", "c")

  expect_no_error(define_rb(data_modified, samples_id = "b", abundance_id = "c"))
})
test_that("User must specify colnames if they are not default", {

  # Modify colnames
  data_modified <- nice_tidy
  colnames(data_modified) <- c("a", "b", "c")

  expect_error(define_rb(data_modified))
})
test_that("define_rb works for a single sample", {

  # Modify colnames
  one_sample <- filter(nice_tidy, Sample == "NB_5")

  expect_no_error(define_rb(one_sample))
})
test_that("For one sample the maximum number of
          elements in the classification vector is
          the number of observations", {

  # Modify colnames
  one_sample <- dplyr::filter(nice_tidy,
                       Sample == "NB_5",
                       Abundance > 0,
                       !is.na(Sample))

  # Calculate maximum number of valid observations per sample
  total_clusters <-
    dplyr::summarise(group_by(one_sample,Sample),
                     Observation = length(Abundance))

  # Get maximum number of clusters
  maximum_possible_clusters <- min(total_clusters$Observation)-1

  # Make classification vector that will not work
  bad_classification_vector <- c(1:(maximum_possible_clusters+1))

  expect_error(define_rb(one_sample, classification_vector = bad_classification_vector))
})
test_that("Works without any column with species information",{

  # Remove species column
  no_species <- nice_tidy %>% select(-OTU)
  expect_no_error(define_rb(no_species))
})
test_that("Abundance must be numeric",{
  wrong_abundance <- mutate(nice_tidy, Abundance = as.character(Abundance))

  expect_error(define_rb(wrong_abundance))
})
test_that("Output does not have Species with zero abundance",{

  # Standard output
  output <- define_rb(nice_tidy)

  # Pull observations with Abundance == 0
  zero_abundance <-
    output %>%
    filter(Abundance == 0) %>%
    pull(Abundance)

  expect_length(zero_abundance, 0)
})
test_that("Output does not have Species with NA abundance",{
  # Standard output
  output <- define_rb(nice_tidy)

  # Pull observations with Abundance == 0
  NA_abundance <-
    output %>%
    filter(is.na(Abundance)) %>%
    pull(Abundance)

  expect_length(NA_abundance, 0)
})

## note: be more specific after adding more functions
test_that("Input must be tidy",{
  untidy_data <- nice_tidy %>% tidyr::pivot_wider(names_from = Sample, values_from = Abundance)

  expect_error(define_rb(untidy_data))
})
