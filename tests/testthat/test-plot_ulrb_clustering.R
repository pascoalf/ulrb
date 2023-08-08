test_that("function works", {

  classified_species <- define_rb(nice_tidy)

  expect_no_error(plot_ulrb_clustering(classified_species, sample = "NB_250", taxa_col = "OTU"))
})

test_that("function fails if missing sample_id argument", {

  classified_species <- define_rb(nice_tidy)

  expect_error(plot_ulrb_clustering(classified_species, taxa_col = "OTU"))
})

test_that("function fails if missing taxa_col argument", {

  classified_species <- define_rb(nice_tidy)

  expect_error(plot_ulrb_clustering(classified_species, sample_id = "ERR2044662"))
})

test_that("function fails if data is in matrix format", {

  classified_species <- define_rb(nice_tidy)

  classified_species_matrix <- as.matrix(classified_species)

  expect_error(plot_ulrb_clustering(classified_species_matrix,
                                      sample_id = "ERR2044662",
                                      taxa_col = "OTU"))
})

test_that("function stops if log_scaled isn't logical (TRUE/FALSE)", {

  classified_species <- define_rb(nice_tidy)

  classified_species_matrix <- as.matrix(classified_species)

  expect_error(plot_ulrb_clustering(classified_species_matrix,
                                      sample_id = "ERR2044662",
                                      taxa_col = "OTU",
                                      log_scaled = "yes"))
})

test_that("function works normally if log_scaled is logical (TRUE/FALSE)", {

  classified_species <- define_rb(nice_tidy)

  classified_species_matrix <- as.matrix(classified_species)

  expect_error(plot_ulrb_clustering(classified_species_matrix,
                                      sample_id = "ERR2044662",
                                      taxa_col = "OTU",
                                      log_scaled = TRUE))
})

test_that("any color vector works if it has the same length as the number of classifications", {

  classified_species <- define_rb(nice_tidy)

  classifications <- unique(classified_species$Classification)

  expect_no_error(plot_ulrb_clustering(classified_species,
                                         colors = seq_along(classifications),
                                         taxa_col = "OTU",
                                         sample_id = "ERR2044662"))
})
test_that("color vector must be the same size as number of classifications", {

  classified_species <- define_rb(nice_tidy)

  classifications <- unique(classified_species$Classification)

  expect_error(plot_ulrb_clustering(classified_species,
                                      colors = c(seq_along(classifications),2), # extra color
                                      taxa_col = "OTU",
                                      sample_id = "ERR2044662"))
})

test_that("function output can be modified with other ggplot functions", {

  classified_species <- define_rb(nice_tidy)

  expect_no_error(plot_ulrb_clustering(classified_species,
                                         taxa_col = "OTU",
                                         sample_id = "ERR2044662") + ggplot2::theme_void())
})

test_that("optional labs can overwrite default labs", {

  classified_species <- define_rb(nice_tidy)

  expect_no_error(plot_ulrb_clustering(classified_species,
                                         taxa_col = "OTU",
                                         sample_id = "ERR2044662") +
                    ggplot2::labs(Title = "Something else",
                                  x = "Something else",
                                  y = "Something else"))
})

test_that("the abundance argument can be changed into any type of score, for example, relative abundance", {

  classified_species <- define_rb(nice_tidy)

  classified_species <- classified_species %>%
    group_by(Sample) %>%
    mutate(Abundance = Abundance*100/sum(Abundance))

  expect_no_error(plot_ulrb_clustering(classified_species,
                                         taxa_col = "OTU",
                                         sample_id = "ERR2044662") +
                    ggplot2::labs(y = "Relative abundance"))
})

test_that("log_scaled argument works", {
  classified_species <- define_rb(nice_tidy)

  expect_no_error(plot_ulrb_clustering(classified_species,
                                         taxa_col = "OTU",
                                         sample_id = "ERR2044662",
                                         log_scaled = TRUE))
})

