test_that("function works", {

  classified_species <- define_rb(nice_tidy)

  expect_no_error(sanity_check_rb_sample(classified_species, sample = "NB_250", taxa_id = "OTU"))
})

test_that("function fails if missing sample_id argument", {

  classified_species <- define_rb(nice_tidy)

  expect_error(sanity_check_rb_sample(classified_species, taxa_id = "OTU"))
})

test_that("function fails if missing taxa_id argument", {

  classified_species <- define_rb(nice_tidy)

  expect_error(sanity_check_rb_sample(classified_species, sample_id = "ERR2044662"))
})

test_that("function fails if data is in matrix format", {

  classified_species <- define_rb(nice_tidy)

  classified_species_matrix <- as.matrix(classified_species)

  expect_error(sanity_check_rb_sample(classified_species_matrix,
                                      sample_id = "ERR2044662",
                                      taxa_id = "OTU"))
})

test_that("any color vector works if it has the same length as the number of classifications", {

  classified_species <- define_rb(nice_tidy)

  classifications <- unique(classified_species$Classification)

  expect_no_error(sanity_check_rb_sample(classified_species,
                                         colors = seq_along(classifications),
                                         taxa_id = "OTU",
                                         sample_id = "ERR2044662"))
})
test_that("color vector must be the same size as number of classifications", {

  classified_species <- define_rb(nice_tidy)

  classifications <- unique(classified_species$Classification)

  expect_error(sanity_check_rb_sample(classified_species,
                                         colors = c(seq_along(classifications),2), # extra color
                                         taxa_id = "OTU",
                                         sample_id = "ERR2044662"))
})


