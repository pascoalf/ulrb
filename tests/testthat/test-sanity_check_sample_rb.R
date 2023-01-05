test_that("function works", {

  classified_species <- define_rb(nice_tidy)

  expect_no_error(sanity_check_rb_sample(classified_species, sample = "NB_250", taxa_id = "OTU"))
})
#test_that("function fails without sample specification", {
  classified_species <- define_rb(nice_tidy)

  expect_error(sanity_check_rb_sample(classified_species, taxa_id = "OTU"))
})
