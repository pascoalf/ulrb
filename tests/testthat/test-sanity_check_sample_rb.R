test_that("function works", {

  classified_species <- define_rb(nice_tidy)

  expect_no_error(sanity_check_rb_sample(classified_species, sample = "NB_250", taxa_id = "OTU"))
})

test_that("function fails missing sample argument", {
  expect_error(sanity_check_rb_sample(define_rb(nice_tidy), taxa_id = "OTU"))
})
