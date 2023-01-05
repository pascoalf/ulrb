test_that("function works", {
  expect_no_error(sanity_check_rb_sample(define_rb(nice_tidy),sample = "NB_250", taxa_id = "OTU"))
})
