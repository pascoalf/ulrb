test_that("function works export_outpt = `no`", {
  expect_output(sanity_check_all(define_rb(nice_tidy),
                  sample_names = sample_names,
                  taxa_col = "OTU",
                  abundance_col = "Abundance",
                  export_output = "no",
                  classification_col = "Classification"))
})


test_that("function works log_scaled = TRUE", {
  expect_output(sanity_check_all(define_rb(nice_tidy),
                                 sample_names = sample_names,
                                 taxa_col = "OTU",
                                 abundance_col = "Abundance",
                                 export_output = "no",
                                 classification_col = "Classification",
                                 log_scaled = TRUE))
})



