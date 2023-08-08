test_that("function works export_outpt = `no`", {
  expect_output(sanity_check_all(define_rb(nice_tidy),
                  sample_names = sample_names,
                  taxa_col = "OTU",
                  abundance_id = "Abundance",
                  export_output = "no",
                  classification_col = "Classification"))
})


test_that("function works log_scaled = `yes`", {
  expect_output(sanity_check_all(define_rb(nice_tidy),
                                 sample_names = sample_names,
                                 taxa_col = "OTU",
                                 abundance_id = "Abundance",
                                 export_output = "no",
                                 classification_col = "Classification",
                                 log_scaled = TRUE))
})

#test_that("function works export_outpt = `yes`", {
  #expect_no_error(sanity_check_all(define_rb(nice_tidy),
   #                                sample_names = sample_names,
    #                               taxa_col = "OTU",
     #                              abundance_id = "Abundance",
      #                             export_output = "yes", # don't run this
       #                            classification_col = "Classification"))
#})


