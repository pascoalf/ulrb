test_that("function works export_outpt = `no`", {
  expect_no_error(sanity_check_all(define_rb(nice_tidy),
                  sample_names = sample_names,
                  taxa_id = "OTU",
                  abundance_id = "Abundance",
                  export_output = "no",
                  classification_id = "Classification"))
})

#test_that("function works export_outpt = `yes`", {
  #expect_no_error(sanity_check_all(define_rb(nice_tidy),
   #                                sample_names = sample_names,
    #                               taxa_id = "OTU",
     #                              abundance_id = "Abundance",
      #                             export_output = "yes", # don't run this
       #                            classification_id = "Classification"))
#})


