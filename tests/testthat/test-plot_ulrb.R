test_that("Works with a single sample",{
  urlb_results <- define_rb(nice_tidy)
  expect_no_error(plot_ulrb(urlb_results,
                           sample_id = "ERR2044669",
                           taxa_col = "OTU",
                           plot_all = FALSE))
})

test_that("Works for all samples",{
  urlb_results <- define_rb(nice_tidy)
  expect_no_error(
    suppressWarnings( ## there is an expected warning
      plot_ulrb(urlb_results,
                taxa_col = "OTU",
                plot_all = TRUE)))
})

test_that("Throws error if wrong taxa_col is provided",{
  urlb_results <- define_rb(nice_tidy)
  expect_error(
    suppressWarnings( ## there is an expected warning
      plot_ulrb(urlb_results,
                taxa_col = "ASV",
                plot_all = TRUE)))
})

test_that("Log scales works for all samples",{
  urlb_results <- define_rb(nice_tidy)
  expect_no_error(
    suppressWarnings( ## there is an expected warning
      plot_ulrb(urlb_results,
                taxa_col = "OTU",
                plot_all = TRUE,
                log_scaled = TRUE)))
})

test_that("Stops if taxa_col is missing",{
  urlb_results <- define_rb(nice_tidy)
  expect_error(
      plot_ulrb(urlb_results,
                plot_all = TRUE,
                log_scaled = TRUE))
})

test_that("Stops if dataset is a matrix",{
  urlb_results <- define_rb(nice_tidy) %>% as.matrix()
  expect_error(
    plot_ulrb(urlb_results,
              taxa_col = "OTU",
              plot_all = TRUE,
              log_scaled = TRUE))
})

test_that("Stops if log scales argument is not logical",{
  urlb_results <- define_rb(nice_tidy) %>% as.matrix()
  expect_error(
    plot_ulrb(urlb_results,
              taxa_col = "OTU",
              plot_all = TRUE,
              log_scaled = 1))
})

test_that("Error if silhouette score column is wrong",{
  urlb_results <- define_rb(nice_tidy) %>% as.matrix()
  expect_error(
    plot_ulrb(urlb_results,
              taxa_col = "OTU",
              plot_all = TRUE,
              log_scaled = TRUE,
              silhouette_score = "Sil"))
})

test_that("Stops if number of colors is wrong
         in extra arguments does not correspond to the
         size of the classification vector",{
           urlb_results <- define_rb(nice_tidy) %>% as.matrix()
           expect_error(
             plot_ulrb(urlb_results,
                       taxa_col = "OTU",
                       plot_all = TRUE,
                       log_scaled = TRUE,
                       colors = c(1,2)))
})

test_that("Stops if multiple samples are used, but plot_all is not set to TRUE",{
           urlb_results <- define_rb(nice_tidy) %>% as.matrix()
           expect_error(
             plot_ulrb(urlb_results,
                       taxa_col = "OTU",
                       plot_all = FALSE,
                       log_scaled = TRUE,
                       colors = c(1,2)))
         })
