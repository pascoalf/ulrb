test_that("Works for a sample with default arguments", {

  classified_species <- define_rb(nice_tidy)

  expect_no_error(plot_ulrb_silhouette(classified_species,
                                       sample_id = "ERR2044669",
                                       taxa_col = "OTU"))
})

test_that("function does not work if you provide a non existent sample ID", {

  classified_species <- define_rb(nice_tidy)

  expect_error(plot_ulrb_silhouette(classified_species, sample = "NB_250", taxa_col = "OTU"))
})

test_that("function fails if missing sample_id argument", {

  classified_species <- define_rb(nice_tidy)

  expect_error(plot_ulrb_silhouette(classified_species, taxa_col = "OTU"))
})

test_that("function fails if missing taxa_col argument", {

  classified_species <- define_rb(nice_tidy)

  expect_error(plot_ulrb_silhouette(classified_species, sample_id = "ERR2044662"))
})

test_that("function fails if data is in matrix format", {

  classified_species <- define_rb(nice_tidy)

  classified_species_matrix <- as.matrix(classified_species)

  expect_error(plot_ulrb_silhouette(classified_species_matrix,
                                    sample_id = "ERR2044662",
                                    taxa_col = "OTU"))
})

test_that("function stops if log_scaled isn't logical (TRUE/FALSE)", {

  classified_species <- define_rb(nice_tidy)

  classified_species_matrix <- as.matrix(classified_species)

  expect_error(plot_ulrb_silhouette(classified_species_matrix,
                                    sample_id = "ERR2044662",
                                    taxa_col = "OTU",
                                    log_scaled = "yes"))
})

test_that("function works normally if log_scaled is logical (TRUE/FALSE)", {

  classified_species <- define_rb(nice_tidy)

  classified_species_matrix <- as.matrix(classified_species)

  expect_error(plot_ulrb_silhouette(classified_species_matrix,
                                    sample_id = "ERR2044662",
                                    taxa_col = "OTU",
                                    log_scaled = TRUE))
})

test_that("Any color vector works if it has the same length as the number of classifications", {

  classified_species <- define_rb(nice_tidy)

  classifications <- unique(classified_species$Classification)

  expect_no_error(plot_ulrb_silhouette(classified_species,
                                       colors = seq_along(classifications),
                                       taxa_col = "OTU",
                                       sample_id = "ERR2044662"))
})
test_that("color vector must be the same size as number of classifications", {

  classified_species <- define_rb(nice_tidy)

  classifications <- unique(classified_species$Classification)

  expect_error(plot_ulrb_silhouette(classified_species,
                                    colors = c(seq_along(classifications),2), # extra color
                                    taxa_col = "OTU",
                                    sample_id = "ERR2044662"))
})

test_that("function output can be modified with other ggplot functions", {

  classified_species <- define_rb(nice_tidy)

  expect_no_error(plot_ulrb_silhouette(classified_species,
                                       taxa_col = "OTU",
                                       sample_id = "ERR2044662") + ggplot2::theme_void())
})

test_that("optional labs can overwrite default labs", {

  classified_species <- define_rb(nice_tidy)

  expect_no_error(plot_ulrb_silhouette(classified_species,
                                       taxa_col = "OTU",
                                       sample_id = "ERR2044662") +
                    ggplot2::labs(Title = "Something else",
                                  x = "Something else",
                                  y = "Something else"))
})

test_that("The abundance argument can be changed into any type of score,
          for example, relative abundance", {

            classified_species <- define_rb(nice_tidy)

            classified_species <- classified_species %>%
              group_by(Sample) %>%
              mutate(Abundance = Abundance*100/sum(Abundance))

            expect_no_error(plot_ulrb_silhouette(classified_species,
                                                 taxa_col = "OTU",
                                                 sample_id = "ERR2044662") +
                              ggplot2::labs(y = "Relative abundance"))
          })

test_that("Works with a single sample",{
  urlb_results <- define_rb(nice_tidy)
  expect_no_error(plot_ulrb_silhouette(urlb_results,
                                       sample_id = "ERR2044669",
                                       taxa_col = "OTU",
                                       plot_all = FALSE))
})

test_that("Works for all samples",{
  urlb_results <- define_rb(nice_tidy)
  expect_no_error(
    suppressWarnings( ## there is an expected warning
      plot_ulrb_silhouette(urlb_results,
                           taxa_col = "OTU",
                           plot_all = TRUE)))
})

test_that("Throws error if wrong taxa_col is provided",{
  urlb_results <- define_rb(nice_tidy)
  expect_error(
    suppressWarnings( ## there is an expected warning
      plot_ulrb_silhouette(urlb_results,
                           taxa_col = "ASV",
                           plot_all = TRUE)))
})

test_that("Log scales works for all samples",{
  urlb_results <- define_rb(nice_tidy)
  expect_no_error(
    suppressWarnings( ## there is an expected warning
      plot_ulrb_silhouette(urlb_results,
                           taxa_col = "OTU",
                           plot_all = TRUE,
                           log_scaled = TRUE)))
})

test_that("Stops if log scales argument is not logical, with option for all samples",{
  urlb_results <- define_rb(nice_tidy) %>% as.matrix()
  expect_error(
    plot_ulrb_silhouette(urlb_results,
                         taxa_col = "OTU",
                         plot_all = TRUE,
                         log_scaled = 1))
})

test_that("Stops if number of colors is wrong
         in extra arguments does not correspond to the
         size of the classification vector",{
           urlb_results <- define_rb(nice_tidy) %>% as.matrix()
           expect_error(
             plot_ulrb_silhouette(urlb_results,
                                  taxa_col = "OTU",
                                  plot_all = TRUE,
                                  log_scaled = TRUE,
                                  colors = c(1,2)))
         })

test_that("Error if silhouette score column is wrong",{
  urlb_results <- define_rb(nice_tidy) %>% as.matrix()
  expect_error(
    plot_ulrb_silhouette(urlb_results,
              taxa_col = "OTU",
              plot_all = TRUE,
              log_scaled = TRUE,
              silhouette_score = "Sil"))
})

test_that("Stops if multiple samples are used, but plot_all is not set to TRUE",{
  urlb_results <- define_rb(nice_tidy) %>% as.matrix()
  expect_error(
    plot_ulrb_silhouette(urlb_results,
              taxa_col = "OTU",
              plot_all = FALSE,
              log_scaled = TRUE,
              colors = c(1,2)))
})
