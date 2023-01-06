#
sample_names <- c("ERR2044662", "ERR2044663", "ERR2044664",
                  "ERR2044665", "ERR2044666", "ERR2044667",
                  "ERR2044668", "ERR2044669", "ERR2044670")
#
nice_tidy <- prepare_tidy_data(nice, sample_names = selected_samples, samples_in = "cols")
#
usethis::use_data(nice_tidy, overwrite = TRUE)
