#' Evaluate k from all samples in a dataset
#'
#' @param data A tibble with, at least, a column for Abundance and Sample. Additional columns are allowed.
#' @param samples_col String with name of column with sample names.
#' @param abundance_col String with name of column with abundance values.
#' @param range The range of values of k to test, default is from 3 to 10.
#' @param ... Extra arguments.
#'
#' @return Tidy table with three indices for each k within each sample.
#' @export
#'
#' @examples
#' evaluate_k(nice_tidy)
#'
#'
evaluate_k <- function(data,
                       range = 3:10,
                       samples_col = "Sample",
                       abundance_col = "Abundance",
                       ...){
  stopifnot(range > 1)
  #

  # Match samples_col and abundance_col with Samples and Abundance, respectively
  data <-
    data %>%
    rename(Sample = all_of(samples_col),
           Abundance = all_of(abundance_col))

  # Get max k that can work for all samples provided
  max_k <- data %>%
    group_by(.data$Sample) %>%
    summarise(topK = length(unique(Abundance))) %>%
    ungroup() %>%
    pull(topK) %>%
    min()

  # Check if range is below the maximum k
  stopifnot(length(range) <= length(unique(max_k)))

  # Apply evaluate_sample_k to all samples
  data %>%
    filter(.data$Abundance > 0, !is.na(.data$Abundance)) %>%
    group_by(.data$Sample, .add = TRUE) %>%
    tidyr::nest() %>%
    mutate(Metrics = purrr::map2(.x = data,
                                 .y = Sample,
                                 .f = ~evaluate_sample_k(data = .x$Abundance,
                                                        sample_col = .y$Sample,
                                                        range = range, # default range = 3:10
                                                        inside_nest = TRUE))) %>%
    tidyr::unnest(cols = .data$Metrics)

}
