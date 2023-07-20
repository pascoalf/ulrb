#' Evaluate k from all samples in a dataset
#'
#' @param data A tibble with, at least, a column for Abundance and Sample. Additional columns are allowed.
#' @param samples_id String with name of column with sample names.
#' @param abundance_id String with name of column with abundance values.
#' @param range The range of values of k to test, default is from 3 to 10.
#' @param ... Extra arguments.
#'
#' @return Tidy table with three indices for each k within each sample.
#' @export
#'
#' @examples
#' #test
evaluate_k <- function(data,
                       range = 3:10,
                       samples_id = "Sample",
                       abundance_id = "Abundance",
                       ...){
  stopifnot(range > 1)
  #

  # Match samples_id and abundance_id with Samples and Abundance, respectively
  data <-
    data %>%
    rename(Sample = all_of(samples_id),
           Abundance = all_of(abundance_id))
  # check if max k has been reached
  stopifnot(range < length(unique(data$Abundance)))

  # Apply evaluate_sample_k to all samples
  data %>%
    filter(.data$Abundance > 0, !is.na(.data$Abundance)) %>%
    group_by(.data$Sample, .add = TRUE) %>%
    tidyr::nest() %>%
    mutate(Metrics = purrr::map(.x = data,
                                .f = ~evaluate_sample_k(data = .x$Abundance, range = range))) %>% # default range = 3:10
    tidyr::unnest(cols = .data$Metrics)

}
