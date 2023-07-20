#' Evaluate k from all samples in a dataset
#'
#' @param data A tibble with, at least, a column for Abundance and Sample. Additional columns are allowed.
#' @param samples_id String with name of column with sample names.
#' @param abundance_id String with name of column with abundance values.
#' @param range
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#' #test
evaluate_k <- function(data, range = 3:10, ...){

  # Match samples_id and abundance_id with Samples and Abundance, respectively
  data <-
    data %>%
    rename(Sample = all_of(samples_id),
           Abundance = all_of(abundance_id))

  # Apply evaluate_sample_k to all samples
  data %>%
    filter(.data$Abundance > 0, !is.na(.data$Abundance)) %>%
    group_by(.data$Sample, .add = TRUE) %>%
    tidyr::nest() %>%

}
