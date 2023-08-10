#' Evaluate k from all samples in a dataset
#'
#' @param data A tibble with, at least, a column for Abundance and Sample. Additional columns are allowed.
#' @param samples_col String with name of column with sample names.
#' @param abundance_col String with name of column with abundance values.
#' @param range The range of values of k to test, default is from 3 to 10.
#' @inheritParams check_DB
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
                       with_plot = FALSE,
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
    summarise(topK = length(unique(.data$Abundance))) %>%
    ungroup() %>%
    pull(.data$topK) %>%
    min()

  # Check if range is below the maximum k
  stopifnot(length(range) <= max_k)

  all_samples_ids <- unique(pull(data, .data$Sample))

  # Apply evaluate_sample_k to all samples
  scores <-
    data %>%
    filter(.data$Abundance > 0, !is.na(.data$Abundance)) %>%
    mutate(SamplePlaceholder = .data$Sample) %>%
    group_by(.data$Sample, .add = TRUE) %>%
    tidyr::nest() %>%
    mutate(Metrics = purrr::map(.x = data,
                                .f = ~evaluate_sample_k(data = .x,
                                                        sample_id = unique(.x$SamplePlaceholder),
                                                        samples_col = "SamplePlaceholder",
                                                        range = range, # default range = 3:10
                                                        inside_nest = FALSE) ## working on removing this
                                 )
           ) %>%
    tidyr::unnest(cols = .data$Metrics)

  if(isTRUE(with_plot)){
    scores %>%
      tidyr::pivot_longer(cols = c("DB", "CH", "average_Silhouette"),
                          names_to = "Index",
                          values_to = "Score") %>%
      mutate(Index = case_when(Index == "DB" ~ "Davies-Bouldin",
                               Index == "CH" ~ "Calinsky-Harabasz",
                               TRUE ~ "Average Silhouette score")) %>%
      ggplot2::ggplot(ggplot2::aes(x = k, y = Score))+
      ggplot2::stat_summary() +
      ggplot2::facet_wrap(~Index, scales = "free_y") +
      ggplot2::theme_bw() +
      ggplot2::labs(y = "Mean (\U00B1 sd) score",
                    title = paste("n =", length(unique(data$Sample))))
  } else {
    return(scores)
  }
}
