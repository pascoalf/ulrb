#' Evaluate k from all samples in a dataset
#'
#' This function extends [evaluate_sample_k()] for any number of samples in a dataset.
#'
#' The plot option (with_plot = TRUE) provides centrality metrics for all samples used.
#'
#' For more details on indices calculation, please see the documentation for [evaluate_sample_k()], [check_DB()],
#' [check_CH()] and [check_avgSil()].
#'
#' @inheritParams evaluate_sample_k
#' @param ... Extra arguments.
#'
#' @return A nested data.frame (or a plot) with three indices for each k and for each sample.
#' @export
#'
#' @seealso [evaluate_sample_k()], [check_DB()], [check_CH()], [check_avgSil()], [suggest_k()]
#'
#' @examples
#' \donttest{
#' library(dplyr)
#'
#' #' evaluate_k(nice_tidy)
#'
#'
#' # To make simple plot
#' evaluate_k(nice_tidy, range = 4:11, with_plot =TRUE)
#' }
#'
evaluate_k <- function(data,
                       range = 2:10,
                       samples_col = "Sample",
                       abundance_col = "Abundance",
                       with_plot = FALSE,
                       ...){
  # range can not begin at 1
  if(min(range) <= 1){
    stop("The range argument must start at 2.")
  }

  # stop if a vector is used as input
  if(is.vector(data)){stop("Input must be a data.frame with at least a column for Samples and another for Abundance.")}

  # stop if abundance values are not numeric (integer or double type)
  if(!is.numeric(pull(data, all_of(abundance_col)))){
    stop("The column with abundance scores must be numeric (integer our double type).")
  }

  # calculate maximum k
#  maxk = data %>%
#    group_by(.data$Sample) %>%
#    summarise(topK = length(unique(.data$Abundance))) %>%
#    ungroup() %>%
#    pull(.data$topK) %>%
#    min()
#  #
#  if(max(range) > maxk){
#    stop(c("Adjust the range of k values. The maximum number of clusters allowed for your samples is", " ", maxk))
#  }
  # Function to calculate maximum k of a sample
  sample_max_k <- function(data){
    data %>%
      filter(.data$Abundance > 0) %>%
      count(.data$Abundance) %>%
      pull(.data$Abundance) %>%
      length()
  }
  # Summary of maximum k possible of each sample
  maxk_summary <- data %>%
    group_by(.data$Sample) %>%
    tidyr::nest() %>%
    mutate(maxk = purrr::map(.x = data, .f = ~sample_max_k(.x))) %>%
    tidyr::unnest(maxk)
  #
  maxk <- maxk_summary %>%
    filter(.data$maxk >= 3) %>%
    pull(.data$maxk) %>%
    min()

  if(sum(maxk_summary[, "maxk"] <= 3) != 0){
    samples_to_remove <- maxk_summary %>%
      filter(maxk <= 3) %>%
      pull(Sample)
    # Remove samples with maxk < 3
    data <- data %>%
      filter(!Sample %in% samples_to_remove)
    # Warn user of samples discarded
    warning(c("Automatic selection of k discarded samples with less than 3 different species:", paste(samples_to_remove, collapse = ",")))
  }

  #
  if(max(range) > maxk){
    stop(c("Adjust the range of k values. The maximum number of clusters allowed
           for your samples is", " ", maxk-1, ". Try range = 2:", maxk-1))
  }

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
                                                        range = range, # default range = 2:10
                                                        inside_nest = FALSE) ## working on removing this
                                 )
           ) %>%
    tidyr::unnest(cols = Metrics)

  if(isTRUE(with_plot)){
    scores %>%
      tidyr::pivot_longer(cols = c("DB", "CH", "average_Silhouette"),
                          names_to = "Index",
                          values_to = "Score") %>%
      mutate(Index = case_when(Index == "DB" ~ "Davies-Bouldin",
                               Index == "CH" ~ "Calinsky-Harabasz",
                               TRUE ~ "Average Silhouette score")) %>%
      ggplot2::ggplot(ggplot2::aes(x = .data$k, y = .data$Score))+
      ggplot2::stat_summary() +
      ggplot2::facet_wrap(~Index, scales = "free_y") +
      ggplot2::theme_bw() +
      ggplot2::labs(y = "Mean (\U00B1 sd) score",
                    title = paste("n =", length(unique(data$Sample))))
  } else {
    return(scores)
  }
}
