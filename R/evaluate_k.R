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
                       range = 3:10,
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
