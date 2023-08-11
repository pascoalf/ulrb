#' Check average Silhouette score index
#'
#' Calculates average Silhouette score for a given sample
#'
#' @details
#' Data input
#'
#' This function takes a data.frame with a column for samples and a column for abundance
#' (minimum), but can take any number of other columns. It will then filter the specific sample
#' that you want to analyze. You can also pre-filter for your specific sample, but you still need to
#' provide the sample ID (sample_id) and the table always needs a column for Sample and another for Abundance
#' (indicate how you name them with the arguments samples_col and abundance_col).
#'
#' @details
#' Output options
#'
#' The default option returns a vector with CH scores for each k. This is a simple output that can then be used
#' for other analysis. However, we also provide the option to show a plot (set `with_plot = TRUE`) with
#' the CH score for each k.
#'
#'
#'
#' @inheritParams check_DB
#'
#' @return Vector with average Silhouette score index for each pre-specified k.
#' @export
#'
#' @examples
#' library(dplyr)
#' # Just scores
#' check_avgSil(nice_tidy, sample_id = "ERR2044662")
#'
#' # To change range
#' check_avgSil(nice_tidy, sample_id = "ERR2044662", range = 4:11)
#'
#' # To see a simple plot
#' check_avgSil(nice_tidy, sample_id = "ERR2044662", range = 4:11, with_plot=TRUE)
#'
check_avgSil <- function(data,
                         sample_id,
                         samples_col = "Sample",
                         abundance_col = "Abundance",
                         range = 3:10,
                         with_plot = FALSE, ...){

  # Conditions for function to run
  stopifnot(range > 1)

  # Match samples_col and abundance_col with Samples and Abundance, respectively
  data <-
    data %>%
    rename(Sample = all_of(samples_col),
           Abundance = all_of(abundance_col)) %>%
    filter(.data$Sample == all_of(sample_id)) %>%
    filter(.data$Abundance > 0, !is.na(.data$Abundance))

  # Make vector with abundance scores
  pulled_data <- pull(data, .data$Abundance)

  # Before continuing, verify if max k was reached in range provided
  stopifnot(length(range) <= length(unique(pulled_data)))

  # Calculate Average Silhouette score index
  scores <- sapply(range, function(k){
    mean(
      cluster::silhouette(
        cluster::pam(pulled_data, k)$clustering,
        stats::dist(pulled_data))[,3])
  })

  if(isTRUE(with_plot)){
    scores_data.frame <- data.frame(Score = scores, k = range)
    scores_data.frame %>%
      ggplot2::ggplot(ggplot2::aes(x = .data$k, y = .data$Score)) +
      ggplot2::geom_point() +
      ggplot2::labs(title = "Average Silhouette score") +
      ggplot2::theme_bw()
  } else {
    scores
  }
}
