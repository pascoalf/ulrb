#' Check Calinsky-Harabasz index
#'
#' @inheritParams check_DB
#'
#' @return Vector or plot with Calinsky-Harabasz index for each pre-specified k.
#' @export
#'
#' @examples
#' library(dplyr)
#' # Just scores
#' check_CH(nice_tidy, sample_id = "ERR2044662")
#'
#' # To change range
#' check_CH(nice_tidy, sample_id = "ERR2044662", range = 4:11)
#'
#' # To see a simple plot
#' check_CH(nice_tidy, sample_id = "ERR2044662", range = 4:11, with_plot=TRUE)
#'
#'
check_CH <- function(data,
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

  # Calculate Calinsky-Harabasz index
  scores <- sapply(range, function(k){
    clusterSim::index.G1(x = pulled_data,
                         cl = cluster::pam(pulled_data, k = k, cluster.only = TRUE)
    )
  })

  if(isTRUE(with_plot)){
    scores_data.frame <- data.frame(Score = scores, k = range)
    scores_data.frame %>%
      ggplot2::ggplot(ggplot2::aes(x = .data$k, y = .data$Score)) +
      ggplot2::geom_point() +
      ggplot2::labs(title = "Calinsky-Harabasz index") +
      ggplot2::theme_bw()
  } else {
      scores
  }
  }


