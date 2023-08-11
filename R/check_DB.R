#' Check Davies-Bouldin Index
#'
#' Calculates Davies-Bouldin (DB)Index for a given sample.
#'
#' DB is an index used to decide the number of clusters in a clustering algorithm.
#' This function, [check_DB()], calculates the DB index for every k in a pre-specified range
#' of values. Thus providing a score for each number of clusters tested (k). The default
#' range of cluster values (k) is `range = 3:10` (see why this is in Pascoal et al., 2023).
#' However, this function may calculate the DB index for all possible k's.
#'
#' Note that DB index is not an absolute value that indicates the quality of a single clustering.
#' Instead, it allows the comparison of clustering results. Thus, if you have several clusterings, the
#' best one will be the one with higher DB index.
#'
#' @details
#' **Data input**
#'
#' This function takes a data.frame with a column for samples and a column for abundance
#' (minimum), but can take any number of other columns. It will then filter the specific sample
#' that you want to analyze. You can also pre-filter for your specific sample, but you still need to
#' provide the sample ID (sample_id) and the table always needs a column for Sample and another for Abundance
#' (indicate how you name them with the arguments samples_col and abundance_col).
#'
#' @details
#' **Output options**
#'
#' The default option returns a vector with DB scores for each k. This is a simple output that can then be used
#' for other analysis. However, we also provide the option to show a plot (set `with_plot = TRUE`) with
#' the DB score for each k.
#'
#' @details
#' **Explanation of Davies-Bouldin index**
#'
#' The DB index (Davies and Bouldin, 1979) measures the similarity between any number of clusters.
#'
#'
#'
#' @references
#' Davies, D. L., & Bouldin, D. W. (1979). A Cluster Separation Measure. IEEE Transactions on Pattern Analysis and Machine Intelligence, PAMI-1(2).
#'
#' @inheritParams define_rb
#' @param sample_id String with name of the sample to apply this function.
#' @param range The range of values of k to test, default is from 3 to 10.
#' @param with_plot If FALSE (default) returns a vector, but if TRUE will return a plot with the scores.
#'
#' @return A vector or plot with Davies-Bouldin index for each pre-specified k in a given sample.
#' @export
#'
#'
#' @examples
#'
#' library(dplyr)
#' # Just scores
#' check_DB(nice_tidy, sample_id = "ERR2044662")
#'
#' # To change range
#' check_DB(nice_tidy, sample_id = "ERR2044662", range = 4:11)
#'
#' # To see a simple plot
#' check_DB(nice_tidy, sample_id = "ERR2044662", range = 4:11, with_plot=TRUE)
#'
#'
check_DB <- function(data,
                     sample_id,
                     samples_col = "Sample",
                     abundance_col = "Abundance",
                     range = 3:10,
                     with_plot = FALSE, ...){
  # range can not begin at 1
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
      stopifnot(range <= length(unique(pulled_data)))
      scores <- sapply(range,
                       function(k){
                         clusterSim::index.DB(x = pulled_data,
                                              cl = cluster::pam(pulled_data,
                                                                k = k,
                                                                cluster.only = TRUE))$DB})

    if(isTRUE(with_plot)){
      scores_data.frame <- data.frame(Score = scores, k = range)
      scores_data.frame %>%
        ggplot2::ggplot(ggplot2::aes(x = .data$k, y = .data$Score)) +
        ggplot2::geom_point() +
        ggplot2::labs(title = "Davies-Boulding index") +
        ggplot2::theme_bw()
    } else {
      scores
    }
  }
