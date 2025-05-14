#' Check Davies-Bouldin Index
#'
#' Calculates Davies-Bouldin (DB) index for a given sample.
#'
#' DB is an index used to decide the number of clusters in a clustering algorithm.
#' This function, [check_DB()], calculates the DB index for every k in a pre-specified range
#' of values. Thus providing a score for each number of clusters tested (k). The default
#' range of cluster values (k) is `range = 3:10` (see why this is in Pascoal et al., 2025).
#' However, this function may calculate the DB index for all possible k's.
#'
#' Note that DB index is not an absolute value that indicates the quality of a single clustering.
#' Instead, it allows the comparison of clustering results. Thus, if you have several clusterings, the
#' best one will be the one with lowest DB index.
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
#' The DB index (Davies and Bouldin, 1979) is an averaged measure of cluster
#' similarity to the closest cluster. This provides a sense of how separated the clusters are.
#'
#' Lower DB scores are better, because they represent more distinct clusters.
#' Higher values of DB indicate overlapping clusters.
#'
#' Let \eqn{N} be the number of clusters and \eqn{R_i} the similarity between the i'th cluster and
#' the cluster most similar to it.
#' The DB index is calculated as the mean similarity between each cluster and the most similar cluster,
#'
#' \deqn{DB = \frac{1}{N}\sum_{i=1}^{N}R_i}
#'
#' Thus, \eqn{R_i} is the maximum similarity among all possible combinations of
#'  \eqn{R_{ij}}, with \eqn{i \neq j}.
#'
#' To get \eqn{R_ij}, let \eqn{S_i} be the intra-cluster dispersion of \eqn{i},
#' \eqn{S_j} be the intra-cluster dispersion of cluster \eqn{j} and \eqn{M_ij} be the
#' distance between clusters \eqn{i} and \eqn{j}.
#'
#' The similarity between any two clusters, \eqn{i} and \eqn{j}, is:
#'
#'  \deqn{ R_{ij} = \frac{S_i + S_j}{M_ij}}
#'
#' The distance between any two clusters, \eqn{M_ij}, is measured as the
#' distance between the
#' centroids of both clusters, \eqn{\left\lvert C_i - C_j \right\rvert}.
#'
#' The dispersion of clusters, \eqn{S_i}, provides a sense of intra-dispersion
#'  of a given cluster.
#'
#' To calculate \eqn{S_i}, let \eqn{T_i} and \eqn{T_j} be the number of
#' observations in \eqn{i} and \eqn{j}, respectively; let \eqn{X_j} be the value for
#' j'th observation (again, \eqn{i \neq j}).
#'
#' \deqn{S_i = \sqrt{\frac{1}{T_i}\sum_{j=1}^{T_i}\left\lvert X_j - C_i \right\rvert}}
#'
#'  **Note** that this is the case for euclidean distances.
#'
#'
#' @references
#' Davies, D. L., & Bouldin, D. W. (1979). A Cluster Separation Measure. IEEE Transactions on Pattern Analysis and Machine Intelligence, PAMI-1(2).
#' Pascoal, F., Branco, P., Torgo, L. et al. Definition of the microbial rare biosphere through unsupervised machine learning. Commun Biol 8, 544 (2025). https://doi.org/10.1038/s42003-025-07912-4
#'
#' @inheritParams define_rb
#' @param sample_id String with name of the sample to apply this function.
#' @param samples_col String with name of column with sample names.
#' @param range The range of values of k to test, default is from 2 to 10.
#' @param with_plot If FALSE (default) returns a vector, but if TRUE will return a plot with the scores.
#'
#' @return A vector or plot with Davies-Bouldin index for each pre-specified k in a given sample.
#' @export
#'
#' @seealso [clusterSim::index.DB()]
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
                     range = 2:10,
                     with_plot = FALSE, ...){
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
             Abundance = all_of(abundance_col)) %>%
      filter(.data$Sample == sample_id) %>%
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
        ggplot2::labs(title = "Davies-Bouldin index") +
        ggplot2::theme_bw()
    } else {
      scores
    }
  }
