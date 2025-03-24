#' Check Calinski-Harabasz index
#'
#' Calculates Calinski-Harabasz pseudo F-statistic (CH) for a given sample
#'
#' CH is an index used to decide the number of clusters in a clustering algorithm.
#' This function, [check_CH()], calculates the CH index for every k in a pre-specified range
#' of values. Thus providing a score for each number of clusters tested (k). The default
#' range of cluster values (k) is `range = 3:10` (see why this is in Pascoal et al., 2024, in peer review).
#' However, this function may calculate the CH index for all possible k's.
#'
#' Note that CH index is not an absolute value that indicates the quality of a single clustering.
#' Instead, it allows the comparison of clustering results. Thus, if you have several clusterings, the
#' best one will be the one with higher CH index.
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
#' The default option returns a vector with CH scores for each k. This is a simple output that can then be used
#' for other analysis. However, we also provide the option to show a plot (set `with_plot = TRUE`) with
#' the CH score for each k.
#'
#'
#' @details
#' **Explanation of Calinski-Harabasz index**
#'
#' The CH index is a **variance ratio criterion**, it measures both **separation** and **density** of the clusters.
#' The higher, the better, because it means that the points within the same cluster are close to each other; and
#' the different clusters are well separated.
#'
#' You can see CH index as:
#'
#' \deqn{CH = \frac{\text{inter cluster dispersion}}{\text{intra cluster dispersion}}}
#'
#' To calculate inter-cluster:
#'
#' Let \eqn{k} be the number of clusters and BGSS be the Between-group sum of squares,
#'
#' inter-cluster dispersion is \deqn{\frac{BGSS}{(k-1)}}
#'
#' To calculate BGSS:
#'
#' Let \eqn{n_k} be the number of observations in a cluster,
#' \eqn{C} be the centroid of the dataset (barycenter) and \eqn{C_k} the centroid of
#' a cluster,
#'
#' \deqn{BGSS = \sum_{k = 1}^{k}{n_k * \left\lvert C_k-C \right\rvert^2}}
#'
#' Thus, the BGSS multiplies the distance between the cluster centroid and
#' the centroid of the whole dataset, by all observations in a given cluster,
#' for all clusters.
#'
#' To calculate intra-cluster dispersion:
#'
#' Let \eqn{WGSS} be the Within Group Sum of Squares and \eqn{N} be the total
#' number of observations in the dataset.
#'
#' intra-cluster dispersion
#'
#' \deqn{\frac{WGSS}{(N-1)}}
#'
#' Let \eqn{X_ik} be i'th observation of a cluster and
#' \eqn{n_k} be the number of observations in a cluster.
#'
#' \deqn{WGSS = \sum_{k=1}^{k}\sum_{i=1}^{n_k}\left\lvert X_ik - C_k \right\rvert}
#'
#' Thus, WGSS measures the distance between observations and their cluster center; if divided by the
#' total number of observations, then gives a sense of intra-dispersion.
#'
#' Finally, the CH index can be given by:
#'
#' \deqn{CH = \frac{\sum_{k = 1}^{k}{n_k * \left\lvert C_k-C \right\rvert^2}}
#'  {\sum_{k=1}^{k}\sum_{i=1}^{n_k}\left\lvert X_ik - C_k \right\rvert}
#'  \frac{(N-k)}{(k-1)}}
#'
#'
#' @seealso [clusterSim::index.G1]
#'
#' @inheritParams check_DB
#'
#' @return Vector or plot with Calinski-Harabasz index for each pre-specified k.
#' @export
#'
#' @references
#' Calinski, T., & Harabasz, J. (1974). A dendrite method for cluster analysis. Communications in Statistics - Theory and Methods, 3(1), 1–27.
#' Pascoal et al. (2025). Definition of the microbial rare biosphere through unsupervised machine learning. Communications Biology.
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
  if(length(range) > length(unique(pulled_data))){
    stop("The range of k values must be equal or less than the number of different abundance scores.")
  }

  # Calculate Calinski-Harabasz index
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
      ggplot2::labs(title = "Calinski-Harabasz index") +
      ggplot2::theme_bw()
  } else {
      scores
  }
  }


