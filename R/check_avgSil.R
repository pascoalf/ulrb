#' Check average Silhouette score index
#'
#' Calculates average Silhouette score for a given sample.
#'
#' The average Silhouette score index provides a sense of cluster definition and separation.
#' It varies between -1 (complete cluster overlap) and 1 (no cluster overlap),
#' the closest to 1, the better. Thus,
#' **the k value with highest average Silhouette score is the best k**.
#' This is the standard metric used by the **ulrb** package for automation of the decision
#' of k, in functions [suggest_k()] and [define_rb()].
#'
#' **Note**: The average Silhouette score is different from the common calculation of
#' the Silhouette index, which provides a score for each observation in a clustering result.
#' Just like the name says, we are taking the average of all silhouette scores
#' obtained in a clustering result. In this way we can have a single, comparable
#' value for each k we test.
#'
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
#' Note that this function does not plot the classical Silhouette plot of a clustering result.
#' To do that particular plot, use the function [plot_ulrb_silhouette()] instead.
#'
#' @details
#' **Explanation of average Silhouette score**
#'
#' To calculate the Silhouette score for a single observation, let:
#' - \eqn{a} be the mean distance between an observation and all other
#'  observations from the same cluster; and
#' - \eqn{b} be the mean distance between all observations in a cluster and the
#' centroid of the nearest cluster.
#'
#' The silhouette score (Sil), is given by:
#'
#' \deqn{Sil = \frac{(b-a)}{max(a,b)}}
#'
#' Once you have the Silhouette score for all observations in a clustering result, just
#' take the simple mean and get the average Silhouette score.
#'
#' @details
#' **Silhouette score explanation**
#'
#' From the above formula, \eqn{Sil = \frac{(b-a)}{max(a,b)}}, it is clear that,
#' for a given observation:
#'
#'  - if \eqn{a > b}, the Silhouette score approaches **1**; this means that the
#'   distance between an observation and its own cluster is larger than the
#'   distance to the nearest different cluster. This is the distance that must be
#'   maximized so that all points in a cluster are more similar with each other,
#'   than they are with other clusters.
#'  - if \eqn{a = b}, then the Silhouette score is **0**; this means that the distance
#'  between the observation and its own cluster is equivalent to distance between
#'  the nearest different cluster.
#'  - if \eqn{a < b}, then the Silhouette score approaches **-1**; in this
#'  situation, an observation is nearer the nearest different cluster,
#'  than it is to its own cluster. Thus, a negative score indicates that the observation
#'  is not in the correct cluster.
#'
#' @details
#' **average Silhouette score intuition**
#'
#' If we take the average of the Silhouette score obtained for each observation in
#' a clustering result, then we have the ability to compare the overall success of that
#' clustering with another clustering. Thus, if we compare the average Silhouette
#' score across different k values, i.e. different number of clusters, we can
#' select the k with highest average Silhouette score.
#'
#' @inheritParams check_DB
#'
#' @return Vector with average Silhouette score index for each pre-specified k.
#' @export
#'
#' @seealso [define_rb()], [suggest_k()], [cluster::pam()], [cluster::silhouette()]
#'
#' @references
#' Rousseeuw, P. J. (1987). Silhouettes: A graphical aid to the interpretation and validation of cluster analysis. Journal of Computational and Applied Mathematics, 20(C), 53–65.
#' Pascoal, F., Branco, P., Torgo, L. et al. Definition of the microbial rare biosphere through unsupervised machine learning. Commun Biol 8, 544 (2025). https://doi.org/10.1038/s42003-025-07912-4
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
                         sample_id = NULL,
                         samples_col = "Sample",
                         abundance_col = "Abundance",
                         range = 2:10,
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
