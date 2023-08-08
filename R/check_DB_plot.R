#' Plots Davies-Bouldin Index for a single sample
#'
#' @param data Data frame with Davies-Bouldin scores for each k.
#' @inheritParams check_DB
#'
#' @return A plot with Davies-Bouldin score for each pre-specified k in a given sample.
#' @export
#'
#' @examples
#'
#' library(dplyr)
#' sample_2044662 <- nice_tidy %>% filter(Sample == "ERR2044662") %>% pull(Abundance)
#' check_DB(sample_2044662)
#'
#' # To change range
#' check_DB(sample_2044662, range = 4:11)
#'
check_DB_plot <- function(data, range = 3:10){

  # Remove NAs
  data <- data[!is.na(data)]
  # Remove zeros
  data <- data[data > 0]

  # Conditions for function to run
  stopifnot(range > 1)
  stopifnot(range < length(unique(data)))
  stopifnot(is.vector(data))

  sapply(range, function(k){
    clusterSim::index.DB(x = data,
                         cl = cluster::pam(data, k = k, cluster.only = TRUE)
    )$DB
  })
}
