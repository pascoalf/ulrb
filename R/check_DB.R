#' Check Davies-Bouldin Index
#'
#' @param data Vector of abundance scores from a single sample.
#' @param range The range of values of k to test.
#'
#' @return A vector with Davies-Boulding index for eac pre-specified k.
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
check_DB <- function(data, range = 3:10){
  stopifnot(range > 1)
  stopifnot(range < length(unique(data)))
  stopifnot(is.vector(data))
  sapply(range, function(k){
    clusterSim::index.DB(x = data,
                         cl = cluster::pam(data, k = k, cluster.only = TRUE)
    )$DB
  })
}
