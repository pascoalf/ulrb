#' Check Average Silhouette score index
#'
#' @inheritParams check_DB
#'
#'
#' @return Vector with average Silhouette score index for each pre-specified k.
#' @export
#'
#' @examples
#' library(dplyr)
#' sample_2044662 <- nice_tidy %>% filter(Sample == "ERR2044662") %>% pull(Abundance)
#' check_avgSil(sample_2044662)
#'
#' # To change range
#' check_avgSil(sample_2044662, range = 4:11)
#'
check_avgSil <- function(data, range = 3:10){
  # Remove NAs
  data <- data[!is.na(data)]
  # Remove zeros
  data <- data[data > 0]

  # Conditions for function to run
  stopifnot(range > 1)
  stopifnot(range < length(unique(data)))
  stopifnot(is.vector(data))

  # Calculate Average Silhouette score index
  sapply(range, function(k){
    mean(
      cluster::silhouette(
        cluster::pam(data, k)$clustering,
        stats::dist(data))[,3])
  })
}
