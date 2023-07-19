#' Check Calinsky-Harabasz index
#'
#' @inheritParams check_DB
#'
#'
#' @return Vector with Calinsky-Harabasz index for each pre-specified k.
#' @export
#'
#' @examples
#' library(dplyr)
#' sample_2044662 <- nice_tidy %>% filter(Sample == "ERR2044662") %>% pull(Abundance)
#' check_CH(sample_2044662)
#'
#' # To change range
#' check_CH(sample_2044662, range = 4:11)
#'
check_CH <- function(data, range = 3:10){
  # Calculate Calinsky-Harabasz index
    sapply(range, function(k){
    clusterSim::index.G1(x = data,
                         cl = cluster::pam(data, k = k, cluster.only = TRUE)
    )
  })
}
