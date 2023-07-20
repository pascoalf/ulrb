#' Evaluate sample k
#'
#' This functions calculates three indices (Davies-Bouldin, Calinsky-Harabasz and average Silhouette score) for each k.
#' Calculations are made for a single sample and for a default range of k that goes from 3 to 10.
#'
#'
#' @inheritParams check_DB
#' @param ... Extra arguments.
#'
#' @return testing
#' @export
#'
#' @examples
#' #testing
evaluate_sample_k <- function(data, range = 3:10, ...){
  ## One sample
  data.frame(DB = check_DB(data, range = range),
             CH = check_CH(data, range = range),
             average_Silhouette = check_avgSil(data, range = range),
             k = range)
}
