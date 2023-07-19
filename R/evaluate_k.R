#' Evaluate k
#'
#' @inheritParams check_DB
#' @param ... Extra arguments.
#'
#' @return testing
#' @export
#'
#' @examples
#' #testing
evaluate_k <- function(data, range = 3:10, ...){

  ## One sample
    data.frame(DB = check_DB(data, range = range),
               CH = check_CH(data, range = range),
               average_Silhouette = check_avgSil(data, range = range),
               k = range)


  ### All samples
}
