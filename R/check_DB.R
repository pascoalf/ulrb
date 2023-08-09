#' Check Davies-Bouldin Index
#'
#' @inheritParams define_rb
#' @param sample_id String with name of the sample to apply this function.
#' @param range The range of values of k to test, default is from 3 to 10.
#' @param with_plot If FALSE (default) returns a vector, but if TRUE will return a plot with the scores.
#' @param inside_nest Default is FALSE. This argument indicates if the function is being used within a nest. If TRUE, a single vector of Abundance is accepted and returned.
#'
#' @return A vector or plot with Davies-Bouldin index for each pre-specified k in a given sample.
#' @export
#'
#' @examples
#'
#' library(dplyr)
#'
#'
#' # Just scores
#' check_DB(nice_tidy, sample_id = "ERR2044662")
#'
#' # To change range
#' check_DB(nice_tidy, sample_id = "ERR2044662", range = 4:11)
#'
#' # To see a simple plot
#' check_DB(nice_tidy, sample_id = "ERR2044662", range = 4:11, with_plot=TRUE)
#'
#' # If inside_nest = TRUE (not recommended)
#' nice_tidy %>%
#'   filter(Sample == "ERR2044662") %>%
#'   pull(Abundance) %>%
#'   check_DB(inside_nest = TRUE)
#'
check_DB <- function(data,
                     sample_id = NULL,
                     samples_col = "Sample",
                     abundance_col = "Abundance",
                     range = 3:10,
                     with_plot = FALSE,
                     inside_nest = FALSE, ...){

  # range can not begin at 1
  stopifnot(range > 1)

  if(isTRUE(inside_nest)){
    stopifnot(!isTRUE(with_plot))
    if(!is.vector(data)){stop("If inside_nest = TRUE, then data must be a vector.")}
    sapply(range, function(k){
      clusterSim::index.DB(x = data,
                           cl = cluster::pam(data, k = k, cluster.only = TRUE))$DB
    })
  } else {
    if(is.null(sample_id)){stop("Please provide the ID of the sample you want to check in argument sample_id.")}

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
    stopifnot(length(range) <= length(unique(pulled_data)))

    scores <- sapply(range, function(k){
      clusterSim::index.DB(x = pulled_data,
                           cl = cluster::pam(pulled_data, k = k, cluster.only = TRUE))$DB
    })

    if(isTRUE(with_plot)){
      plot(y = scores, x = range, main = "Davies-Boulding index", ...)
    } else {
      scores
    }
  }
}
