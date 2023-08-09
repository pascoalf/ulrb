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
#' # Just scores
#' check_avgSil(nice_tidy, sample_id = "ERR2044662")
#'
#' # To change range
#' check_avgSil(nice_tidy, sample_id = "ERR2044662", range = 4:11)
#'
#' # To see a simple plot
#' check_avgSil(nice_tidy, sample_id = "ERR2044662", range = 4:11, with_plot=TRUE)
#'
#' # If inside_nest = TRUE (not recommended)
#' nice_tidy %>%
#'   filter(Sample == "ERR2044662") %>%
#'   pull(Abundance) %>%
#'   check_avgSil(inside_nest = TRUE)
#'
check_avgSil <- function(data,
                         sample_id = NULL,
                         samples_col = "Sample",
                         abundance_col = "Abundance",
                         range = 3:10,
                         with_plot = FALSE,
                         inside_nest = FALSE, ...){

  # Conditions for function to run
  stopifnot(range > 1)

  # option for nests
  if(isTRUE(inside_nest)){
    stopifnot(!isTRUE(with_plot))
    if(!is.vector(data)){
      stop("If inside_nest = TRUE, then data must be a vector.")
    }
    sapply(range, function(k){
      mean(
        cluster::silhouette(
          cluster::pam(data, k)$clustering,
          stats::dist(data))[,3])
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

  # Calculate Average Silhouette score index
  scores <- sapply(range, function(k){
    mean(
      cluster::silhouette(
        cluster::pam(pulled_data, k)$clustering,
        stats::dist(pulled_data))[,3])
  })

  if(isTRUE(with_plot)){
    plot(y = scores, x = range, main = "Average Silhouette score", ...)
  } else {
    scores
  }
}
}
