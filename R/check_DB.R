#' Check Davies-Bouldin Index
#'
#' @inheritParams define_rb
#' @param range The range of values of k to test, default is from 3 to 10.
#' @param with_plot If FALSE (default) returns a vector, but if TRUE will returna plot with the scores.
#'
#' @return A vector or plot with Davies-Bouldin index for each pre-specified k in a given sample.
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
check_DB <- function(data,
                     sample_id,
                     samples_col = "Sample",
                     abundance_col = "Abundance",
                     range = 3:10,
                     with_plot = FALSE, ...){

  # range can not begin at 1
  stopifnot(range > 1)

  # Match samples_col and abundance_col with Samples and Abundance, respectively
  data <-
    data %>%
    rename(Sample = all_of(samples_col),
           Abundance = all_of(abundance_col)) %>%
    filter(.data$Sample == all_of(sample_id)) %>%
    filter(.data$Abundance > 0, !is.na(.data$Abundance))

  # Make vector with abundance scores
  pulled_data <- pull(data, Abundance)

  # Before continuing, verify if max k was reached in range provided
  stopifnot(range < length(unique(pulled_data)))

  scores <- sapply(range, function(k){
    clusterSim::index.DB(x = pulled_data,
                         cl = cluster::pam(pulled_data, k = k, cluster.only = TRUE)
    )$DB
  })

  if(isTRUE(with_plot)){
    plot(y = scores, x = range, main = "Davies-Boulding index", ...)
  } else {

    scores
  }

  #
}
