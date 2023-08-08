#' Check Calinsky-Harabasz index
#'
#' @inheritParams check_CH
#'
#'
#' @return Vector or plot with Calinsky-Harabasz index for each pre-specified k.
#' @export
#'
#' @examples
#' library(dplyr)
#' sample_2044662 <- "ERR2044662"
#' # Just scores
#' check_CH(nice_tidy, sample_id = "ERR2044662")
#'
#' # To change range
#' check_CH(nice_tidy, sample_id = "ERR2044662", range = 4:11)
#'
#' # To see a simple plot
#' check_CH(nice_tidy, sample_id = "ERR2044662", range = 4:11, with_plot=TRUE)
#'
#' # If inside_nest = TRUE (not recommended)
#' #' nice_tidy %>%
#'   filter(Sample == "ERR2044662") %>%
#'   pull(Abundance) %>%
#'   check_CH(inside_nest = TRUE)
#'
check_CH <- function(data,
                     sample_id = NULL,
                     samples_col = "Sample",
                     abundance_col = "Abundance",
                     range = 3:10,
                     with_plot = FALSE,
                     inside_nest = FALSE, ...){

  # Conditions for function to run
  stopifnot(range > 1)
  #
  if(isTRUE(inside_nest)){
    stopifnot(!isTRUE(with_plot))
    if(!is.vector(data)){
      stop("If inside_nest = TRUE, then data must be a vector.")
      }
    sapply(range, function(k){
      clusterSim::index.G1(x = data,
                           cl = cluster::pam(data, k = k, cluster.only = TRUE))
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
  pulled_data <- pull(data, Abundance)

  # Before continuing, verify if max k was reached in range provided
  stopifnot(range < length(unique(pulled_data)))

  # Calculate Calinsky-Harabasz index
  scores <- sapply(range, function(k){
    clusterSim::index.G1(x = pulled_data,
                         cl = cluster::pam(pulled_data, k = k, cluster.only = TRUE)
    )
  })

  if(isTRUE(with_plot)){
    plot(y = scores, x = range, main = "Calinsky-Harabasz index", ...)
  } else {
      scores
  }
      }
  }


