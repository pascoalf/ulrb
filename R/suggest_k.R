#' Suggest k
#'
#' Tool to help decide how many clusters to use for partition around medoids algorithm.
#'
#' The best k is selected for each sample, based on the selected index.
#' If different k's are obtained for different samples (probable) then we
#' calculate the mean value of k and return it as an integer. Alternatively, we can
#' return a more detailed result in the form of a list.
#'
#' **Note**: this function is used within [define_rb()], with default parameters, for the
#' optional automatic selection of k.
#'
#' @details
#' **Detailed option**
#'
#' If `detailed = TRUE`, then the output is a list with information to help decide for k.
#' More specifically, the list will include:
#'
#' - A data.frame summarizing what information each index provides and how to interpret the value.
#' - A brief summary indicating the number of samples in the dataset and the range of k values used.
#' - A data.frame with the best k for each sample, based on each index.
#'
#' @details
#' **Automatic k selection**
#'
#' If `detailed = FALSE`, this function will provide a single integer with the best k.
#' The **default** decision is based on the maximum average Silhouette score obtained
#' for the values of k between 3 and 10. To better understand why the average Silhouette score and
#' this range of k's were selected, we refer to Pascoal et al., 2025 and to
#' vignette("explore-classifications").
#'
#' Alternatively, this function can also provide the best k, as an integer, based on another index
#' (Davies-Bouldin and Calinski-Harabasz) and can compare the entire of possible k's.
#'
#'
#' @inheritParams evaluate_k
#' @param index Index used to select best k. Can be one of: "Average Silhouette Score", "Davies-Bouldin" or "Calinski-Harabasz".
#' @param detailed If False (default) returns an integer with best overall k. If TRUE, returns a list with full details.
#'
#' @return Integer indicating best k from selected index. Optionally, can return a list with details.
#' @export
#'
#' @seealso [evaluate_k()], [evaluate_sample_k()], [check_DB()], [check_CH()], [check_avgSil()], [cluster::pam()]
#'
#' @examples
#'
#'\donttest{
#' # Get the best k with default parameters
#' suggest_k(nice_tidy)
#'
#'
#' # Get detailed results to decide for yourself
#' suggest_k(nice_tidy, detailed = TRUE, range = 2:7)
#'
#' # Get best k, based on Davies-Bouldin index
#' suggest_k(nice_tidy, detailed = FALSE, index = "Davies-Bouldin")
#' }
#'
suggest_k <- function(data,
                      range = 3:10,
                      samples_col = "Sample",
                      abundance_col = "Abundance",
                      index = "Average Silhouette Score",
                      detailed = FALSE, ...){
  # range can not begin at 1
  if(min(range) == 1){
    stop("The range argument must start at 2.")
  }

  # stop if a vector is used as input
  if(is.vector(data)){stop("Input must be a data.frame with at least a column for Samples and another for Abundance.")}

  # stop if abundance values are not numeric (integer or double type)
  if(!is.numeric(pull(data, all_of(abundance_col)))){
    stop("The column with abundance scores must be numeric (integer our double type).")
  }

  # calculate maximum k
  maxk = data %>%
    group_by(.data$Sample) %>%
    summarise(topK = length(unique(.data$Abundance))) %>%
    ungroup() %>%
    pull(.data$topK) %>%
    min()
  #
  stopifnot(range < maxk)
  all_scores <-
    evaluate_k(data = data,
               range = range,
               samples_col = samples_col,
               abundance_col = abundance_col,
               ...)

  best_avgSil_k <-
    all_scores %>%
    group_by(.data$Sample) %>%
    filter(.data$average_Silhouette == max(.data$average_Silhouette)) %>%
    select(Sample, average_Silhouette, k) %>%
    ungroup()

  best_DB_k <-
    all_scores %>%
    group_by(.data$Sample) %>%
    filter(.data$DB == min(.data$DB)) %>%
    select(Sample, DB, k) %>%
    ungroup()

  best_CH_k <-
    all_scores %>%
    group_by(.data$Sample) %>%
    filter(.data$CH == max(.data$CH)) %>%
    select(Sample, CH, k) %>%
    ungroup()

  if(detailed == FALSE){
  if(index == "Average Silhouette Score"){
    best_k <- pull(best_avgSil_k, .data$k) %>% mean() %>% as.integer()
  }
  if(index == "Davies-Bouldin"){
    best_k <- pull(best_DB_k, .data$k) %>% mean() %>% as.integer()
  }
  if(index == "Calinski-Harabasz"){
    best_k <- pull(best_CH_k, .data$k) %>% mean() %>% as.integer()
  }} else {
    ## some list

    best_k <-
      list("This list contains several details that might help you decide a k parameter.",
           data.frame(Score =
                        c("Davies-Bouldin index",
                          "Calinski-Harabasz index",
                          "Average Silhouette Score"),
                      Criteria =
                        c("Minimum value for best k",
                          "Maximum value for best k",
                          "Maximum value for best k"),
                      Details =
                        c("Measures cluster separation",
                          "Measures cluster definition",
                          "Measures cluster density")),
           SamplesSummary = paste("You study has",
                                  length(unique(pull(data, .data$Sample))),
           "samples. For each one we calculated all indices obtained for each k, from",
           min(range), "to", max(range)),
           DaviesBouldin = best_CH_k,
           CalinskiHarabasz = best_CH_k,
           averageSilhouette = best_avgSil_k)

  }
return(best_k)
}
