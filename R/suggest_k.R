#' Suggest k
#'
#' The best k is selected for each sample, based on the selected index. If different k's are obtained for different samples
#' (probable) then we calculate the mean value of k and return it as an integer.
#'
#'
#' @inheritParams evaluate_k
#' @param index Index used to select best k. Can be one of: "Average Silhouette Score", "Davies-Bouldin" or "Calinsky-Harabasz".
#' @param detailed If False (default) returns an integer with best overall k. If TRUE, returns a list with full details.
#'
#' @return Integer indicating best k from selected index. Optionally, can return a list with details.
#' @export
#'
#' @examples
#' #test
suggest_k <- function(data,
                      range = 3:10,
                      samples_id = "Sample",
                      abundance_id = "Abundance",
                      index = "Average Silhouette Score",
                      detailed = FALSE,
                      ...){
  stopifnot(range > 1)
  # calculate maximum k
  maxk = data %>%
    group_by(.data$Sample) %>%
    summarise(topK = length(unique(.data$Abundance))) %>%
    ungroup() %>%
    pull(topK) %>%
    min()
  #
  stopifnot(range < maxk)
  all_scores <-
    evaluate_k(data = data,
               range = range,
               samples_id = samples_id,
               abundance_id = abundance_id)

  best_avgSil_k <-
    all_scores %>%
    group_by(.data$Sample) %>%
    filter(.data$average_Silhouette == max(.data$average_Silhouette)) %>%
    select(.data$Sample, .data$average_Silhouette, .data$k) %>%
    ungroup()

  best_DB_k <-
    all_scores %>%
    group_by(.data$Sample) %>%
    filter(.data$DB == min(.data$DB)) %>%
    select(.data$Sample, .data$DB, .data$k) %>%
    ungroup()

  best_CH_k <-
    all_scores %>%
    group_by(.data$Sample) %>%
    filter(.data$CH == max(.data$CH)) %>%
    select(.data$Sample, .data$CH, .data$k) %>%
    ungroup()

  if(detailed == FALSE){
  if(index == "Average Silhouette Score"){
    best_k <- pull(best_avgSil_k, .data$k) %>% mean() %>% as.integer()
  }
  if(index == "Davies-Bouldin"){
    best_k <- pull(best_DB_k, .data$k) %>% mean() %>% as.integer()
  }
  if(index == "Calinsky-Harabasz"){
    best_k <- pull(best_CH_k, .data$k) %>% mean() %>% as.integer()
  }} else {
    ## some list

    best_k <-
      list("This list contains several details that might help you decide a k parameter.",
           data.frame(Score =
                        c("Davies-Bouldin index",
                          "Calinsky-Harabasz index",
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
           CalinskyHarabasz = best_CH_k,
           averageSilhouette = best_avgSil_k)

  }
return(best_k)
}
