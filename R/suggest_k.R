#' Suggest k
#'
#' The best k is selected for each sample, based on the selected index. If different k's are obtained for different samples
#' (probable) then we calculate the mean value of k and return it as an integer.
#'
#'
#' @inheritParams evaluate_k
#' @param index Index used to select best k. Can be one of: "Average Silhouette Score", "Davies-Bouldin" or "Calinsky-Harabasz".
#'
#' @return Integer indicating best k from selected index. Optionally, can return a list with details.
#' @export
#'
#' @examples
#' #test
suggest_k <- function(data,
                      range,
                      sample_ids,
                      abundance_id,
                      index = "Average Silhouette Score",
                      detailed = FALSE,
                      ...){


  all_scores <-
    evaluate_k(data = data,
               range = range,
               sample_ids = sample_ids,
               abundance_id = abundance_id)

  best_avgSil_k <-
    all_scores %>%
    group_by(.data$Sample) %>%
    filter(.data$average_Silhouette == max(.data$average_Silhouette)) %>%
    select(.data$Sample, .data$average_Silhouette, .data$k) %>%
    tidyr::ungroup()

  best_DB_k <-
    all_scores %>%
    group_by(.data$Sample) %>%
    filter(.data$DB == min(.data$DB)) %>%
    select(.data$Sample, .data$DB, .data$k) %>%
    tidyr::ungroup()

  best_CH_k <-
    all_scores %>%
    group_by(.data$Sample) %>%
    filter(.data$CH == max(.data$CH)) %>%
    select(.data$Sample, .data$CH, .data$k) %>%
    tidyr::ungroup()

  if(detailed == FALSE){
  if(index == "Average Silhouette Score"){
    best_k <- pull(best_avgSil_k, k) %>% mean() %>% as.integer()
  }
  if(index == "Davies-Bouldin"){
    best_k <- pull(best_DB_k, k) %>% mean() %>% as.integer()
  }
  if(index == "Calinsky-Harabasz"){
    best_k <- pull(best_CH_k, k) %>% mean() %>% as.integer()
  }} else {
    ## some list
  }

}
