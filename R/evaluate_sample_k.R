#' Evaluate sample k
#'
#' This functions calculates three indices (Davies-Bouldin, Calinsky-Harabasz and average Silhouette score) for each k.
#' Calculations are made for a single sample and for a default range of k that goes from 3 to 10.
#'
#'
#' @inheritParams check_DB
#' @inheritParams plot_ulrb_clustering
#' @param ... Extra arguments.
#'
#' @return testing
#' @export
#'
#' @examples
#' library(dplyr)
#' #
#' evaluate_sample_k(nice_tidy, sample_id = "ERR2044662")
#'
#' # To change range
#' evaluate_sample_k(nice_tidy, sample_id = "ERR2044662", range = 4:11)
#'
evaluate_sample_k <- function(data,
                              sample_id = NULL,
                              range = 3:10,
                              with_plot = FALSE,
                              inside_nest = FALSE,
                              ...){
  # option for nests
  if(isTRUE(inside_nest)){
    data.frame(DB = check_DB(data, sample_id = sample_id, range = range, inside_nest = TRUE, ...),
               CH = check_CH(data, sample_id = sample_id, range = range, inside_nest = TRUE, ...),
               average_Silhouette = check_avgSil(data, sample_id = sample_id, range = range, inside_nest = TRUE, ...),
               k = range)
  } else {
    if(is.null(sample_id)){stop("Please provide the ID of the sample you want to check in argument sample_id.")}

  ## One sample
  scores <- data.frame(DB = check_DB(data, sample_id = sample_id, range = range, ...),
             CH = check_CH(data, sample_id = sample_id, range = range, ...),
             average_Silhouette = check_avgSil(data, sample_id = sample_id, range = range, ...),
             k = range)

  if(isTRUE(with_plot)){
  scores_tidy <- scores %>%
    tidyr::pivot_longer(cols = c("DB", "CH", "average_Silhouette"),
                        names_to = "Index",
                        values_to = "Score") %>%
    mutate(Index = case_when(Index == "DB" ~ "Davies-Bouldin",
                             Index == "CH" ~ "Calinsky-Harabasz",
                             TRUE ~ "Average Silhouette score" ))

  scores_tidy %>%
    ggplot2::ggplot(ggplot2::aes(x = .data$k, y = .data$Score)) +
    ggplot2::geom_point() +
    ggplot2::facet_wrap(~.data$Index, scales = "free_y") +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "top") +
    ggplot2::scale_color_manual(values = .data$colors) +
    ggplot2::labs(title = paste("Metrics obtained for ", sample_id))

  } else {
    scores
    }
  }
}
