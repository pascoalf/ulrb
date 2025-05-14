#' Evaluate sample k
#'
#' This functions calculates three indices (Davies-Bouldin, Calinsky-Harabasz and average Silhouette score) for each k.
#' Calculations are made for a single sample and for a default range of k that goes from 2 to 10.
#'
#' **Note**: To get the indices for all samples, use [evaluate_k()] instead.
#'
#' @details
#' **Data input**
#'
#' This function takes a data.frame with a column for samples and a column for abundance
#' (minimum), but can take any number of other columns. It will then filter the specific sample
#' that you want to analyze. You can also pre-filter for your specific sample, but you still need to
#' provide the sample ID (sample_id) and the table always needs a column for Sample and another for Abundance
#' (indicate how you name them with the arguments samples_col and abundance_col).
#'
#' @details
#' **Output options**
#'
#' The default option returns a data.frame with Davies-Bouldin, Calinsky-Harabasz and
#' average Silhouette scores for each k. This is a simple output that can then be used
#' for other analysis. However, we also provide the option to show a plot (set `with_plot = TRUE`).
#'
#' **Three indices are calculated by this function:**
#'
#'  - Davies-Bouldin with [check_DB()];
#'  - Calinsky-Harabasz with [check_DB()];
#'  - average Silhouette score [check_avgSil()].
#'
#'
#' @inheritParams check_DB
#' @inheritParams plot_ulrb_clustering
#' @param ... Extra arguments.
#'
#' @return A data.frame (or plot) with several indices for each number of clusters.
#' @export
#'
#' @seealso [check_CH()], [check_DB()], [check_avgSil()], [suggest_k()], [evaluate_k()]
#'
#' @examples
#' library(dplyr)
#' #
#' evaluate_sample_k(nice_tidy, sample_id = "ERR2044662")
#'
#' # To change range
#' evaluate_sample_k(nice_tidy, sample_id = "ERR2044662", range = 4:11)
#'
#' # To make simple plot
#' evaluate_sample_k(nice_tidy, sample_id = "ERR2044662", range = 4:11, with_plot =TRUE)
#'
evaluate_sample_k <- function(data,
                              sample_id,
                              samples_col = "Sample",
                              abundance_col = "Abundance",
                              range = 2:10,
                              with_plot = FALSE,
                              ...){
  # stop if a vector is used as input
  if(is.vector(data)){stop("Input must be a data.frame with at least a column for Samples and another for Abundance.")}

  # stop if abundance values are not numeric (integer or double type)
  if(!is.numeric(pull(data, all_of(abundance_col)))){
    stop("The column with abundance scores must be numeric (integer our double type).")
  }

# Ensure the range of k values is appropriate
#    # calculate maximum k
#  maxk = data %>%
#    summarise(topK = length(unique(.data$Abundance))) %>%
#    ungroup() %>%
#    pull(.data$topK) %>%
#    min()
  #
#  if(max(range) > maxk){
#    stop(c("Adjust the range of k values. The maximum number of clusters allowed for your sample is", " ", maxk))
#  }

  # Function to calculate maximum k of a sample
  sample_max_k <- function(data){
    data %>%
      filter(.data$Abundance > 0) %>%
      count(.data$Abundance) %>%
      pull(.data$Abundance) %>%
      length()
  }

  maxk <- data %>% sample_max_k()

  #
  if(max(range) > maxk){
    stop(c("Adjust the range of k values. The maximum number of clusters allowed
           for your samples is", " ", maxk, ". Try range = 2:", maxk-1))
  }

  ## One sample
  scores <- data.frame(DB = check_DB(data, sample_id = sample_id, range = range, samples_col = samples_col, abundance_col = abundance_col, ...),
                       CH = check_CH(data, sample_id = sample_id, range = range, samples_col = samples_col, abundance_col = abundance_col, ...),
                       average_Silhouette = check_avgSil(data, sample_id = sample_id, range = range, samples_col = samples_col, abundance_col = abundance_col, ...),
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
    ggplot2::labs(title = paste("Metrics obtained for ", sample_id))

  } else {
    scores
    }
  }

