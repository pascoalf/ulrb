#' Define Rare Biosphere
#'
#' @param data A tibble with, at least, a column for Abundance and Sample. Additional columns are allowed.
#' @param classification_vector A vector of strings with the names for each cluster, from lower to higher abundance. Default is c("Rare", "Undetermined", "Abundance")
#' @param samples_col String with name of column with sample names
#' @param abundance_col String with name of column with abundance values
#' @param simplified Can be TRUE/FALSE. Default (FALSE) provides an additional column with detailed pam() results (pam_details) and Silhouette scores (Silhouette_scores).
#' @param automatic If TRUE, then it will automatically select the number of classifications (or k),
#' based on the index argument.
#' @inheritParams suggest_k
#'
#' @return A tibble with the initial columns and new columns for the cluster and classification of each species.
#' @export
#'
#' @examples
#'
#' define_rb(nice_tidy)
#'
#' # If data is in wide format, with samples in cols
#' sample_names <- c("ERR2044662", "ERR2044663", "ERR2044664",
#'                    "ERR2044665", "ERR2044666", "ERR2044667",
#'                    "ERR2044668", "ERR2044669", "ERR2044670")
#'
#' nice_tidy <- prepare_tidy_data(nice, sample_names = sample_names, samples_in = "cols")
#' define_rb(nice_tidy)
#'
#' # If data is in wide format, with samples in rows
#' ### do this later ###
#'
#' @import dplyr
#' @importFrom rlang .data
define_rb <- function(data,
                      classification_vector = c("Rare","Undetermined","Abundant"),
                      samples_col = "Sample",
                      abundance_col = "Abundance",
                      simplified = FALSE,
                      automatic = FALSE,
                      index = "Average Silhouette Score",
                      ...) {

  #If automatic, use suggest_k()
  if(isTRUE(automatic)){
    automatic_k <- suggest_k(data, index = index, ...)
    classification_vector <- seq_along(1:automatic_k)
    message(paste0("K= ", automatic_k, " based on ", index,"."))
  }

  # Define number of cluster based on possible classifications
  k <- length(classification_vector)

  # Match samples_col and abundance_col with Samples and Abundance, respectively
  data <-
    data %>%
    rename(Sample = all_of(samples_col),
           Abundance = all_of(abundance_col))

  # Calculate kmedoids
    ## Apply cluster algorithm
    clustered_data <-
      data %>%
      filter(.data$Abundance > 0, !is.na(.data$Abundance)) %>%
      group_by(.data$Sample, .add = TRUE) %>%
      tidyr::nest() %>%
      mutate(pam_object = purrr::map(.x = data,
                                     .f = ~cluster::pam(.x$Abundance,
                                                        k = k,
                                                        diss = FALSE))) %>%
      mutate(Level = purrr::map(.x = .data$pam_object, .f = ~.x[[3]]), # obtain clusters
             Silhouette_scores = purrr::map(.x = .data$pam_object, .f = ~.x[[7]][[1]][,3])) %>%  ## obtain silhouette plots
      tidyr::unnest(cols = c(data, .data$Level, .data$Silhouette_scores))

  # Make classification table
  classification_table <- clustered_data %>%
    group_by(.data$Sample, Level = as.factor(.data$Level), .add = TRUE) %>%
    summarize(Cluster_median_abundance = stats::median(.data$Abundance)) %>%
    arrange(.data$Sample, .data$Cluster_median_abundance) %>%
    mutate(Classification = factor(classification_vector, levels = classification_vector))

  # Apply classification_table to classify clusters
  classified_clusters <- clustered_data %>%
    mutate(Level = as.factor(.data$Level)) %>%
    left_join(classification_table)

  # Evaluate
  classified_clusters <-
    classified_clusters %>%
    group_by(.data$Sample, .data$Classification) %>%
    tidyr::nest() %>%
    mutate(median_Silhouette = purrr::map(.x = data, .f = ~median(.x$Silhouette_scores))) %>%
    mutate(Evaluation = purrr::map(.x = .data$median_Silhouette, .f = ~case_when(median_Silhouette > 0.9 ~ "Very good",
                                                                                 median_Silhouette > 0.75 ~ "Good",
                                                                                 median_Silhouette > 0.5 ~ "Sufficient",
                                                                                 median_Silhouette <= 0.5 ~ "Bad"))) %>%
    tidyr::unnest(cols = c(data, .data$median_Silhouette, .data$Evaluation))

  ## the nest steps only check if a warning is necessary, the output is classified clusters
  clusters_report <- classified_clusters %>%
    select(.data$Sample, .data$Classification, .data$median_Silhouette, .data$Evaluation) %>% ### break from here
    distinct() %>%
    group_by(.data$Classification) %>%
    count(.data$Evaluation)

  # Count number of samples with bad scores
  bad_samples <- clusters_report %>%
    filter(.data$Evaluation == "Bad") %>%
    pull(.data$n) %>%
    sum()

  #
  if(bad_samples > 0){
    warning(paste(bad_samples, "samples got a bad Silhouette score. Consider changing the number of classifications."))
    message("If half the observations within a classification are below 0.5 Silhouette score, we consider that the clustering was 'Bad'.")
    message("Check 'Evaluation' collumn for more details.")
  }


  # Option to simplify
  if(simplified == TRUE){
    # Remove unnecessary columns
    classified_clusters %>%
      select(-.data$Level, -.data$pam_object, -.data$Evaluation, -.data$median_Silhouette, -.data$Silhouette_scores, -.data$Cluster_median_abundance)
  }

  return(classified_clusters)
}
