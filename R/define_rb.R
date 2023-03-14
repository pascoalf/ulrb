#' Define Rare Biosphere
#'
#' @param data A tibble with, at least, a column for Abundance and Sample. Additional columns are allowed.
#' @param classification_vector A vector of strings with the names for each cluster, from lower to higher abundance. Default is c("Rare", "Undetermined", "Abundance")
#' @param samples_id String with name of column with sample names
#' @param abundance_id String with name of column with abundance values
#' @param simplified Can be TRUE/FALSE. Default (FALSE) provides an additional column with detailed pam() results (pam_details) and Silhouette scores (Silhouette_scores).
#'
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
                      samples_id = "Sample",
                      abundance_id = "Abundance",
                      simplified = FALSE) {

  # Define number of cluster based on possible classifications
  k <- length(classification_vector)

  # Match samples_id and abundance_id with Samples and Abundance, respectively
  data <-
    data %>%
    rename(Sample = all_of(samples_id),
           Abundance = all_of(abundance_id))

  # Calculate kmedoids
  # Complete option
  if(simplified == FALSE){
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
             Silhouette_scores = purrr::map(.x = .data$pam_object, .f = ~.x[[7]][[1]][,3])) %>%  ## obtain silhouete plots
      tidyr::unnest(cols = c(data,.data$Level, .data$Silhouette_scores))
  }
  if(simplified == TRUE){
    clustered_data <-
      data %>%
      filter(.data$Abundance > 0, !is.na(.data$Abundance)) %>%
      group_by(.data$Sample, .add = TRUE) %>%
      tidyr::nest() %>%
      mutate(Level = purrr::map(.x = data,
                                .f = ~cluster::pam(.x$Abundance,
                                                   k = k,
                                                   cluster.only = TRUE,
                                                   diss = FALSE))) %>%
             tidyr::unnest(cols = c(data,.data$Level))
  }

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

  return(classified_clusters)
}
