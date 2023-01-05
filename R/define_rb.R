#' Define Rare Biosphere
#'
#' @param data A tibble with, at least, a column for Abundance and Sample. Additional columns are allowed.
#' @param classification_vector A vector of strings with the names for each cluster, from lower to higher abundance. Default is c("Rare", "Undetermined", "Abundance")
#' @param samples_id String with name of column with sample names
#' @param abundance_id String with name of column with abundance values
#'
#' @return A tibble with the initial columns and new columns for the cluster and classification of each species.
#' @export
#'
#' @examples 1
#'
#' @import dplyr
#' @importFrom rlang .data
define_rb <- function(data,
                      classification_vector = c("Rare","Undetermined","Abundant"),
                      samples_id = "Sample",
                      abundance_id = "Abundance") {

  # Define number of cluster based on possible classifications
  k <- length(classification_vector)

  # Match samples_id and abundance_id with Samples and Abundance, respectively
  data <-
    data %>%
    rename(Sample = all_of(samples_id),
           Abundance = all_of(abundance_id))

  # Calculate kmedoids
  clustered_data <-
    data %>%
    filter(.data$Abundance > 0, !is.na(.data$Abundance)) %>%
    group_by(.data$Sample) %>%
    tidyr::nest() %>%
    mutate(Level = purrr::map(.x = data,
                       .f = ~cluster::pam(.x$Abundance,
                                 k = k,
                                 cluster.only = TRUE,
                                 diss = FALSE))) %>%
    tidyr::unnest(cols = c(data,.data$Level))

  # Make classification table
  classification_table <- clustered_data %>%
    group_by(.data$Sample,Level = as.factor(.data$Level)) %>%
    summarize(Cluster_median_abundance = stats::median(.data$Abundance)) %>%
    arrange(.data$Sample, .data$Cluster_median_abundance) %>%
    mutate(Classification = factor(all_of(classification_vector),
                                   levels = all_of(classification_vector)))

  # Apply classification_table to classify clusters
  classified_clusters <- clustered_data %>%
    mutate(Level = as.factor(.data$Level)) %>%
    left_join(classification_table)

  return(classified_clusters)
}
