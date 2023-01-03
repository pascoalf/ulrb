#' Title
#'
#' @param data A tibble with, at least, a column for Abundance and Sample. Additional columns are allowed.
#' @param classification_vector A vector of strings with the names for each cluster, from lower to higher abundance. Default is c("Rare", "Undetermined", "Abundance")
#' @param samples_id String with name of column with sample names
#' @param abundance_id String with name of column with abundance values
#'
#' @return The input tibble with additional columns for the cluster and classification.
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
    filter(.data$Abundance > 0, !is.na(Abundance)) %>%
    group_by(.data$Sample) %>%
    tidyr::nest() %>%
    mutate(Level = purrr::map(.x = data,
                       .f = ~cluster::pam(.x$Abundance,
                                 k = k,
                                 cluster.only = TRUE,
                                 diss = FALSE))) %>%
    tidyr::unnest(cols = c(data,Level))

  return(clustered_data)
}
