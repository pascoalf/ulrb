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
#' @import tidyr
#' @import cluster
#' @import purrr
define_rb <- function(data,
                      classification_vector = c("Rare","Undetermined","Abundant"),
                      samples_id = "Sample",
                      abundance_id = "Abundance") {

}
