#' Prepare data in tidy format
#'
#' @param data a data.frame in "wide" format, with samples in either columns or rows. This data.frame should not include any data besides abundance values per sample, per taxonomic unit. Additional data (e.g. taxonomy details) should be added afterwards.
#' @param sample_names a vector with the name of all samples.
#' @param samples_in a vector specifyng the location of the samples. It can either be "cols" (default) if samples are in columns, or "rows" if samples are in rows.
#' @param ... additional arguments
#'
#' @return a tibble with with the arguments for define_rb() in tidy format.
#' @export
#'
#' @examples 1
#'
#' @import dplyr
#' @importFrom rlang .data
#'
prepare_tidy_data <- function(data,
                              sample_names,
                              samples_in = "cols",
                              ...){
  # Samples are in cols
  if(samples_in == "cols"){
    tidy_data <- tidyr::pivot_longer(data,
                        cols = all_of(sample_names),
                        names_to = "Sample", values_to = "Abundance")
    }

  # Samples are in rows
  if(samples_in == "rows"){
    data <- data %>% t() %>% as.data.frame()
    #
    colnames(data) <- sample_names ### Problem if there is extra data
    #
    tidy_data <- tidyr::pivot_longer(data,
                        cols = all_of(sample_names),
                        names_to = "Sample",
                        values_to = "Abundance")
  }
  return(tidy_data)
}
