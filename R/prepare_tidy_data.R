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

  if(missing(sample_names)){
    message("Missing argument sample_names. This is a vector with the names of the samples, as in the data input")
  }

  # Samples are in cols
  if(samples_in == "cols"){

    tidy_data <- tidyr::pivot_longer(data,
                        cols = all_of(sample_names),
                        names_to = "Sample",
                        values_to = "Abundance")
    }

  # Samples are in rows

  ### Still dont have data to test this part!!!
  if(samples_in == "rows"){

    if(nrow(data) != length(sample_names)){
      stop("for samples_in = `rows`, each row must correspond to a specific sample")
    }

    # If the user provides the samples in the rownames, better automatically set them
    if(rownames(data) == sample_names){
      data <- data %>% t() %>% as.data.frame()
      #
      tidy_data <- tidyr::pivot_longer(data,
                                       cols = all_of(sample_names),
                                       names_to = "Sample",
                                       values_to = "Abundance")

    } else {
      # if the user doesn't include the sample names in rownames, then we assume that the order of samples in rows and in the sample vector is the same
      warning("please check if samples in sample_names vector and rownames of data are in the same order") ## this is not ideal}
      data <- data %>% t() %>% as.data.frame()
      #
      colnames(data) <- sample_names
      #
      tidy_data <- tidyr::pivot_longer(data,
                                       cols = all_of(sample_names),
                                       names_to = "Sample",
                                       values_to = "Abundance")
  }
  return(tidy_data)
}
