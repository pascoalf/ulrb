#' Prepare data in tidy format
#'
#' Function to transforms common abundance table formats into a "long" format.
#'
#' This function guarantees that the abundance table includes one column with sample ID's and
#' one column with abundance.
#'
#' @details
#' **Common species table formats**
#'
#' There are two common formats for abundance tables:
#'  - samples as rows and taxa as columns;
#'  - taxa as rows and samples as columns.
#'
#' However, both formats are not tidy/long, because they include several columns with the same variable. They
#' are in a "wide format" instead of a "long format".
#'
#' This function re-organizes samples and taxa so that there is a single column with the samples ID's and
#' another with the abundance scores. Extra columns are allowed.
#'
#' @param data a data.frame in "wide" format, with samples in either columns or rows. This data.frame should not include any data besides abundance values per sample, per taxonomic unit. Additional data (e.g. taxonomy details) should be added afterwards.
#' @param sample_names a vector with the name of all samples.
#' @param samples_in a vector specifying the location of the samples. It can either be "cols" (default) if samples are in columns, or "rows" if samples are in rows.
#' @param ... additional arguments
#'
#' @return An abundance table in long format, compatible with dplyr pipes and **ulrb** package functions.
#' @export
#'
#' @seealso [define_rb()]
#'
#' @examples
#' library(dplyr)
#' #
#' sample_names <- c("ERR2044662", "ERR2044663", "ERR2044664",
#'                    "ERR2044665", "ERR2044666", "ERR2044667",
#'                    "ERR2044668", "ERR2044669", "ERR2044670")
#'
#' # Example for samples in cols and with additional data available
#' prepare_tidy_data(nice, sample_names = sample_names, samples_in = "cols")
#'
#' # Example for samples in rows
#' # Select columns with samples from nice
#' nice_rows <- nice %>% select(all_of(sample_names))
#'
#' # Change columns to rows
#' nice_rows <- nice_rows %>% t() %>% as.data.frame()
#'
#' # Turn colnames into phylogenetic units ID
#' colnames(nice_rows) <- paste0("OTU_", seq_along(colnames(nice_rows)))
#'
#' prepare_tidy_data(nice_rows, sample_names = sample_names, samples_in = "rows")
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
  if(samples_in == "rows"){

    # capture phylogenetic units ID
    taxonomic_units <- colnames(data)

    if(nrow(data) != length(sample_names)){
      stop("For samples_in = `rows`, the length of sample_names must equal the number of rows.")
    }
    # sample names are in rownames
    if(mean(rownames(data) == sample_names) == 1){
      #
      data <- data %>% t() %>% as.data.frame()
      #
      tidy_data <- tidyr::pivot_longer(data,
                                       cols = all_of(sample_names),
                                       names_to = "Sample",
                                       values_to = "Abundance")

    } else {
      # if the user doesn't include the sample names in rownames, then we assume that the order of samples in rows and in the sample vector is the same
      warning("Please check if samples in sample_names vector and rownames of data are in the same order.")
      data <- data %>% t() %>% as.data.frame()
      #
      colnames(data) <- sample_names
      #
      tidy_data <- tidyr::pivot_longer(data,
                                       cols = all_of(sample_names),
                                       names_to = "Sample",
                                       values_to = "Abundance")

  }
    # Add phylogenetic units column
    tidy_data <-
      tidy_data %>%
      group_by(.data$Sample) %>%
      mutate(Taxa_id = taxonomic_units)
    #
    message("Taxa_id assumes each column is a taxonomic unit.")
  }
  return(tidy_data)
}
