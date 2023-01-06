#' Sanity check rare biosphere definition for one sample
#'
#' @param data a data.frame with, at least, the classification, abundance and sample information for each taxonomic unit.
#' @param sample_id string with name of selected sample.
#' @param taxa_id string with name of column with taxonomic units. Usually OTU or ASV.
#' @param classification_id string with name of column with classification for each row. Default value is "Classification".
#' @param abundance_id string with name of column with abundance values. Default is "Abundance".
#' @param colors vector with colors. Should have the same lenght as the number of classifications
#' @param ... other arguments
#'
#' @return a ggplot object
#' @export
#'
#' @examples 1
#'
#' @import dplyr
#' @importFrom rlang .data
sanity_check_sample_rb <- function(data,
                                   sample_id,
                                   taxa_id,
                                   classification_id = "Classification",
                                   abundance_id = "Abundance",
                                   colors = c("#0072B2", "#D55E00", "#CC79A7"), ...){
  #
  if(missing(sample_id)){
    stop("You must specify one sample from the column with samples ID's.")
  }
  if(missing(taxa_id)){
    stop("You must specify which column includes the taxonomic units.")
  }
  if(is.matrix(data))
    stop("Please use data.frame in tidy format.")
  if(length(colors) != length(unique(data$Classification))){
    stop("Number of colors must correspond to number of classifications used.")
  }

  # Make sure the taxa_id corresponds to the correct column
  data <- data %>%
    rename(ID = taxa_id,
           Classification = classification_id,
           Abundance = abundance_id)

  data %>%
    filter(.data$Sample == all_of(sample_id)) %>%
    ggplot2::ggplot(ggplot2::aes(x = .data$ID, .data$Abundance, col = .data$Classification)) +
    ggplot2::geom_point()+
    ggplot2::theme(axis.text.x = ggplot2::element_blank(),
                   axis.ticks.x = ggplot2::element_blank(),
                   panel.grid = ggplot2::element_blank(),
                   axis.line.x.bottom = ggplot2::element_line(),
                   axis.line.y.left = ggplot2::element_line(),
                   panel.background = ggplot2::element_blank())+
    ggplot2::scale_color_manual(values = colors)+
    ggplot2::labs(title = paste("Sample", sample_id))
}
