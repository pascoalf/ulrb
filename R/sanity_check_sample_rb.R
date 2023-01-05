#' Sanity check rare biosphere definition for one sample
#'
#' @param data a data.frame with, at least, the classification, abundance and sample information for each taxonomic unit.
#' @param sample string with name of selected sample.
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
sanity_check_rb_sample <- function(data,
                                   sample,
                                   taxa_id, ## go back to other functions, make this mandatory
                                   classification_id = "Classification", # in case the user changes something
                                   abundance_id = "Abundance",
                                   colors = c("#0072B2", "#D55E00", "#CC79A7"), ...){ # in case the user changes something

  # Make sure the taxa_id corresponds to the correct column
  data <- data %>%
    rename(ID = taxa_id)

  #
  if(length(colors) != length(unique(data$Classification))){
    message("Number of colors must correspond to number of classifications used.")
    }

  data %>%
    filter(.data$Sample == all_of(sample)) %>%
    ggplot2::ggplot(ggplot2::aes(x = .data$ID, .data$Abundance, col = .data$Classification)) +
    ggplot2::geom_point()+
    ggplot2::theme(axis.text.x = ggplot2::element_blank(),
                   axis.ticks.x = ggplot2::element_blank(),
                   panel.grid = ggplot2::element_blank(),
                   axis.line.x.bottom = ggplot2::element_line(),
                   axis.line.y.left = ggplot2::element_line(),
                   panel.background = ggplot2::element_blank())+
    ggplot2::scale_color_manual(values = colors)+
    ggplot2::labs(title = paste("Sample", sample))
}
