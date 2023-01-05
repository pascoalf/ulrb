#' Sanity check rare biosphere definition for one sample
#'
#' @param data
#' @param species_id
#' @param samples_id
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#'
#' @import dplyr
#' @import ggplot2
#' @importFrom rlang .data
sanity_check_rb_sample <- function(data, sample, taxa_id,...){

  # Make sure the taxa_id corresponds to the correct column
  data <- data %>%
    rename(ID = taxa_id)

  #
  data %>%
    filter(Sample == all_of(sample)) %>%
    ggplot(aes(x = .data$ID, "Abundance", col = "Classification")) +
    geom_point()+
    theme(axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          panel.grid = element_blank(),
          axis.line.x.bottom = element_line(),
          axis.line.y.left = element_line(),
          panel.background = element_blank())+
    scale_color_manual(values = c("#0072B2", "#D55E00", "#CC79A7"))+
    labs(title = paste("Sample", sample))
}
