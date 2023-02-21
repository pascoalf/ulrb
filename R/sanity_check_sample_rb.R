#' Sanity check rare biosphere definition for one sample
#'
#' @param data a data.frame with, at least, the classification, abundance and sample information for each taxonomic unit.
#' @param sample_id string with name of selected sample.
#' @param taxa_id string with name of column with taxonomic units. Usually OTU or ASV.
#' @param classification_id string with name of column with classification for each row. Default value is "Classification".
#' @param abundance_id string with name of column with abundance values. Default is "Abundance".
#' @param colors vector with colors. Should have the same lenght as the number of classifications
#' @param log_scaled if TRUE then abundance scores will be shown in Log10 scale. Default to FALSE.
#' @param ... other arguments
#'
#' @return a ggplot object
#' @export
#'
#' @examples
#' classified_species <- define_rb(nice_tidy)
#'
#' sanity_check_sample_rb(classified_species,
#'                        sample_id = "ERR2044669",
#'                        taxa_id = "OTU",
#'                        abundance_id = "Abundance")
#'
#' @import dplyr
#' @importFrom rlang .data
sanity_check_sample_rb <- function(data,
                                   sample_id,
                                   taxa_id,
                                   classification_id = "Classification",
                                   abundance_id = "Abundance",
                                   colors = c("#0072B2", "#D55E00", "#CC79A7"),
                                   log_scaled = FALSE,
                                   ...){
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
  if(!is.logical(log_scaled)){
    stop("'log_scaled' argument needs to be logical (TRUE/FALSE)")
  }

  # Make sure the taxa_id corresponds to the correct column
  data <- data %>%
    rename(ID = taxa_id,
           Classification = classification_id,
           Abundance = abundance_id)

  make_plot <- function(){
    data %>%
      filter(.data$Sample == all_of(sample_id)) %>%
      ggplot2::ggplot(ggplot2::aes(x = reorder(.data$ID, -.data$Abundance),
                                   .data$Abundance, col = .data$Classification)) +
      ggplot2::geom_point()+
      ggplot2::theme(axis.text.x = ggplot2::element_blank(),
                     axis.ticks.x = ggplot2::element_blank(),
                     panel.grid = ggplot2::element_blank(),
                     axis.line.x.bottom = ggplot2::element_line(),
                     axis.line.y.left = ggplot2::element_line(),
                     panel.background = ggplot2::element_blank(),
                     legend.position = "top")+
      ggplot2::scale_color_manual(values = colors)+
      ggplot2::labs(title = paste("Rank Abundance Curve for ", sample_id),
                    x = taxa_id)
  }

  if(isTRUE(log_scaled)){
    intermediate_plot <- make_plot()
    intermediate_plot +
      ggplot2::scale_y_log10()+
      ggplot2::labs(y = "Abundance in Log10 scale")
  } else {
    make_plot()
  }
}
