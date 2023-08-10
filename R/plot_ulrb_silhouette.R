#' Sanity check of Silhouette plot of selected sample
#'
#' @inheritParams plot_ulrb_clustering
#' @param data ...
#' @param silhouette_score string with column name with silhouette score values. Default is "Silhouette_scores"
#'
#' @return A ggplot object of Silhouette plot obtained from the selected sample.
#' @export
#'
#' @importFrom stats reorder
#' @examples
#' plot_ulrb_silhouette(data = define_rb(nice_tidy, simplified = FALSE),
#'  taxa_col = "OTU", sample_id = "ERR2044662")
#'
plot_ulrb_silhouette <- function(data,
                                 sample_id = NULL,
                                 taxa_col,
                                 plot_all = FALSE,
                                 classification_col = "Classification",
                                 silhouette_score = "Silhouette_scores",
                                 colors = c("#009E73", "#F0E442","#CC79A7"),
                                 log_scaled = FALSE,
                                 ...){
  # Check data before starting
  if(missing(taxa_col)){stop("You must specify which column includes the taxonomic units.")}
  if(is.matrix(data))
    stop("Please use data.frame in tidy format.")
  if(length(colors) != length(unique(data$Classification))){
    stop("Number of colors must correspond to number of classifications used.")
  }
  if(!is.logical(log_scaled)){
    stop("'log_scaled' argument needs to be logical (TRUE/FALSE)")
  }

  # Prepare data
  data <- data %>%
    rename(ID = all_of(taxa_col),
           Silhouette_scores = all_of(silhouette_score),
           Classification = all_of(classification_col))

  # Conditions for plots
  if(!isTRUE(plot_all)){
    data %>%
      filter(.data$Sample == sample_id) %>%
      ggplot2::ggplot(ggplot2::aes(x = reorder(.data$ID, .data$Silhouette_scores),
                                   .data$Silhouette_scores,
                                   fill = .data$Classification,
                                   col = .data$Classification)) +
      ggplot2::geom_col()+
      ggplot2::geom_hline(yintercept = c(-0.25, 0, 0.25, 0.5, 0.75),
                          colour = c("red", "red", "black", "black", "black"),
                          linetype = "dashed")+
      ggplot2::coord_flip()+
      ggplot2::theme(axis.text.y = ggplot2::element_blank(),
                     axis.ticks.y = ggplot2::element_blank(),
                     panel.grid = ggplot2::element_blank(),
                     axis.line.x.bottom = ggplot2::element_line(),
                     axis.line.y.left = ggplot2::element_line(),
                     panel.background = ggplot2::element_blank(),
                     legend.position = "top")+
      ggplot2::scale_color_manual(values = colors)+
      ggplot2::scale_fill_manual(values = colors)+
      ggplot2::labs(title = paste("Silhouette plot for", sample_id),
                    y = "Silhouette scores",
                    x = taxa_col)
  } else {
    data %>%
      ggplot2::ggplot(ggplot2::aes(x = reorder(.data$ID, -.data$Silhouette_scores),
                                   .data$Silhouette_scores,
                                   fill = .data$Classification,
                                   col = .data$Classification)) +
      ggplot2::stat_summary()+
      ggplot2::geom_hline(yintercept = c(0),
                          colour = c("black"),
                          linetype = "dashed")+
      ggplot2::theme(axis.text.x = ggplot2::element_blank(),
                     axis.ticks.x = ggplot2::element_blank(),
                     panel.grid = ggplot2::element_blank(),
                     axis.line.x.bottom = ggplot2::element_line(),
                     axis.line.y.left = ggplot2::element_line(),
                     panel.background = ggplot2::element_blank(),
                     legend.position = "top")+
      ggplot2::scale_color_manual(values = colors)+
      ggplot2::scale_fill_manual(values = colors)+
      ggplot2::labs(title = paste("Silhouette plot for all samples"),
                    y = "Mean (\U00B1 sd) Silhouette scores",
                    x = taxa_col)
  }

}
