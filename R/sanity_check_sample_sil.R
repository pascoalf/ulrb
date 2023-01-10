#' Sanity check of Silhouette plot of selected sample
#'
#' @inheritParams sanity_check_sample_rb
#' @param data ...
#' @param silhouette_score string with collumn name with silhouette score values. Default is "Silhouette_scores"
#'
#' @return A ggplot object of Silhouette plot obtained from the selected sample.
#' @export
#'
#' @importFrom stats reorder
#' @examples
#' sanity_check_sample_sil(data = define_rb(nice_tidy, simplified = FALSE),
#'  taxa_id = "OTU", sample_id = "ERR2044662")
#'
sanity_check_sample_sil <- function(data,
                                    sample_id,
                                    taxa_id,
                                    classification_id = "Classification",
                                    silhouette_score = "Silhouette_scores",
                                    colors = c("#0072B2", "#D55E00", "#CC79A7"),
                                    ...){
  data <- data %>%
    rename(ID = taxa_id,
           Silhouette_scores = silhouette_score,
           Classification = classification_id)

  data %>%
    filter(.data$Sample == all_of(sample_id)) %>%
    ggplot2::ggplot(ggplot2::aes(x = reorder(.data$ID, .data$Silhouette_scores),
                                 .data$Silhouette_scores,
                                 fill = .data$Classification,
                                 col = .data$Classification)) +
    ggplot2::geom_col()+
    ggplot2::coord_flip()+
    ggplot2::theme(axis.text.x = ggplot2::element_blank(),
                   axis.ticks.x = ggplot2::element_blank(),
                   panel.grid = ggplot2::element_blank(),
                   axis.line.x.bottom = ggplot2::element_line(),
                   axis.line.y.left = ggplot2::element_line(),
                   panel.background = ggplot2::element_blank())+
    ggplot2::scale_color_manual(values = colors)+
    ggplot2::scale_fill_manual(values = colors)
  #  ggplot2::labs(title = paste("Sample", sample_id))

}
