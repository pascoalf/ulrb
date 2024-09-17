#' Plot silhouette scores from clustering results
#'
#' Plots the Silhouette scores from the clustering results of [define_rb()].
#'
#' This works as a sanity check of the results obtained by the unsupervised learning method used
#' to classify taxa. This is specially important if you used an automatic number of clusters.
#'
#' The function works for either a single sample (that you specify with sample_id argument), or
#' it can apply a centrality metric for taxa across all your samples (plot_all = TRUE).
#'
#' For more details on Silhouette score, see [check_avgSil()] and [cluster::silhouette()].
#'
#' @details
#' **Interpretation of Silhouette plot**
#'
#' Based on chapter 2 of "Finding Groups in Data: An Introduction to Cluster Analysis."
#' (Kaufman and Rousseeuw, 1991); a possible interpretation of the clustering structure based
#' on the Silhouette plot is:
#'
#' - 0.71-1.00 (A strong structure has been found);
#' - 0.51-0.70 (A reasonable structure has been found);
#' - 0.26-0.50 (The structure is weak and could be artificial);
#' - < 0.26 (No structure has been found).
#'
#'
#' @inheritParams plot_ulrb_clustering
#' @param data ...
#' @param silhouette_score string with column name with silhouette score values. Default is "Silhouette_scores"
#'
#' @return A ggplot object of Silhouette plot obtained from the selected sample.
#' @export
#'
#' @seealso [define_rb()], [check_avgSil()], [plot_ulrb_clustering()],
#' [plot_ulrb()], [cluster::silhouette()], [cluster::pam()]
#'
#' @importFrom stats reorder
#' @examples
#'
#' classified_species <- define_rb(nice_tidy)
#'
#' # Standard plot for a single sample
#' plot_ulrb_silhouette(classified_species,
#'                        sample_id = "ERR2044669",
#'                        taxa_col = "OTU",
#'                        abundance_col = "Abundance",
#'                        plot_all = FALSE)
#' # All samples in a dataset
#' plot_ulrb_silhouette(classified_species,
#'           taxa_col = "OTU",
#'           abundance_col = "Abundance",
#'           plot_all = TRUE)
#'
#' # All samples with a log scale
#' plot_ulrb_silhouette(classified_species,
#'           taxa_col = "OTU",
#'           abundance_col = "Abundance",
#'           plot_all = TRUE,
#'           log_scaled = TRUE)
#'
plot_ulrb_silhouette <- function(data,
                                 sample_id = NULL,
                                 taxa_col,
                                 samples_col = "Sample",
                                 plot_all = TRUE,
                                 classification_col = "Classification",
                                 silhouette_score = "Silhouette_scores",
                                 colors = c("#009E73", "grey41","#CC79A7"),
                                 log_scaled = FALSE,
                                 ...){
  # Check data before starting
  if(isFALSE(plot_all)){
    if(missing(sample_id)){
      stop("Are you trying to plot multiple samples? If so, please set plot_all to TRUE.")
    }
  }
  if(!is.null(sample_id)){
    if(isTRUE(plot_all)){
      warning(paste("If you want to plot only", sample_id, "use plot_all = FALSE"))
    }
  }
  if(missing(taxa_col)){
    stop("Please specify the name of the column with phylohenetic units in the argument taxa_col.")
  }
  if(is.matrix(data))
    stop("Please use data.frame in tidy format.")
  if(length(colors) != length(unique(data$Classification))){
    stop("Number of colors must correspond to number of classifications used.")
  }
  if(!is.logical(log_scaled)){
    stop("'log_scaled' argument needs to be logical (TRUE/FALSE)")
  }
  # store number of classifications
  n_classifications <- length(unique(data$Classification))
  #
  # Prepare data
  data <- data %>%
    rename(ID = all_of(taxa_col),
           Sample = all_of(samples_col),
           Silhouette_scores = all_of(silhouette_score),
           Classification = all_of(classification_col))

  # Conditions for plots
  if(!isTRUE(plot_all)){
    # For one sample, the sample_id must be specified
    if(missing(sample_id)){stop("You must specify one sample from the column with samples ID's.")}
    # The sample id must be present in the data
    if(
      data %>%
      filter(.data$Sample == sample_id) %>%
      pull(.data$Sample) %>%
      length() == 0
    ){stop("Sample ID must be present in Sample column. Verify sample_id and samples_col arguments.")}

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
                     axis.text.x = ggplot2::element_text(size = 10),
                     axis.title = ggplot2::element_text(size = 12),
                     axis.ticks.y = ggplot2::element_blank(),
                     panel.grid = ggplot2::element_blank(),
                     axis.line.x.bottom = ggplot2::element_line(),
                     axis.line.y.left = ggplot2::element_line(),
                     panel.background = ggplot2::element_blank(),
                     legend.text = ggplot2::element_text(size = 12)) +
      ggplot2::theme(legend.position = ifelse(n_classifications <= 3, "top", "right"))+
      ggplot2::scale_color_manual(values = colors)+
      ggplot2::scale_fill_manual(values = colors)+
      ggplot2::labs(title = paste("Silhouette plot for", sample_id),
                    y = "Silhouette scores",
                    x = taxa_col, col = "", fill = "")
  } else {
    if(n_classifications > 3){
      message("Classification label might not fit, consider changing the plot.")
    }
    data %>%
      group_by(.data$Sample, .add = TRUE) %>%
      mutate(Group = paste(.data$Sample, .data$Classification, sep = "_")) %>%
      arrange(desc(.data$Silhouette_scores)) %>%
      mutate(uniqueRank = row_number()) %>%
      ungroup() %>%
      ggplot2::ggplot(ggplot2::aes(x = .data$uniqueRank, #reorder(.data$ID, -.data$Silhouette_scores),
                                   .data$Silhouette_scores,
                                   fill = .data$Classification,
                                   col = .data$Classification)) +
      ggplot2::geom_point() +
      ggplot2::geom_line(ggplot2::aes(group = .data$Group)) +
      #ggplot2::stat_summary(fun.data = ggplot2::mean_se)+
      ggplot2::geom_hline(yintercept = c(0),
                          colour = c("black"),
                          linetype = "dashed")+
      ggplot2::theme(axis.text.x = ggplot2::element_blank(),
                     axis.text.y = ggplot2::element_text(size = 10),
                     axis.title = ggplot2::element_text(size = 12),
                     axis.ticks.x = ggplot2::element_blank(),
                     panel.grid = ggplot2::element_blank(),
                     axis.line.x.bottom = ggplot2::element_line(),
                     axis.line.y.left = ggplot2::element_line(),
                     panel.background = ggplot2::element_blank(),
                     legend.text = ggplot2::element_text(size = 12)) +
      ggplot2::theme(legend.position = ifelse(n_classifications <= 3, "top", "right"))+
      ggplot2::scale_color_manual(values = colors)+
      ggplot2::scale_fill_manual(values = colors)+
      ggplot2::labs(title = paste("Silhouette plot for all samples"),
                    #subtitle = paste("n = ", length(unique(data$Sample))),
                    y = "Silhouette scores",
                    x = taxa_col,
                    col = "", fill = "")
  }

}
