#' Plot Rank Abundance Curve of classification results
#'
#' Plots the clustering results from [define_rb()].
#'
#' This works as a sanity check of the results obtained by the unsupervised learning method used
#' to classify species. This is specially important if you used an automatic number of clusters.
#'
#' The function works for either a single sample (that you specify with sample_id argument), or
#' it can apply a centrality metric for species across all your samples (plot_all = TRUE).
#'
#' @param data a data.frame with, at least, the classification, abundance and sample information for each phylogenetic unit.
#' @param sample_id string with name of selected sample.
#' @param samples_col name of column with sample ID's.
#' @param taxa_col string with name of column with phylogenetic units. Usually OTU or ASV.
#' @param plot_all If TRUE, will make a plot for all samples with mean and standard deviation. If FALSE (default), then the plot will illustrate a single sample, that you have to specifiy in sample_id argument.
#' @param classification_col string with name of column with classification for each row. Default value is "Classification".
#' @param abundance_col string with name of column with abundance values. Default is "Abundance".
#' @param colors vector with colors. Should have the same lenght as the number of classifications.
#' @param log_scaled if TRUE then abundance scores will be shown in Log10 scale. Default to FALSE.
#' @param ... other arguments.
#'
#' @return A ggplot object with clustering results from [define_rb()].
#' @export
#'
#' @seealso [define_rb()], [plot_ulrb()], [plot_ulrb_silhouette()]
#'
#' @examples
#' classified_species <- define_rb(nice_tidy)
#'
#' # Standard plot for a single sample
#' plot_ulrb_clustering(classified_species,
#'                        sample_id = "ERR2044669",
#'                        taxa_col = "OTU",
#'                        abundance_col = "Abundance",
#'                        plot_all = FALSE)
#' # All samples in a dataset
#' plot_ulrb_clustering(classified_species,
#'           taxa_col = "OTU",
#'           abundance_col = "Abundance",
#'           plot_all = TRUE)
#'
#' # All samples with a log scale
#' plot_ulrb_clustering(classified_species,
#'           taxa_col = "OTU",
#'           abundance_col = "Abundance",
#'           plot_all = TRUE,
#'           log_scaled = TRUE)
#'
#' @import dplyr
#' @importFrom rlang .data
plot_ulrb_clustering <- function(data,
                                 sample_id = NULL,
                                 taxa_col,
                                 plot_all = TRUE,
                                 samples_col = "Sample",
                                 classification_col = "Classification",
                                 abundance_col = "Abundance",
                                 log_scaled = FALSE,
                                 colors = c("#009E73", "grey41","#CC79A7"),
                                 ...){
  #
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
  data <- data %>%
    rename(ID = all_of(taxa_col),
           Sample = all_of(samples_col),
           Classification = all_of(classification_col),
           Abundance = all_of(abundance_col))

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

    make_plot <- function(){
      data %>%
        filter(.data$Sample == sample_id) %>%
        mutate(Group = paste(.data$Sample, .data$Classification, sep = "_")) %>%
        ggplot2::ggplot(ggplot2::aes(x = reorder(.data$ID, -.data$Abundance),
                                     .data$Abundance, col = .data$Classification)) +
        ggplot2::geom_point() +
        ggplot2::geom_line(ggplot2::aes(group = .data$Group)) +
        ggplot2::theme(axis.text.x = ggplot2::element_blank(),
                       axis.text.y = ggplot2::element_text(size = 10),
                       axis.title = ggplot2::element_text(size = 12),
                       axis.ticks.x = ggplot2::element_blank(),
                       panel.grid = ggplot2::element_blank(),
                       axis.line.x.bottom = ggplot2::element_line(),
                       axis.line.y.left = ggplot2::element_line(),
                       panel.background = ggplot2::element_blank(),
                       legend.text = ggplot2::element_text(size = 12))+
        ggplot2::theme(legend.position = ifelse(n_classifications <= 3, "top", "right"))+
        ggplot2::scale_color_manual(values = colors)+
        ggplot2::labs(title = paste("Rank Abundance Curve for ", sample_id),
                      x = taxa_col, col = "", fill = "")
    }
  } else {
    if(n_classifications > 3){
      message("Classification label might not fit, consider changing the plot.")
    }
    make_plot <- function(){
      data %>%
        group_by(.data$Sample, .add = TRUE) %>%
        mutate(Group = paste(.data$Sample, .data$Classification, sep = "_")) %>%
        arrange(desc(.data$Abundance)) %>%
        mutate(uniqueRank = row_number()) %>%
        ungroup() %>%
        ggplot2::ggplot(ggplot2::aes(x = .data$uniqueRank, #reorder(.data$ID, -.data$Abundance),
                                     .data$Abundance, col = .data$Classification)) +
        ggplot2::geom_point() +
        ggplot2::geom_line(ggplot2::aes(group = .data$Group)) +
        #ggplot2::stat_summary(fun.data = ggplot2::mean_se)+
        ggplot2::theme(axis.text.x = ggplot2::element_blank(),
                       axis.text.y = ggplot2::element_text(size = 10),
                       axis.title = ggplot2::element_text(size = 12),
                       axis.ticks.x = ggplot2::element_blank(),
                       panel.grid = ggplot2::element_blank(),
                       axis.line.x.bottom = ggplot2::element_line(),
                       axis.line.y.left = ggplot2::element_line(),
                       panel.background = ggplot2::element_blank(),
                       legend.text = ggplot2::element_text(size = 12))+
        ggplot2::theme(legend.position = ifelse(n_classifications <= 3, "top", "right"))+
        ggplot2::scale_color_manual(values = colors)+
        ggplot2::labs(title = "Rank Abundance Curve for all samples",
                      #subtitle = paste("n = ", length(unique(data$Sample))),
                      x = taxa_col,
                      y = "Abundance",
                      col = "")
    }
  }

  if(isTRUE(log_scaled)){
    intermediate_plot <- make_plot()
    intermediate_plot +
      ggplot2::scale_y_log10()+
      ggplot2::labs(y ="Abundance in Log10 scale")
#                      ifelse(isTRUE(plot_all),
 #                            "Mean (\U00B1 sd)\n abundance in Log10 scale",
  #                           "Abundance in Log10 scale"))
  } else {
    make_plot()
  }
}
