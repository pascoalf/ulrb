#' Plot ulrb clustering results and silhouette scores
#'
#' Function to help access clustering results from ulrb.
#'
#' This function combined [plot_ulrb_clustering()] and [plot_ulrb_silhouette()].
#' The plots can be done for a single sample or for all samples.
#'
#' The results from the main function of ulrb package, [define_rb()], will include the classification of
#' each species (OTU, ASVs, etc) and the silhouette score obtained for each observation. Thus, to access the clustering results, there are two main plots to check:
#'  - the rank abundance curve obtained after ulrb classification;
#'  - and the silhouette plot.
#'
#' @details
#' **Interpretation of Silhouette plot**
#'
#' Based on chapter 2 of "Finding Groups in Data: An Introduction to Cluster Analysis."
#' (Kaufman and Rousseeuw, 1991); a possible (**subjective**) interpretation of the clustering structure based
#' on the Silhouette plot is:
#'
#' - 0.71-1.00 (A strong structure has been found);
#' - 0.51-0.70 (A reasonable structure has been found);
#' - 0.26-0.50 (The structure is weak and could be artificial);
#' - <0.26 (No structure has been found).
#'
#' @inheritParams plot_ulrb_clustering
#' @inheritParams plot_ulrb_silhouette
#' @param ... other arguments
#'
#' @return A grid of ggplot objects with clustering results and
#' silhouette plot obtained from [define_rb()].
#' @export
#'
#' @seealso [define_rb()], [check_avgSil()], [plot_ulrb_clustering()], [plot_ulrb_silhouette()]
#'
#' @examples
#' \donttest{
#' classified_species <- define_rb(nice_tidy)
#'
#' # Default parameters for a single sample ERR2044669
#' plot_ulrb(classified_species,
#'           sample_id = "ERR2044669",
#'           taxa_col = "OTU",
#'           abundance_col = "Abundance")
#'
#' # All samples in a dataset
#' plot_ulrb(classified_species,
#'           taxa_col = "OTU",
#'           abundance_col = "Abundance",
#'           plot_all = TRUE)
#'
#' # All samples with a log scale
#' plot_ulrb(classified_species,
#'           taxa_col = "OTU",
#'           abundance_col = "Abundance",
#'           plot_all = TRUE,
#'           log_scaled = TRUE)
#'}
#' @import dplyr
#' @importFrom rlang .data
#'
plot_ulrb <- function(data,
                      sample_id = NULL,
                      taxa_col,
                      plot_all = FALSE,
                      silhouette_score = "Silhouette_scores",
                      classification_col = "Classification",
                      abundance_col = "Abundance",
                      log_scaled = FALSE,
                      colors = c("#009E73", "#F0E442","#CC79A7"),
                      ...){
  #
  if(isFALSE(plot_all)){
    if(missing(sample_id)){
      stop("Are you trying to plot multiple samples? If so, please set plot_all to TRUE.")
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

  gridExtra::grid.arrange(
    plot_ulrb_clustering(data,
                         sample_id = sample_id,
                         taxa_col = taxa_col,
                         plot_all = plot_all,
                         classification_col = classification_col,
                         abundance_col = abundance_col,
                         log_scaled = log_scaled,
                         colors = colors,
                         ...),
    plot_ulrb_silhouette(data,
                         sample_id = sample_id,
                         taxa_col = taxa_col,
                         plot_all = plot_all,
                         classification_col = classification_col,
                         silhouette_score = silhouette_score,
                         colors = colors,
                         log_scaled = log_scaled,
                         ...),
    nrow = 1,
    ncol = 2
  )
}
