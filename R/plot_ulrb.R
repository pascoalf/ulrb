#' Plot relevant metrics obtained from ulrb clustering
#'
#' @inheritParams plot_ulrb_clustering
#' @inheritParams plot_ulrb_silhouette
#' @param ... other arguments
#'
#' @return a ggplot object
#' @export
#'
#' @examples
#' classified_species <- define_rb(nice_tidy)
#'
#' plot_ulrb(classified_species,
#'                        sample_id = "ERR2044669",
#'                        taxa_col = "OTU",
#'                        abundance_col = "Abundance")
#'
#' @import dplyr
#' @importFrom rlang .data
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

  if(missing(taxa_col)){
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
