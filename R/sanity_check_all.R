#' Sanity check of results for all samples
#'
#' @inheritParams sanity_check_sample_rb
#' @inheritParams sanity_check_sample_sil
#' @inheritParams prepare_tidy_data
#' @param data a tidy data.frame with samples, abundance and classification for each taxonomic unit.
#' @param export_output can be "yes" (default) or "no". If "yes", then a pdf file with all plots is produced,
#'  else the plots are not printed into pdf and a are returned to R session instead.
#' @param output_name name of pdf file with ouput. Only works if export_output = "yes".
#' @param ... other arguments
#'
#' @return 1
#' @export
#'
#' @examples
#' #
#' sample_names <- c("ERR2044662", "ERR2044663", "ERR2044664",
#'                    "ERR2044665", "ERR2044666", "ERR2044667",
#'                    "ERR2044668", "ERR2044669", "ERR2044670")
#' #
#' classified_species <- define_rb(nice_tidy)
#' #
#' sanity_check_all(classified_species,
#'                  sample_names = sample_names,
#'                  taxa_id = "OTU",
#'                  abundance_id = "Abundance",
#'                  export_output = "no",
#'                  classification_id = "Classification")
#'
#' @importFrom grDevices dev.off pdf
sanity_check_all <- function(data,
                             sample_names,
                             taxa_id,
                             classification_id = "Classification",
                             abundance_id = "Abundance",
                             silhouette_score = "Silhouette_scores",
                             colors = c("#0072B2", "#D55E00", "#CC79A7"),
                             export_output = FALSE,
                             output_name = "Sanity check figures",
                             log_scaled = FALSE,
                             ...){
    #
    plot_list <- lapply(all_of(sample_names), function(x){
      gridExtra::grid.arrange(
        sanity_check_sample_rb(data = data,
                               sample_id = x,
                               taxa_id = taxa_id,
                               classification_id = classification_id,
                               abundance_id = abundance_id,
                               colors = colors,
                               log_scaled = log_scaled),
        sanity_check_sample_sil(data = data,
                                sample_id = x,
                                taxa_id = taxa_id,
                                classification_id = classification_id,
                                silhouette_score = silhouette_score,
                                colors = colors),
        ncol= 2
      )

      })
    #
    if(!isTRUE(export_output)){
      return(print(plot_list))
    } else {
        pdf(paste0(output_name, ".pdf"))
          lapply(plot_list, print)
        dev.off()

    }

}
