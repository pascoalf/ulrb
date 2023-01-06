#' Sanity check of results for all samples
#'
#' @inheritParams sanity_check_sample_rb
#' @inheritParams prepare_tidy_data
#' @param data a tidy data.frame with samples, abundance and classification for each taxonomic unit.
#' @param export_output can be "yes" (default) or "no". If "yes", then a pdf file with all plots is produced,
#'  else the plots are not printed into pdf and a list is returned instead.
#' @param output_name name of pdf file with ouput. Only works if export_output = "yes".
#' @param ... other arguments
#'
#' @return 1
#' @export
#'
#' @examples 1
sanity_check_all <- function(data,
                             sample_names,
                             taxa_id,
                             classification_id = "Classification",
                             abundance_id = "Abundance",
                             colors = c("#0072B2", "#D55E00", "#CC79A7"),
                             export_output = "yes",
                             output_name = "Sanity check figures",
                             ...){
    #
    plot_list <- lapply(sample_names, function(x){
      sanity_check_sample_rb(data = data,
                             sample_id = x,
                             taxa_id = taxa_id,
                             classification_id = classification_id,
                             abundance_id = abundance_id,
                             colors = colors)})
    #
    if(export_output == "yes"){
      pdf(output_name)
      lapply(plot_list, print)
      dev.off()
    } else {
      return(plot_list)
    }

}
