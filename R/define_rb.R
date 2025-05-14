#' Define Rare Biosphere
#'
#' Classifies taxa in each sample into either "rare", "undetermined" or "abundant". Other classifications are allowed.
#'
#' @details
#' **Overview**
#'
#' Function to cluster taxa abundance with partition around medoids algorithm (Kaufman and Rousseuw. 1991).
#' By default, we propose the division into three clusters (k = 3),
#' which can be translated into convenient classifications: "rare", "undetermined" and "abundant".
#' Taxa from the cluster with lowest median abundance corresponds to the "rare biosphere".
#'
#' @details
#' **The classification vector**
#'
#' The classification vector (argument classification_vector) represents the
#' different clusters to be used, by ascending order of median abundance.
#' To change the number of clusters, change the number of elements in the
#' classification vector, **but order matters!** Depending on the number of clusters used,
#' you can change the meaning that best applies to your research.
#'
#' For example, you can use a classification vector with the designations: "very rare", "rare",
#' "abundant" and "very abundant"; which would apply a k = 4 underneath.
#' It is possible to use any number of clusters, as long as they are within 2 and
#' the maximum possible k.
#'
#' The maximum possible k is the number of different abundance scores observed in a single sample. Note, however,
#'  that we do not recommend any clustering for k > 10
#' and we also don't recommend k = 2 (we explain in more detail in Pascoal et al., 2025;
#' and in the vignette `vignette("explore-classifications")`.
#'
#' @details
#' **Automatic selection of the number of clusters**
#'
#' To automatically decide the number of clusters (i.e., the value of k), it is possible to do so with the argument **automatic=TRUE**. For details on
#' complete automation of [define_rb()], please see the documentation for [suggest_k()]. Briefly, the k with best average Silhouette score
#' is selected from a range of k values between 2 and 10. It is possible to decide k based on other indices ("Davies-Bouldin" or "Calinsky-Harabasz").
#'
#' If you want a more fine grained analysis of k values, we provide several functions:
#'  - [evaluate_k()];
#'  - [check_avgSil()];
#'  - [check_DB()];
#'  - [check_CH()].
#'
#' @details
#' **Verify clustering results**
#'
#' If half of the taxa of any cluster got a Silhouette score below 0.5 in any sample, then a warning is provided.
#' The warning provides the number of times this issue occurred.
#' You can inspect other alternatives to reduce the occurrences of a bad clustering,
#' but it is possible that, in some situations, you just can't find an optimal clustering.
#'
#' The detailed output gives you access to all of the clustering results:
#'  - `pam_object` is a list with the original results from the [cluster::pam()],
#'  see [cluster::pam()] documentation for more details.
#'  - `Level` is an integer indicating the specific cluster attributed by the [cluster::pam()]
#'  function for each observation. Its order is random.
#'  - `Silhouette_scores` provides the Silhouette score obtained for each
#'  observation, i.e. a score for each taxa.
#'  - `Cluster_median_abundance` provides the median taxa abundance of each cluster.
#'  - `median_Silhouette` provides the median Silhouette score obtained for each cluster.
#'  - `Evaluation` indicates if the silhouette score obtained for a given observation
#'   is below the median Silhouette of its cluster and sample.
#'
#' You can make your own plots and analysis, but we also provide another function,
#'  [plot_ulrb()], which illustrates the results obtained.
#'
#' @details
#' **Partition around medoids (pam)**
#'
#' To calculate k-medoids, we used the partition around medoids (pam)
#' algorithm, which was described in Chapter 2 of "Finding Groups in Data: An Introduction to Cluster Analysis."
#' (Kaufman and Rousseeuw, 1991) and implemented by the cluster package with the [cluster::pam()] function.
#'
#' Briefly, the pam algorithm is divided into two main phases: **build** and **swap**.
#'
#' The first phase (**build**) selects k observations as cluster representatives. The first
#' observation selected as representative is the one that minimizes the sum of the dissimilarities to the
#' remaining observations. The second, third and so on repeat the same process, until k clusters have
#' been formed.
#'
#' The **build** steps are:
#'
#' 1 - Propose a centroid with observation, \eqn{i}, which has not been selected as a centroid yet
#'
#' 2 - Calculate the distance between another observation, \eqn{j}, and its most similar
#' observation, \eqn{D_j}; and calculate the difference with the proposed centroid,
#'  \eqn{i}, i.e., \eqn{d(j,i)}
#'
#' 3 - If \eqn{d(j,i) > 0}, then calculate its contribution to the centroid:
#' \deqn{max(D_j - d(j,i),0)}
#'
#' 4 - Calculate the total gain obtained by \eqn{i}, \deqn{\sum_{j}C_{ji}}
#'
#' 5 - From all possible centroids, select the one that maximizes the previous total gain obtained,
#' \deqn{max_i \sum_jC_{ji}}
#'
#' 6 - Repeat until k observations have been selected as cluster representatives.
#'
#' The purpose of the next phase, **swap**, is to improve the representatives
#' for the clusters. The principle is to swap the cluster representative between
#' all possibilities and calculate the value sum of dissimilarities between each
#' observation and the closest centroid. The swapping continues until no more improvement is
#' possible, i.e., when the minimum sum of dissimilarities of the clusters is reached.
#'
#'
#' @details
#' **Notes**:
#'
#' Understand that **ulrb** package considers each sample as an independent
#' community of taxa, which means clustering is also independent across
#' different samples.
#' Thus, be aware that you will have clustering results and metrics for each
#' single sample, which is why we also provide some functions to analyze results across
#' any number of samples (see: [plot_ulrb()] for clustering results
#' and [evaluate_k()] for k selection).
#'
#'
#' @param data A data.frame with, at least, a column for Abundance and Sample. Additional columns are allowed.
#' @param classification_vector A vector of strings with the names for each cluster, from lower to higher abundance. Default is c("Rare", "Undetermined", "Abundance").
#' @param samples_col String with name of column with sample names.
#' @param abundance_col String with name of column with abundance values.
#' @param simplified Can be TRUE/FALSE. Default (FALSE) provides an additional column with detailed [cluster::pam()] results
#'  and Silhouette scores. If TRUE, only the Classification result is added to the original input data.
#' @param automatic By default (FALSE), will assume a classification into "Rare", "Undetermined" or "Abundant". If TRUE, then it will automatically select the number of classifications (or k),
#' based on the index argument.
#' @param check_singles Default if FALSE. If TRUE, the user is warned of the number of clusters represented by a single taxon,
#' if any.
#' @inheritParams suggest_k
#'
#' @returns The input data.frame with extra columns containing the classification and additional metrics (if detailed = TRUE).
#' @seealso [suggest_k()], [evaluate_k()], [plot_ulrb()], [cluster::pam()]
#' @export
#'
#' @references
#' Kaufman, L., & Rousseuw, P. J. (1991). Chapter 2 in book Finding Groups in Data: An Introduction to Cluster Analysis. Biometrics, 47(2), 788.
#' Pascoal, F., Branco, P., Torgo, L. et al. Definition of the microbial rare biosphere through unsupervised machine learning. Commun Biol 8, 544 (2025). https://doi.org/10.1038/s42003-025-07912-4
#'
#' @examples
#' \donttest{
#' library(dplyr)
#' # Sample ID's
#' sample_names <- c("ERR2044662", "ERR2044663", "ERR2044664",
#'                    "ERR2044665", "ERR2044666", "ERR2044667",
#'                    "ERR2044668", "ERR2044669", "ERR2044670")
#'
#' # If data is in wide format, with samples in cols
#' nice_tidy <- prepare_tidy_data(nice,
#'                                sample_names = sample_names,
#'                                samples_in = "cols")
#'
#' # Straightforward with tidy format
#' define_rb(nice_tidy)
#'
#' #Closer look
#' classified_table <- define_rb(nice_tidy)
#' classified_table %>%
#' select(Sample, Abundance, Classification) %>%
#' head()
#'
#'
#' # Automatic decision, instead of a predefined definition
#' define_rb(nice_tidy, automatic = TRUE) %>% select(Sample, Abundance, Classification)
#'
#' # Automatic decision, using Davies-Bouldin index,
#' # instead of average Silhouette score (default)
#' define_rb(nice_tidy, automatic = TRUE, index = "Davies-Bouldin") %>%
#' select(Sample, Abundance, Classification)
#'
#' # User defined classifications
#' user_classifications <- c("very rare",
#'                           "rare",
#'                           "undetermined",
#'                           "abundant",
#'                           "very abundant")
#'
#' define_rb(nice_tidy, classification_vector = user_classifications) %>%
#' select(Sample, Abundance, Classification)
#'
#' # Easy to incorporate in big pipes
#' # Remove Archaea
#' # Remove taxa below 10 reads
#' # Classify according to a different set of classifications
#' nice_tidy %>%
#'  filter(Domain != "sk__Archaea") %>%
#'  filter(Abundance > 10) %>%
#'  define_rb(classification_vector = c("very rare",
#'                                      "rare",
#'                                      "abundant",
#'                                      "very abundant")) %>%
#'  select(Sample, Abundance, Classification)
#'
#'  # An example that summarises results
#' nice_tidy %>%
#'  filter(Domain != "sk__Archaea") %>%
#'  filter(Abundance > 10) %>%
#'  define_rb(classification_vector = c("very rare",
#'                                      "rare",
#'                                      "abundant",
#'                                      "very abundant")) %>%
#'  select(Sample, Abundance, Classification) %>%
#'  group_by(Sample, Classification) %>%
#'  summarise(totalAbundance = sum(Abundance))
#'}
#' @import dplyr
#' @importFrom rlang .data
#'
define_rb <- function(data,
                      classification_vector = c("Rare","Undetermined","Abundant"),
                      samples_col = "Sample",
                      abundance_col = "Abundance",
                      simplified = FALSE,
                      automatic = FALSE,
                      index = "Average Silhouette Score",
                      check_singles = FALSE,
                      ...){
  # Match samples_col and abundance_col with Samples and Abundance, respectively
  data <-
    data %>%
    rename(Sample = all_of(samples_col),
           Abundance = all_of(abundance_col))

  # Verify possible k values

  # Function to calculate maximum k of a sample
    sample_max_k <- function(data){
    data %>%
      filter(.data$Abundance > 0) %>%
      count(.data$Abundance) %>%
      pull(.data$Abundance) %>%
      length()
    }
  # Summary of maximum k possible of each sample
  maxk_summary <- data %>%
    group_by(.data$Sample) %>%
    tidyr::nest() %>%
      mutate(maxk = purrr::map(.x = data, .f = ~sample_max_k(.x))) %>%
    tidyr::unnest(maxk)

  if(sum(maxk_summary[, "maxk"] < 3) != 0){
    samples_to_remove <- maxk_summary %>%
      filter(maxk <= 3) %>%
      pull(Sample)
    # Remove samples with maxk < 3
    data <- data %>%
      filter(!Sample %in% samples_to_remove)
    # Warn user of samples discarded
    warning(c("Samples with less than 3 different species were discarded:", paste(samples_to_remove, collapse = ",")))
  }

  #If automatic, use suggest_k()
  if(isTRUE(automatic)){
    message("Automatic option set to TRUE, so classification vector was overwritten")
    automatic_k <- suggest_k(data, index = index, ...)
    classification_vector <- seq_along(1:automatic_k)
    message(paste0("K= ", automatic_k, " based on ", index,"."))
  }

  # Define number of cluster based on possible classifications
  k <- length(classification_vector)


  # Calculate k-medoids
    ## Apply cluster algorithm
    clustered_data <-
      data %>%
      filter(.data$Abundance > 0, !is.na(.data$Abundance)) %>%
      group_by(.data$Sample, .add = TRUE) %>%
      tidyr::nest() %>%
      mutate(pam_object = purrr::map(.x = data,
                                     .f = ~cluster::pam(.x$Abundance,
                                                        k = k,
                                                        diss = FALSE))) %>%
      mutate(Level = purrr::map(.x = .data$pam_object, .f = ~.x[[3]]), # obtain clusters
             Silhouette_scores = purrr::map(.x = .data$pam_object, .f = ~.x[[7]][[1]][,3])) %>%  ## obtain silhouette plots
      tidyr::unnest(cols = c("data", "Level", "Silhouette_scores"))

  # Make classification table
  classification_table <- clustered_data %>%
    group_by(.data$Sample, Level = as.factor(.data$Level), .add = TRUE) %>%
    summarize(Cluster_median_abundance = stats::median(.data$Abundance)) %>%
    arrange(.data$Sample, .data$Cluster_median_abundance) %>%
    mutate(Classification = factor(classification_vector, levels = classification_vector))

  # Apply classification_table to classify clusters
  classified_clusters <- clustered_data %>%
    mutate(Level = as.factor(.data$Level)) %>%
    left_join(classification_table)

  # Evaluate
  classified_clusters <-
    classified_clusters %>%
    group_by(.data$Sample, .data$Classification) %>%
    tidyr::nest() %>%
    mutate(median_Silhouette = purrr::map(.x = data, .f = ~median(.x$Silhouette_scores))) %>%
    mutate(Evaluation = purrr::map(.x = .data$median_Silhouette, .f = ~case_when(median_Silhouette > 0.9 ~ "Very good",
                                                                                 median_Silhouette > 0.75 ~ "Good",
                                                                                 median_Silhouette > 0.5 ~ "Sufficient",
                                                                                 median_Silhouette <= 0.5 ~ "Bad"))) %>%
    tidyr::unnest(cols = c(data, median_Silhouette, Evaluation))

  ## the nest steps only check if a warning is necessary, the output is classified clusters
  clusters_report <- classified_clusters %>%
    select(Sample, Classification, median_Silhouette, Evaluation) %>% ### break from here
    distinct() %>%
    group_by(.data$Classification) %>%
    count(.data$Evaluation)

  # Count number of samples with bad scores
  bad_samples <- clusters_report %>%
    filter(.data$Evaluation == "Bad") %>%
    pull(.data$n) %>%
    sum()

  #
  if(bad_samples > 0){
    warning(paste(bad_samples, "samples got a bad Silhouette score. Consider changing the number of classifications."))
    message("If half the observations within a classification are below 0.5 Silhouette score, we consider that the clustering was 'Bad'.")
    message("Check 'Evaluation' collumn for more details.")
  }

  if(check_singles == TRUE){
  # verify if any cluster is represented by a single taxon
  single_tax_clusters <- classified_clusters %>%
    group_by(.data$Sample, .data$Classification) %>%
    count() %>%
    filter(n == 1)

  samples_with_single_tax_cluster <-  single_tax_clusters %>%
    pull(Sample) %>%
    unique()

  n_single_clusters <- length(samples_with_single_tax_cluster)

  # warn user if there are clusters with a single taxon
  if(n_single_clusters > 1){
    warning(
      paste0("There are ",
             n_single_clusters,
             " samples with a cluster represented by a single taxon."))
  }}

  # Option to simplify
  if(simplified == TRUE){
    # Remove unnecessary columns
    classified_clusters %>%
      select(-"Level", -"pam_object",
             -"Evaluation", -"median_Silhouette",
             -"Silhouette_scores",
             -"Cluster_median_abundance")
  }

  return(classified_clusters)
}
