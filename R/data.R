#' Raw OTU table from SSU rRNA amplicon of the Arctic Ocean during Winter-Spring Transition
#'
#' Not done yet.
#'
#' @format ## `nice_raw`
#' A data frame with 524 rows and 17 columns:
#' \describe{
#'   \item{ERR2044662, "ERR2044663", "ERR2044664","ERR2044665", "ERR2044666", "ERR2044667","ERR2044668", "ERR2044669", "ERR2044670"}{Country name}
#'   \item{OTU}{Taxonomic unit ID}
#'   \item{Domain}{Domain level classification of taxonomic unit}
#'   \item{Phylum}{Phylum level classification of taxonomic unit}
#'   \item{Class}{Class level classification of taxonomic unit}
#'   \item{Order}{Order level classification of taxonomic unit}
#'   \item{Family}{Family level classification of taxonomic unit}
#'   \item{Genus}{Genus level classification of taxonomic unit}
#'   \item{Species}{Species level classification of taxonomic unit}
#'   ...
#' }
#' @source <https://www.ebi.ac.uk/metagenomics/studies/MGYS00001922#analysis>
"nice_raw"

#' OTU table from SSU rRNA amplicon of the Arctic Ocean during Winter-Spring Transition
#'
#' Table with abundance and taxonomic classification of each OTU.
#' This OTU table was cleaned (see: XXXX) so that it only includes samples from 16S rRNA amplicon sequencing
#' and eukaryotes were filtered out. Additionally, we added a column with (OTU) with a ID for each OTU.
#' Raw reads were processed into OTUs by the MGnify platform (v5).
#' Original .fastq files with sequences are publicly available in European Nucleotide Archive under accesion number XXXX.
#' The original raw OTU table is publicly available in MGnify under accession number XXXX.
#'
#' @format ## `nice`
#' A data frame with 524 rows and 17 columns:
#' \describe{
#'   \item{ERR2044662, "ERR2044663", "ERR2044664","ERR2044665", "ERR2044666", "ERR2044667","ERR2044668", "ERR2044669", "ERR2044670"}{Country name}
#'   \item{OTU}{Taxonomic unit ID}
#'   \item{Domain}{Domain level classification of taxonomic unit}
#'   \item{Phylum}{Phylum level classification of taxonomic unit}
#'   \item{Class}{Class level classification of taxonomic unit}
#'   \item{Order}{Order level classification of taxonomic unit}
#'   \item{Family}{Family level classification of taxonomic unit}
#'   \item{Genus}{Genus level classification of taxonomic unit}
#'   \item{Species}{Species level classification of taxonomic unit}
#'   ...
#' }
#' @source <https://www.ebi.ac.uk/metagenomics/studies/MGYS00001922#analysis>
"nice"

#' Tidy OTU table from SSU rRNA amplicon of the Arctic Ocean during Winter-Spring Transition
#'
#' Tidy version of original OTU table (nice).
#' A new column (Sample) includes the sample identifiers and a new column (Abundance) includes the abundance for each taxonomic unit.
#' For details on OTU table processing see help page of nice data.
#'
#' @format ## `nice_tidy`
#' A data frame with 4716 rows and 10 columns:
#' \describe{
#'   \item{Sample}{Sample ID}
#'   \item{Abundance}{Abundance}
#'   \item{OTU}{Taxonomic unit ID}
#'   \item{Domain}{Domain level classification of taxonomic unit}
#'   \item{Phylum}{Domain level classification of taxonomic unit}
#'   \item{Class}{Domain level classification of taxonomic unit}
#'   \item{Order}{Domain level classification of taxonomic unit}
#'   \item{Family}{Domain level classification of taxonomic unit}
#'   \item{Genus}{Domain level classification of taxonomic unit}
#'   \item{Species}{Domain level classification of taxonomic unit}
#'   ...
#' }
#' @source <https://www.ebi.ac.uk/metagenomics/studies/MGYS00001922#analysis>
"nice_tidy"

#' Metadata of samples from SSU rRNA amplicon of the Arctic Ocean during Winter-Spring Transition
#'
#' This dataset provides information on the samples used for rRNA amplicon of the Arctic Ocean during Winter-Spring Transition.
#'
#' @format ## `nice_tidy`
#' A data frame with 4716 rows and 10 columns:
#' \describe{
#'   \item{Sample}{Sample ID used in the original study}
#'   \item{ENA_ID}{Sample ID equivalent to Sample in the OTU table}
#'   \item{Month}{Month of sampling}
#'   \item{Region}{Ocean region of sampling event}
#'   \item{Water.mass}{Water mass of sampling event}
#'   \item{Latitude}{Latitude of sampling event}
#'   \item{Longitude}{Longitude of sampling event}
#'
#' }
#' @source <https://link.springer.com/article/10.1007/s00248-021-01821-9>
"nice_env"
