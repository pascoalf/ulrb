#' V4-V5 16S rRNA gene amplicons, raw OTU table (N-ICE, 2015)
#'
#' This is the "raw" data for the N-ICE dataset.
#'
#' The original sequencing results are available at European Nucleotide Archive
#' (accession number: PRJEB15043). Those reads were processed into OTUs
#' by MGnify platform (Study: MGYS00001922). The later study accession provides the
#' table used in here.
#'
#' This table contains the taxonomy and an abundance score for each taxonomic lineage, which
#' we will refer to as "OTU" (Operational OTU) for simplicity sake.
#'
#' For details on the sampling campaign in the Arctic ocean, sequencing protocols and
#' bioinformatic processing, please see ref (de Sousa et al., 2019).
#'
#' @seealso [nice()], [nice_tidy], [nice_env]
#'
#' @references
#' - de Sousa, A. G. G., Tomasino, M. P., Duarte, P., Fernández-Méndez, M., Assmy, P., Ribeiro, H., Surkont, J., Leite, R. B., Pereira-Leal, J. B., Torgo, L., & Magalhães, C. (2019). Diversity and Composition of Pelagic Prokaryotic and Protist Communities in a Thin Arctic Sea-Ice Regime. Microbial Ecology, 78(2), 388–408. https://doi.org/10.1007/s00248-018-01314-2
#'
#' @format ## `nice_raw`
#' A data frame with 524 rows and 17 columns:
#' \describe{
#'   \item{ERR2044662,
#'         ERR2044663,
#'         ERR2044664,
#'         ERR2044665,
#'         ERR2044666,
#'         ERR2044667,
#'         ERR2044668,
#'         ERR2044669 and
#'         ERR2044670}{Sample ID}
#'   \item{OTU}{OTU ID}
#'   \item{Domain}{Domain level classification of OTU}
#'   \item{Phylum}{Phylum level classification of OTU}
#'   \item{Class}{Class level classification of OTU}
#'   \item{Order}{Order level classification of OTU}
#'   \item{Family}{Family level classification of OTU}
#'   \item{Genus}{Genus level classification of OTU}
#'   \item{Species}{Species level classification of OTU}
#'   ...
#' }
#' @source <https://www.ebi.ac.uk/metagenomics/studies/MGYS00001922#analysis>
"nice_raw"

#' V4-V5 16S rRNA gene amplicons, clean OTU table (N-ICE, 2015)
#'
#' Table in "wide" format with abundance and taxonomic classification of each OTU.
#'
#' This OTU table was cleaned so that it only includes samples from 16S rRNA amplicon sequencing
#' and no eukaryotes (similarly to Pascoal et al., 2022). Additionally, we added a column with a ID for each OTU.
#'
#' For details on raw data, see [nice_raw]
#'
#' @seealso [nice_tidy()], [nice_raw], [nice_env]
#'
#' @references
#' - Pascoal, F., Costa, R., Assmy, P., Duarte, P., & Magalhães, C. (2022). Exploration of the Types of Rarity in the Arctic Ocean from the Perspective of Multiple Methodologies. Microbial Ecology, 84(1), 59–72. https://doi.org/10.1007/s00248-021-01821-9
#'
#' @format ## `nice`
#' A data frame with 524 rows and 17 columns:
#' \describe{
#'   \item{ERR2044662,
#'         ERR2044663,
#'         ERR2044664,
#'         ERR2044665,
#'         ERR2044666,
#'         ERR2044667,
#'         ERR2044668,
#'         ERR2044669 and
#'         ERR2044670}{Sample ID}
#'   \item{OTU}{OTU ID}
#'   \item{Domain}{Domain level classification of OTU}
#'   \item{Phylum}{Phylum level classification of OTU}
#'   \item{Class}{Class level classification of OTU}
#'   \item{Order}{Order level classification of OTU}
#'   \item{Family}{Family level classification of OTU}
#'   \item{Genus}{Genus level classification of OTU}
#'   \item{Species}{Species level classification of OTU}
#'   ...
#' }
#' @source <https://www.ebi.ac.uk/metagenomics/studies/MGYS00001922#analysis>
"nice"

#' V4-V5 16S rRNA gene amplicons, clean OTU table in tidy/long format (N-ICE, 2015)
#'
#' Original OTU table ([nice]) in "long" format.
#'
#' A new column (Sample) includes the sample identifiers and a new column
#' (Abundance) includes the abundance for each OTU.
#' For details on OTU table processing see help pages for [nice] and [nice_raw].
#'
#' @details
#' Some details on N-ICE dataset:
#'
#' This dataset resulted from the Norwegian Young Sea Ice expedition (N-ICE)
#' in 2015 (Granskog et al., 2018). The sample processing and DNA sequencing
#' were described in de Sousa et al., 2019, the bioinformatic processing was
#' performed by the MGnify platform (v5) (Mitchell et al., 2020).
#'
#' Since the purpose of this dataset if for creating examples and testing the package,
#' we did not apply strict quality control to the final OTU table. Thus, we
#' didn't remove singletons, etc. However, we did remove any
#' non-prokarotic OTUs and organelles, if any (Pascoal et al., 2022).
#'
#' @seealso [nice()], [nice_raw], [nice_env]
#'
#' @references
#' - Mitchell, A. L., Almeida, A., Beracochea, M., Boland, M., Burgin, J., Cochrane, G., Crusoe, M. R., Kale, V., Potter, S. C., Richardson, L. J., Sakharova, E., Scheremetjew, M., Korobeynikov, A., Shlemov, A., Kunyavskaya, O., Lapidus, A., & Finn, R. D. (2019). MGnify: the microbiome analysis resource in 2020. Nucleic Acids Research, 48(D1), D570–D578.
#'
#' - Granskog, M. A., Fer, I., Rinke, A., & Steen, H. (2018). Atmosphere-Ice-Ocean-Ecosystem Processes in a Thinner Arctic Sea Ice Regime: The Norwegian Young Sea ICE (N-ICE2015) Expedition. Journal of Geophysical Research: Oceans, 123(3), 1586–1594.
#'
#' - de Sousa, A. G. G., Tomasino, M. P., Duarte, P., Fernández-Méndez, M., Assmy, P., Ribeiro, H., Surkont, J., Leite, R. B., Pereira-Leal, J. B., Torgo, L., & Magalhães, C. (2019). Diversity and Composition of Pelagic Prokaryotic and Protist Communities in a Thin Arctic Sea-Ice Regime. Microbial Ecology, 78(2), 388–408.
#'
#' - Pascoal, F., Costa, R., Assmy, P., Duarte, P., & Magalhães, C. (2022). Exploration of the Types of Rarity in the Arctic Ocean from the Perspective of Multiple Methodologies. Microbial Ecology, 84(1), 59–72.
#'
#' @format ## `nice_tidy`
#' A data frame with 4716 rows and 10 columns:
#' \describe{
#'   \item{Sample}{Sample ID}
#'   \item{Abundance}{Abundance}
#'   \item{OTU}{OTU ID}
#'   \item{Domain}{Domain level classification of OTU}
#'   \item{Phylum}{Domain level classification of OTU}
#'   \item{Class}{Domain level classification of OTU}
#'   \item{Order}{Domain level classification of OTU}
#'   \item{Family}{Domain level classification of OTU}
#'   \item{Genus}{Domain level classification of OTU}
#'   \item{Species}{Domain level classification of OTU}
#'   ...
#' }
#' @source <https://www.ebi.ac.uk/metagenomics/studies/MGYS00001922#analysis>
"nice_tidy"

#' Metadata of samples from OTU tables (N-ICE, 2015)
#'
#' This dataset provides information on the samples used for the N-ICE dataset.
#'
#' Based on de Sousa et al., 2019.
#'
#' @seealso [nice], [nice_raw], [nice_tidy]
#'
#' @references
#' - de Sousa, A. G. G., Tomasino, M. P., Duarte, P., Fernández-Méndez, M., Assmy, P., Ribeiro, H., Surkont, J., Leite, R. B., Pereira-Leal, J. B., Torgo, L., & Magalhães, C. (2019). Diversity and Composition of Pelagic Prokaryotic and Protist Communities in a Thin Arctic Sea-Ice Regime. Microbial Ecology, 78(2), 388–408.
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
