#
# Obtained from link: https://www.ebi.ac.uk/metagenomics/studies/MGYS00001922#analysis
# At 06/01/2023
nice_raw <- read.delim("~/Downloads/ERP024265_taxonomy_abundances_SSU_v5.0.tsv")

# Change name of first column
nice_clean <- dplyr::rename(nice_raw, Taxonomy = "X.SampleID")

# Add col with taxonomic units (OTUs in this case)
nice_clean <- dplyr::mutate(nice_clean, OTU = paste0("OTU_", row_number()))

# Select 16S rRNA amplicon sequencing samples
selected_samples <- c("ERR2044662", "ERR2044663", "ERR2044664",
                      "ERR2044665", "ERR2044666", "ERR2044667",
                      "ERR2044668", "ERR2044669", "ERR2044670")

nice_clean <- dplyr::select(nice_clean, selected_samples, OTU, Taxonomy)


# Clean taxonomy
nice_clean <- tidyr::separate(nice_clean, Taxonomy,
                              c("Domain","Kingdom","Phylum","Class","Order","Family","Genus","Species"),sep=";")

# Remove Kingdom column, because it is not used for prokaryotes
nice_clean <- dplyr::select(nice_clean, -Kingdom)

# Remove eukaryotes
nice_clean <- dplyr::filter(nice_clean, Domain != "sk__Eukaryota")

# Remove unclassified OTUs at phylum level
nice_clean <- dplyr::filter(nice_clean, !is.na(Phylum))

# Check if everything looks normal
head(nice_clean)

# Save dataset for package
nice <- nice_clean

usethis::use_data(nice, overwrite = TRUE)
