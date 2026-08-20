# V4-V5 16S rRNA gene amplicons, raw OTU table (N-ICE, 2015)

This is the "raw" data for the N-ICE dataset.

## Usage

``` r
nice_raw
```

## Format

### `nice_raw`

A data frame with 524 rows and 17 columns:

- ERR2044662, ERR2044663, ERR2044664, ERR2044665, ERR2044666,
  ERR2044667, ERR2044668, ERR2044669 and ERR2044670:

  Sample ID

- OTU:

  OTU ID

- Domain:

  Domain level classification of OTU

- Phylum:

  Phylum level classification of OTU

- Class:

  Class level classification of OTU

- Order:

  Order level classification of OTU

- Family:

  Family level classification of OTU

- Genus:

  Genus level classification of OTU

- Species:

  Species level classification of OTU

## Source

<https://www.ebi.ac.uk/metagenomics/studies/MGYS00001922#analysis>

## Details

The original sequencing results are available at European Nucleotide
Archive (accession number: PRJEB15043). Those reads were processed into
OTUs by MGnify platform (Study: MGYS00001922). The later study accession
provides the table used in here.

This table contains the taxonomy and an abundance score for each
taxonomic lineage, which we will refer to as "OTU" (Operational OTU) for
simplicity sake.

For details on the sampling campaign in the Arctic ocean, sequencing
protocols and bioinformatic processing, please see ref (de Sousa et al.,
2019).

## References

- de Sousa, A. G. G., Tomasino, M. P., Duarte, P., Fernández-Méndez, M.,
  Assmy, P., Ribeiro, H., Surkont, J., Leite, R. B., Pereira-Leal, J.
  B., Torgo, L., & Magalhães, C. (2019). Diversity and Composition of
  Pelagic Prokaryotic and Protist Communities in a Thin Arctic Sea-Ice
  Regime. Microbial Ecology, 78(2), 388–408.
  https://doi.org/10.1007/s00248-018-01314-2

## See also

[`nice()`](https://pascoalf.github.io/ulrb/reference/nice.md),
[nice_tidy](https://pascoalf.github.io/ulrb/reference/nice_tidy.md),
[nice_env](https://pascoalf.github.io/ulrb/reference/nice_env.md)
