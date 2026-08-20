# V4-V5 16S rRNA gene amplicons, clean OTU table (N-ICE, 2015)

Table in "wide" format with abundance and taxonomic classification of
each OTU.

## Usage

``` r
nice
```

## Format

### `nice`

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

This OTU table was cleaned so that it only includes samples from 16S
rRNA amplicon sequencing and no eukaryotes (similarly to Pascoal et al.,
2022). Additionally, we added a column with a ID for each OTU.

For details on raw data, see
[nice_raw](https://pascoalf.github.io/ulrb/reference/nice_raw.md)

## References

- Pascoal, F., Costa, R., Assmy, P., Duarte, P., & Magalhães, C. (2022).
  Exploration of the Types of Rarity in the Arctic Ocean from the
  Perspective of Multiple Methodologies. Microbial Ecology, 84(1),
  59–72. https://doi.org/10.1007/s00248-021-01821-9

## See also

[`nice_tidy()`](https://pascoalf.github.io/ulrb/reference/nice_tidy.md),
[nice_raw](https://pascoalf.github.io/ulrb/reference/nice_raw.md),
[nice_env](https://pascoalf.github.io/ulrb/reference/nice_env.md)
