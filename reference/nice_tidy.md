# V4-V5 16S rRNA gene amplicons, clean OTU table in tidy/long format (N-ICE, 2015)

Original OTU table
([nice](https://pascoalf.github.io/ulrb/reference/nice.md)) in "long"
format.

## Usage

``` r
nice_tidy
```

## Format

### `nice_tidy`

A data frame with 4716 rows and 10 columns:

- Sample:

  Sample ID

- Abundance:

  Abundance

- OTU:

  OTU ID

- Domain:

  Domain level classification of OTU

- Phylum:

  Domain level classification of OTU

- Class:

  Domain level classification of OTU

- Order:

  Domain level classification of OTU

- Family:

  Domain level classification of OTU

- Genus:

  Domain level classification of OTU

- Species:

  Domain level classification of OTU

## Source

<https://www.ebi.ac.uk/metagenomics/studies/MGYS00001922#analysis>

## Details

A new column (Sample) includes the sample identifiers and a new column
(Abundance) includes the abundance for each OTU. For details on OTU
table processing see help pages for
[nice](https://pascoalf.github.io/ulrb/reference/nice.md) and
[nice_raw](https://pascoalf.github.io/ulrb/reference/nice_raw.md).

Some details on N-ICE dataset:

This dataset resulted from the Norwegian Young Sea Ice expedition
(N-ICE) in 2015 (Granskog et al., 2018). The sample processing and DNA
sequencing were described in de Sousa et al., 2019, the bioinformatic
processing was performed by the MGnify platform (v5) (Mitchell et al.,
2020).

Since the purpose of this dataset if for creating examples and testing
the package, we did not apply strict quality control to the final OTU
table. Thus, we didn't remove singletons, etc. However, we did remove
any non-prokarotic OTUs and organelles, if any (Pascoal et al., 2022).

## References

- Mitchell, A. L., Almeida, A., Beracochea, M., Boland, M., Burgin, J.,
  Cochrane, G., Crusoe, M. R., Kale, V., Potter, S. C., Richardson, L.
  J., Sakharova, E., Scheremetjew, M., Korobeynikov, A., Shlemov, A.,
  Kunyavskaya, O., Lapidus, A., & Finn, R. D. (2019). MGnify: the
  microbiome analysis resource in 2020. Nucleic Acids Research, 48(D1),
  D570–D578.

- Granskog, M. A., Fer, I., Rinke, A., & Steen, H. (2018).
  Atmosphere-Ice-Ocean-Ecosystem Processes in a Thinner Arctic Sea Ice
  Regime: The Norwegian Young Sea ICE (N-ICE2015) Expedition. Journal of
  Geophysical Research: Oceans, 123(3), 1586–1594.

- de Sousa, A. G. G., Tomasino, M. P., Duarte, P., Fernández-Méndez, M.,
  Assmy, P., Ribeiro, H., Surkont, J., Leite, R. B., Pereira-Leal, J.
  B., Torgo, L., & Magalhães, C. (2019). Diversity and Composition of
  Pelagic Prokaryotic and Protist Communities in a Thin Arctic Sea-Ice
  Regime. Microbial Ecology, 78(2), 388–408.

- Pascoal, F., Costa, R., Assmy, P., Duarte, P., & Magalhães, C. (2022).
  Exploration of the Types of Rarity in the Arctic Ocean from the
  Perspective of Multiple Methodologies. Microbial Ecology, 84(1),
  59–72.

## See also

[`nice()`](https://pascoalf.github.io/ulrb/reference/nice.md),
[nice_raw](https://pascoalf.github.io/ulrb/reference/nice_raw.md),
[nice_env](https://pascoalf.github.io/ulrb/reference/nice_env.md)
