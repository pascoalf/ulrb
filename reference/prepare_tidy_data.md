# Prepare data in tidy format

Function to transforms common abundance table formats into a "long"
format.

## Usage

``` r
prepare_tidy_data(data, sample_names, samples_in = "cols", ...)
```

## Arguments

- data:

  a data.frame in "wide" format, with samples in either columns or rows.
  This data.frame should not include any data besides abundance values
  per sample, per taxonomic unit. Additional data (e.g. taxonomy
  details) should be added afterwards.

- sample_names:

  a vector with the name of all samples.

- samples_in:

  a vector specifying the location of the samples. It can either be
  "cols" (default) if samples are in columns, or "rows" if samples are
  in rows.

- ...:

  additional arguments

## Value

An abundance table in long format, compatible with dplyr pipes and
**ulrb** package functions.

## Details

This function guarantees that the abundance table includes one column
with sample ID's and one column with abundance.

To use this function, the user should have a vector with the samples
names as they appear in the abundance table. Usually simple data
wrangling with base R is enough to obtain this information from the
abundance table itself.

**Common species table formats**

There are two common formats for abundance tables:

- samples as rows and taxa as columns;

- taxa as rows and samples as columns.

However, both formats are not tidy/long, because they include several
columns with the same variable. They are in a "wide format" instead of a
"long format".

This function re-organizes samples and taxa so that there is a single
column with the samples ID's and another with the abundance scores.
Extra columns are allowed.

## See also

[`define_rb()`](https://pascoalf.github.io/ulrb/reference/define_rb.md)

## Examples

``` r
library(dplyr)
#
sample_names <- c("ERR2044662", "ERR2044663", "ERR2044664",
                   "ERR2044665", "ERR2044666", "ERR2044667",
                   "ERR2044668", "ERR2044669", "ERR2044670")

# Example for samples in cols and with additional data available
prepare_tidy_data(nice, sample_names = sample_names, samples_in = "cols")
#> # A tibble: 4,716 × 10
#>    OTU   Domain      Phylum    Class Order Family Genus Species Sample Abundance
#>    <chr> <chr>       <chr>     <chr> <chr> <chr>  <chr> <chr>   <chr>      <int>
#>  1 OTU_2 sk__Archaea p__Eurya… c__C… NA    NA     NA    NA      ERR20…       165
#>  2 OTU_2 sk__Archaea p__Eurya… c__C… NA    NA     NA    NA      ERR20…       323
#>  3 OTU_2 sk__Archaea p__Eurya… c__C… NA    NA     NA    NA      ERR20…        51
#>  4 OTU_2 sk__Archaea p__Eurya… c__C… NA    NA     NA    NA      ERR20…        70
#>  5 OTU_2 sk__Archaea p__Eurya… c__C… NA    NA     NA    NA      ERR20…       134
#>  6 OTU_2 sk__Archaea p__Eurya… c__C… NA    NA     NA    NA      ERR20…       216
#>  7 OTU_2 sk__Archaea p__Eurya… c__C… NA    NA     NA    NA      ERR20…         0
#>  8 OTU_2 sk__Archaea p__Eurya… c__C… NA    NA     NA    NA      ERR20…        11
#>  9 OTU_2 sk__Archaea p__Eurya… c__C… NA    NA     NA    NA      ERR20…         0
#> 10 OTU_3 sk__Archaea p__Eurya… c__C… o__C… f__    g__   s__Mar… ERR20…         0
#> # ℹ 4,706 more rows

# Example for samples in rows
# Select columns with samples from nice
nice_rows <- nice %>% select(all_of(sample_names))

# Change columns to rows
nice_rows <- nice_rows %>% t() %>% as.data.frame()

# Turn colnames into phylogenetic units ID
colnames(nice_rows) <- paste0("OTU_", seq_along(colnames(nice_rows)))

prepare_tidy_data(nice_rows, sample_names = sample_names, samples_in = "rows")
#> Taxa_id assumes each column is a taxonomic unit.
#> # A tibble: 4,716 × 3
#> # Groups:   Sample [9]
#>    Sample     Abundance Taxa_id
#>    <chr>          <int> <chr>  
#>  1 ERR2044662       165 OTU_1  
#>  2 ERR2044663       323 OTU_1  
#>  3 ERR2044664        51 OTU_1  
#>  4 ERR2044665        70 OTU_1  
#>  5 ERR2044666       134 OTU_1  
#>  6 ERR2044667       216 OTU_1  
#>  7 ERR2044668         0 OTU_1  
#>  8 ERR2044669        11 OTU_1  
#>  9 ERR2044670         0 OTU_1  
#> 10 ERR2044662         0 OTU_2  
#> # ℹ 4,706 more rows


# Extra examples with mock values
# Mock example 1 - wide table, samples in rows
mock_1 <- data.frame(Sample = paste0("S", 1:10),
                        Taxa1 = sample(10),
                        Taxa2 = sample(10),
                        Taxa3 = sample(10),
                        Taxa4 = sample(10),
                        Taxa5 = sample(10),
                        Taxa6 = sample(10))

prepare_tidy_data(mock_1[, -1], # remove Sample column
                  sample_names = mock_1$Sample,
                  samples_in = "rows")
#> Please check if samples in sample_names vector and rownames of data are in the same order.
#> Taxa_id assumes each column is a taxonomic unit.
#> # A tibble: 60 × 3
#> # Groups:   Sample [10]
#>    Sample Abundance Taxa_id
#>    <chr>      <int> <chr>  
#>  1 S1             7 Taxa1  
#>  2 S2             5 Taxa1  
#>  3 S3             6 Taxa1  
#>  4 S4            10 Taxa1  
#>  5 S5             4 Taxa1  
#>  6 S6             1 Taxa1  
#>  7 S7             9 Taxa1  
#>  8 S8             8 Taxa1  
#>  9 S9             3 Taxa1  
#> 10 S10            2 Taxa1  
#> # ℹ 50 more rows

# Mock example 2 - wide table, sample in columns
mock_2 <- data.frame(Sample = paste0("Taxa_", 1:6),
                        S1 = sample(6),
                        S2 = sample(6),
                        S3 = sample(6),
                        S4 = sample(6),
                        S5 = sample(6),
                        S6 = sample(6))

mock_2 %>%
 rename(TaxaID = Sample) %>% # Correct column name
 prepare_tidy_data(samples_in = "cols",
                   sample_names = colnames(mock_2)[-1])
#> # A tibble: 36 × 3
#>    TaxaID Sample Abundance
#>    <chr>  <chr>      <int>
#>  1 Taxa_1 S1             6
#>  2 Taxa_1 S2             1
#>  3 Taxa_1 S3             1
#>  4 Taxa_1 S4             2
#>  5 Taxa_1 S5             3
#>  6 Taxa_1 S6             6
#>  7 Taxa_2 S1             3
#>  8 Taxa_2 S2             4
#>  9 Taxa_2 S3             2
#> 10 Taxa_2 S4             4
#> # ℹ 26 more rows

```
