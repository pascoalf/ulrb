# Suggest k

Tool to help decide how many clusters to use for partition around
medoids algorithm.

## Usage

``` r
suggest_k(
  data,
  range = 2:10,
  samples_col = "Sample",
  abundance_col = "Abundance",
  index = "Average Silhouette Score",
  detailed = FALSE,
  ...
)
```

## Arguments

- data:

  a data.frame with, at least, the classification, abundance and sample
  information for each phylogenetic unit.

- range:

  The range of values of k to test, default is from 2 to 10.

- samples_col:

  String with name of column with sample names.

- abundance_col:

  string with name of column with abundance values. Default is
  "Abundance".

- index:

  Index used to select best k. Can be one of: "Average Silhouette
  Score", "Davies-Bouldin" or "Calinski-Harabasz".

- detailed:

  If False (default) returns an integer with best overall k. If TRUE,
  returns a list with full details.

- ...:

  Extra arguments.

## Value

Integer indicating best k from selected index. Optionally, can return a
list with details.

## Details

The best k is selected for each sample, based on the selected index. If
different k's are obtained for different samples (probable) then we
calculate the mean value of k and return it as an integer.
Alternatively, we can return a more detailed result in the form of a
list.

**Note**: this function is used within
[`define_rb()`](https://pascoalf.github.io/ulrb/reference/define_rb.md),
with default parameters, for the optional automatic selection of k.

**Detailed option**

If `detailed = TRUE`, then the output is a list with information to help
decide for k. More specifically, the list will include:

- A data.frame summarizing what information each index provides and how
  to interpret the value.

- A brief summary indicating the number of samples in the dataset and
  the range of k values used.

- A data.frame with the best k for each sample, based on each index.

**Automatic k selection**

If `detailed = FALSE`, this function will provide a single integer with
the best k. The **default** decision is based on the maximum average
Silhouette score obtained for the values of k between 3 and 10. To
better understand why the average Silhouette score and this range of k's
were selected, we refer to Pascoal et al., 2025 and to
vignette("explore-classifications").

Alternatively, this function can also provide the best k, as an integer,
based on another index (Davies-Bouldin and Calinski-Harabasz) and can
compare the entire of possible k's.

## See also

[`evaluate_k()`](https://pascoalf.github.io/ulrb/reference/evaluate_k.md),
[`evaluate_sample_k()`](https://pascoalf.github.io/ulrb/reference/evaluate_sample_k.md),
[`check_DB()`](https://pascoalf.github.io/ulrb/reference/check_DB.md),
[`check_CH()`](https://pascoalf.github.io/ulrb/reference/check_CH.md),
[`check_avgSil()`](https://pascoalf.github.io/ulrb/reference/check_avgSil.md),
[`cluster::pam()`](https://rdrr.io/pkg/cluster/man/pam.html)

## Examples

``` r

# \donttest{
# Get the best k with default parameters
suggest_k(nice_tidy)
#> [1] 2


# Get detailed results to decide for yourself
suggest_k(nice_tidy, detailed = TRUE, range = 2:7)
#> [[1]]
#> [1] "This list contains several details that might help you decide a k parameter."
#> 
#> [[2]]
#>                      Score                 Criteria                     Details
#> 1     Davies-Bouldin index Minimum value for best k Measures cluster separation
#> 2  Calinski-Harabasz index Maximum value for best k Measures cluster definition
#> 3 Average Silhouette Score Maximum value for best k    Measures cluster density
#> 
#> $SamplesSummary
#> [1] "You study has 9 samples. For each one we calculated all indices obtained for each k, from 2 to 7"
#> 
#> $DaviesBouldin
#> # A tibble: 9 × 3
#>   Sample         CH     k
#>   <chr>       <dbl> <int>
#> 1 ERR2044662 17589.     7
#> 2 ERR2044663 12741.     7
#> 3 ERR2044664 87610.     7
#> 4 ERR2044665  9486.     6
#> 5 ERR2044666 19652.     7
#> 6 ERR2044667  6504.     7
#> 7 ERR2044669  6616.     7
#> 8 ERR2044668 36144.     7
#> 9 ERR2044670  9480.     7
#> 
#> $CalinskiHarabasz
#> # A tibble: 9 × 3
#>   Sample         CH     k
#>   <chr>       <dbl> <int>
#> 1 ERR2044662 17589.     7
#> 2 ERR2044663 12741.     7
#> 3 ERR2044664 87610.     7
#> 4 ERR2044665  9486.     6
#> 5 ERR2044666 19652.     7
#> 6 ERR2044667  6504.     7
#> 7 ERR2044669  6616.     7
#> 8 ERR2044668 36144.     7
#> 9 ERR2044670  9480.     7
#> 
#> $averageSilhouette
#> # A tibble: 9 × 3
#>   Sample     average_Silhouette     k
#>   <chr>                   <dbl> <int>
#> 1 ERR2044662              0.977     2
#> 2 ERR2044663              0.978     2
#> 3 ERR2044664              0.984     2
#> 4 ERR2044665              0.979     2
#> 5 ERR2044666              0.983     2
#> 6 ERR2044667              0.972     2
#> 7 ERR2044669              0.976     2
#> 8 ERR2044668              0.979     2
#> 9 ERR2044670              0.932     3
#> 

# Get best k, based on Davies-Bouldin index
suggest_k(nice_tidy, detailed = FALSE, index = "Davies-Bouldin")
#> [1] 4
# }
```
