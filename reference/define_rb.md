# Define Rare Biosphere

Classifies taxa in each sample into either "rare", "undetermined" or
"abundant". Other classifications are allowed.

## Usage

``` r
define_rb(
  data,
  classification_vector = c("Rare", "Undetermined", "Abundant"),
  samples_col = "Sample",
  abundance_col = "Abundance",
  simplified = FALSE,
  automatic = FALSE,
  index = "Average Silhouette Score",
  check_singles = FALSE,
  ...
)
```

## Arguments

- data:

  A data.frame with, at least, a column for Abundance and Sample.
  Additional columns are allowed.

- classification_vector:

  A vector of strings with the names for each cluster, from lower to
  higher abundance. Default is c("Rare", "Undetermined", "Abundance").

- samples_col:

  String with name of column with sample names.

- abundance_col:

  String with name of column with abundance values.

- simplified:

  Can be TRUE/FALSE. Default (FALSE) provides an additional column with
  detailed [`cluster::pam()`](https://rdrr.io/pkg/cluster/man/pam.html)
  results and Silhouette scores. If TRUE, only the Classification result
  is added to the original input data.

- automatic:

  By default (FALSE), will assume a classification into "Rare",
  "Undetermined" or "Abundant". If TRUE, then it will automatically
  select the number of classifications (or k), based on the index
  argument.

- index:

  Index used to select best k. Can be one of: "Average Silhouette
  Score", "Davies-Bouldin" or "Calinski-Harabasz".

- check_singles:

  Default if FALSE. If TRUE, the user is warned of the number of
  clusters represented by a single taxon, if any.

- ...:

  Extra arguments.

## Value

The input data.frame with extra columns containing the classification
and additional metrics (if detailed = TRUE).

## Details

**Overview**

Function to cluster taxa abundance with partition around medoids
algorithm (Kaufman and Rousseuw. 1991). By default, we propose the
division into three clusters (k = 3), which can be translated into
convenient classifications: "rare", "undetermined" and "abundant". Taxa
from the cluster with lowest median abundance corresponds to the "rare
biosphere".

**The classification vector**

The classification vector (argument classification_vector) represents
the different clusters to be used, by ascending order of median
abundance. To change the number of clusters, change the number of
elements in the classification vector, **but order matters!** Depending
on the number of clusters used, you can change the meaning that best
applies to your research.

For example, you can use a classification vector with the designations:
"very rare", "rare", "abundant" and "very abundant"; which would apply a
k = 4 underneath. It is possible to use any number of clusters, as long
as they are within 2 and the maximum possible k.

The maximum possible k is the number of different abundance scores
observed in a single sample. Note, however, that we do not recommend any
clustering for k \> 10 and we also don't recommend k = 2 (we explain in
more detail in Pascoal et al., 2025; and in the vignette
[`vignette("explore-classifications")`](https://pascoalf.github.io/ulrb/articles/explore-classifications.md).

**Automatic selection of the number of clusters**

To automatically decide the number of clusters (i.e., the value of k),
it is possible to do so with the argument **automatic=TRUE**. For
details on complete automation of `define_rb()`, please see the
documentation for
[`suggest_k()`](https://pascoalf.github.io/ulrb/reference/suggest_k.md).
Briefly, the k with best average Silhouette score is selected from a
range of k values between 2 and 10. It is possible to decide k based on
other indices ("Davies-Bouldin" or "Calinsky-Harabasz").

If you want a more fine grained analysis of k values, we provide several
functions:

- [`evaluate_k()`](https://pascoalf.github.io/ulrb/reference/evaluate_k.md);

- [`check_avgSil()`](https://pascoalf.github.io/ulrb/reference/check_avgSil.md);

- [`check_DB()`](https://pascoalf.github.io/ulrb/reference/check_DB.md);

- [`check_CH()`](https://pascoalf.github.io/ulrb/reference/check_CH.md).

**Verify clustering results**

If half of the taxa of any cluster got a Silhouette score below 0.5 in
any sample, then a warning is provided. The warning provides the number
of times this issue occurred. You can inspect other alternatives to
reduce the occurrences of a bad clustering, but it is possible that, in
some situations, you just can't find an optimal clustering.

The detailed output gives you access to all of the clustering results:

- `pam_object` is a list with the original results from the
  [`cluster::pam()`](https://rdrr.io/pkg/cluster/man/pam.html), see
  [`cluster::pam()`](https://rdrr.io/pkg/cluster/man/pam.html)
  documentation for more details.

- `Level` is an integer indicating the specific cluster attributed by
  the [`cluster::pam()`](https://rdrr.io/pkg/cluster/man/pam.html)
  function for each observation. Its order is random.

- `Silhouette_scores` provides the Silhouette score obtained for each
  observation, i.e. a score for each taxa.

- `Cluster_median_abundance` provides the median taxa abundance of each
  cluster.

- `median_Silhouette` provides the median Silhouette score obtained for
  each cluster.

- `Evaluation` indicates if the silhouette score obtained for a given
  observation is below the median Silhouette of its cluster and sample.

You can make your own plots and analysis, but we also provide another
function,
[`plot_ulrb()`](https://pascoalf.github.io/ulrb/reference/plot_ulrb.md),
which illustrates the results obtained.

**Partition around medoids (pam)**

To calculate k-medoids, we used the partition around medoids (pam)
algorithm, which was described in Chapter 2 of "Finding Groups in Data:
An Introduction to Cluster Analysis." (Kaufman and Rousseeuw, 1991) and
implemented by the cluster package with the
[`cluster::pam()`](https://rdrr.io/pkg/cluster/man/pam.html) function.

Briefly, the pam algorithm is divided into two main phases: **build**
and **swap**.

The first phase (**build**) selects k observations as cluster
representatives. The first observation selected as representative is the
one that minimizes the sum of the dissimilarities to the remaining
observations. The second, third and so on repeat the same process, until
k clusters have been formed.

The **build** steps are:

1 - Propose a centroid with observation, \\i\\, which has not been
selected as a centroid yet

2 - Calculate the distance between another observation, \\j\\, and its
most similar observation, \\D_j\\; and calculate the difference with the
proposed centroid, \\i\\, i.e., \\d(j,i)\\

3 - If \\d(j,i) \> 0\\, then calculate its contribution to the centroid:
\$\$max(D_j - d(j,i),0)\$\$

4 - Calculate the total gain obtained by \\i\\, \$\$\sum\_{j}C\_{ji}\$\$

5 - From all possible centroids, select the one that maximizes the
previous total gain obtained, \$\$max_i \sum_jC\_{ji}\$\$

6 - Repeat until k observations have been selected as cluster
representatives.

The purpose of the next phase, **swap**, is to improve the
representatives for the clusters. The principle is to swap the cluster
representative between all possibilities and calculate the value sum of
dissimilarities between each observation and the closest centroid. The
swapping continues until no more improvement is possible, i.e., when the
minimum sum of dissimilarities of the clusters is reached.

**Notes**:

Understand that **ulrb** package considers each sample as an independent
community of taxa, which means clustering is also independent across
different samples. Thus, be aware that you will have clustering results
and metrics for each single sample, which is why we also provide some
functions to analyze results across any number of samples (see:
[`plot_ulrb()`](https://pascoalf.github.io/ulrb/reference/plot_ulrb.md)
for clustering results and
[`evaluate_k()`](https://pascoalf.github.io/ulrb/reference/evaluate_k.md)
for k selection).

## References

Kaufman, L., & Rousseuw, P. J. (1991). Chapter 2 in book Finding Groups
in Data: An Introduction to Cluster Analysis. Biometrics, 47(2), 788.
Pascoal, F., Branco, P., Torgo, L. et al. Definition of the microbial
rare biosphere through unsupervised machine learning. Commun Biol 8, 544
(2025). https://doi.org/10.1038/s42003-025-07912-4

## See also

[`suggest_k()`](https://pascoalf.github.io/ulrb/reference/suggest_k.md),
[`evaluate_k()`](https://pascoalf.github.io/ulrb/reference/evaluate_k.md),
[`plot_ulrb()`](https://pascoalf.github.io/ulrb/reference/plot_ulrb.md),
[`cluster::pam()`](https://rdrr.io/pkg/cluster/man/pam.html)

## Examples

``` r
# \donttest{
library(dplyr)
# Sample ID's
sample_names <- c("ERR2044662", "ERR2044663", "ERR2044664",
                   "ERR2044665", "ERR2044666", "ERR2044667",
                   "ERR2044668", "ERR2044669", "ERR2044670")

# If data is in wide format, with samples in cols
nice_tidy <- prepare_tidy_data(nice,
                               sample_names = sample_names,
                               samples_in = "cols")

# Straightforward with tidy format
define_rb(nice_tidy)
#> Joining with `by = join_by(Sample, Level)`
#> # A tibble: 2,177 × 17
#> # Groups:   Sample, Classification [27]
#>    Sample    Classification OTU   Domain Phylum Class Order Family Genus Species
#>    <chr>     <fct>          <chr> <chr>  <chr>  <chr> <chr> <chr>  <chr> <chr>  
#>  1 ERR20446… Rare           OTU_2 sk__A… p__Eu… c__C… NA    NA     NA    NA     
#>  2 ERR20446… Rare           OTU_5 sk__A… p__Eu… c__T… NA    NA     NA    NA     
#>  3 ERR20446… Rare           OTU_6 sk__A… p__Th… NA    NA    NA     NA    NA     
#>  4 ERR20446… Rare           OTU_7 sk__A… p__Th… c__   o__   f__    g__C… NA     
#>  5 ERR20446… Rare           OTU_8 sk__A… p__Th… c__   o__N… NA     NA    NA     
#>  6 ERR20446… Rare           OTU_… sk__A… p__Th… c__   o__N… f__Ni… g__N… NA     
#>  7 ERR20446… Rare           OTU_… sk__B… p__Ac… NA    NA    NA     NA    NA     
#>  8 ERR20446… Rare           OTU_… sk__B… p__Ac… c__A… o__A… NA     NA    NA     
#>  9 ERR20446… Rare           OTU_… sk__B… p__Ac… NA    NA    NA     NA    NA     
#> 10 ERR20446… Rare           OTU_… sk__B… p__Ac… c__A… NA    NA     NA    NA     
#> # ℹ 2,167 more rows
#> # ℹ 7 more variables: Abundance <int>, pam_object <list>, Level <fct>,
#> #   Silhouette_scores <dbl>, Cluster_median_abundance <dbl>,
#> #   median_Silhouette <dbl>, Evaluation <chr>

#Closer look
classified_table <- define_rb(nice_tidy)
#> Joining with `by = join_by(Sample, Level)`
classified_table %>%
select(Sample, Abundance, Classification) %>%
head()
#> # A tibble: 6 × 3
#> # Groups:   Sample, Classification [1]
#>   Sample     Abundance Classification
#>   <chr>          <int> <fct>         
#> 1 ERR2044662       165 Rare          
#> 2 ERR2044662       541 Rare          
#> 3 ERR2044662         8 Rare          
#> 4 ERR2044662        15 Rare          
#> 5 ERR2044662         5 Rare          
#> 6 ERR2044662         4 Rare          


# Automatic decision, instead of a predefined definition
define_rb(nice_tidy, automatic = TRUE) %>% select(Sample, Abundance, Classification)
#> Automatic option set to TRUE, so classification vector was overwritten
#> K= 2 based on Average Silhouette Score.
#> Joining with `by = join_by(Sample, Level)`
#> # A tibble: 2,177 × 3
#> # Groups:   Sample, Classification [18]
#>    Sample     Abundance Classification
#>    <chr>          <int> <fct>         
#>  1 ERR2044662       165 1             
#>  2 ERR2044662       541 1             
#>  3 ERR2044662         8 1             
#>  4 ERR2044662        15 1             
#>  5 ERR2044662         5 1             
#>  6 ERR2044662      9701 1             
#>  7 ERR2044662         4 1             
#>  8 ERR2044662        19 1             
#>  9 ERR2044662         1 1             
#> 10 ERR2044662        61 1             
#> # ℹ 2,167 more rows

# Automatic decision, using Davies-Bouldin index,
# instead of average Silhouette score (default)
define_rb(nice_tidy, automatic = TRUE, index = "Davies-Bouldin") %>%
select(Sample, Abundance, Classification)
#> Automatic option set to TRUE, so classification vector was overwritten
#> K= 4 based on Davies-Bouldin.
#> Joining with `by = join_by(Sample, Level)`
#> # A tibble: 2,177 × 3
#> # Groups:   Sample, Classification [36]
#>    Sample     Abundance Classification
#>    <chr>          <int> <fct>         
#>  1 ERR2044662       165 1             
#>  2 ERR2044662         8 1             
#>  3 ERR2044662        15 1             
#>  4 ERR2044662         5 1             
#>  5 ERR2044662         4 1             
#>  6 ERR2044662        19 1             
#>  7 ERR2044662         1 1             
#>  8 ERR2044662        61 1             
#>  9 ERR2044662       123 1             
#> 10 ERR2044662         1 1             
#> # ℹ 2,167 more rows

# User defined classifications
user_classifications <- c("very rare",
                          "rare",
                          "undetermined",
                          "abundant",
                          "very abundant")

define_rb(nice_tidy, classification_vector = user_classifications) %>%
select(Sample, Abundance, Classification)
#> Joining with `by = join_by(Sample, Level)`
#> # A tibble: 2,177 × 3
#> # Groups:   Sample, Classification [45]
#>    Sample     Abundance Classification
#>    <chr>          <int> <fct>         
#>  1 ERR2044662       165 very rare     
#>  2 ERR2044662         8 very rare     
#>  3 ERR2044662        15 very rare     
#>  4 ERR2044662         5 very rare     
#>  5 ERR2044662         4 very rare     
#>  6 ERR2044662        19 very rare     
#>  7 ERR2044662         1 very rare     
#>  8 ERR2044662        61 very rare     
#>  9 ERR2044662       123 very rare     
#> 10 ERR2044662         1 very rare     
#> # ℹ 2,167 more rows

# Easy to incorporate in big pipes
# Remove Archaea
# Remove taxa below 10 reads
# Classify according to a different set of classifications
nice_tidy %>%
 filter(Domain != "sk__Archaea") %>%
 filter(Abundance > 10) %>%
 define_rb(classification_vector = c("very rare",
                                     "rare",
                                     "abundant",
                                     "very abundant")) %>%
 select(Sample, Abundance, Classification)
#> Joining with `by = join_by(Sample, Level)`
#> # A tibble: 885 × 3
#> # Groups:   Sample, Classification [36]
#>    Sample     Abundance Classification
#>    <chr>          <int> <fct>         
#>  1 ERR2044662        19 very rare     
#>  2 ERR2044662        61 very rare     
#>  3 ERR2044662       123 very rare     
#>  4 ERR2044662        75 very rare     
#>  5 ERR2044662        24 very rare     
#>  6 ERR2044662       190 very rare     
#>  7 ERR2044662        13 very rare     
#>  8 ERR2044662        23 very rare     
#>  9 ERR2044662        20 very rare     
#> 10 ERR2044662       374 very rare     
#> # ℹ 875 more rows

 # An example that summarises results
nice_tidy %>%
 filter(Domain != "sk__Archaea") %>%
 filter(Abundance > 10) %>%
 define_rb(classification_vector = c("very rare",
                                     "rare",
                                     "abundant",
                                     "very abundant")) %>%
 select(Sample, Abundance, Classification) %>%
 group_by(Sample, Classification) %>%
 summarise(totalAbundance = sum(Abundance))
#> Joining with `by = join_by(Sample, Level)`
#> `summarise()` has regrouped the output.
#> ℹ Summaries were computed grouped by Sample and Classification.
#> ℹ Output is grouped by Sample.
#> ℹ Use `summarise(.groups = "drop_last")` to silence this message.
#> ℹ Use `summarise(.by = c(Sample, Classification))` for per-operation grouping
#>   (`?dplyr::dplyr_by`) instead.
#> # A tibble: 36 × 3
#> # Groups:   Sample [9]
#>    Sample     Classification totalAbundance
#>    <chr>      <fct>                   <int>
#>  1 ERR2044662 very rare                5842
#>  2 ERR2044662 rare                     8632
#>  3 ERR2044662 abundant                12953
#>  4 ERR2044662 very abundant           24242
#>  5 ERR2044663 very rare                9555
#>  6 ERR2044663 rare                    12622
#>  7 ERR2044663 abundant                14846
#>  8 ERR2044663 very abundant           28412
#>  9 ERR2044664 very rare                3143
#> 10 ERR2044664 rare                     6306
#> # ℹ 26 more rows
# }
```
