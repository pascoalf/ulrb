# ulrb
<!-- badges: start -->
  [![R-CMD-check](https://github.com/pascoalf/ulrb/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/pascoalf/ulrb/actions/workflows/R-CMD-check.yaml)
  [![test-coverage](https://github.com/pascoalf/ulrb/actions/workflows/test-coverage.yaml/badge.svg)](https://github.com/pascoalf/ulrb/actions/workflows/test-coverage.yaml)
<!-- badges: end -->


The R package **ulrb** stands for **Unsupervised Machine Learning definition of the Rare Biosphere**. As the name suggests, it applies unsupervised learning principles to define the rare biosphere.

More specifically, the partitioning around medoids (k-medoids) algorithm is 
used to divide phylogenetic units (ASVs, OTUs, Species, ...) within a 
microbial community (usually, a sample) into clusters. The clusters are then
ordered based on a user-defined classification vector. By default, our 
method classifies all phylogenetic units in one of these: "rare", "undetermined"
or "abundant". However, this can be changed by the user if necessary.

For detailed theory behind our reasoning for this definition of the microbial 
rare biosphere, results and applications, see our paper Pascoal et al., 2023 (in preparation). 
For more details on the R functions used and data wrangling please see the package 
documentation. 

For tutorials and documentation of the **urlb** package, visite our website: link.

**Installation**

- To install from CRAN (recommended)

```{r}
install.packages("ulrb") #not available yet
```
- To install from GitHub 

```{r}
library(devtools)
install_github("https://github.com/pascoalf/ulrb")
```


