\name{ulrb-package}
\alias{ulrb-package}
\docType{package}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  ulrb: Unsupervised Learning Based Definition of Microbial Rare Biosphere
}
\description{
  The R package ulrb stands for Unsupervised Machine Learning definition of the
  Rare Biosphere. As the name suggests, it applies unsupervised learning principles
  to define the rare biosphere.

  More specifically, the partitioning around medoids (k-medoids) algorithm is used
  to divide phylogenetic units (ASVs, OTUs, Species, …) within a microbial community
  (usually, a sample) into clusters. The clusters are then ordered based on a user-defined
  classification vector. By default, our method classifies all phylogenetic units in one
  of these: “rare”, “undetermined” or “abundant”. In alternative, we provide functions
  to help the user decide the number of clusters and we also provide a fully automated
  option. Besides clustering, we have functions to help you evaluate the clustering
  quality (e.g. silhouette scores).

  For detailed theory behind our reasoning for this definition of the microbial rare biosphere,
  results and applications, see our paper Pascoal et al., 2023 (in preparation).
  For more details on the R functions used and data wrangling please see the package documentation.
}
\details{
  \tabular{ll}{
    Name: \tab ulrb\cr
    Type: \tab Package\cr
    Version: \tab 0.1.0\cr
    %update!
      Date: \tab 2023-11-13\cr
    License: \tab GPL 3\cr
  }



}
\references{
  Pascoal, F., Paula, B., Torgo, L., Costa, R., Magalhães, C. (2023) \emph{Unsupervised machine learning definition of the microbial rare biosphere} Manuscript in preparation.
}
\author{ Francisco Pascoal \email{fpascoal1996@gmail.com}, Paula Branco
  \email{paobranco@gmail.com}, Luis Torgo, Rodrigo Costa \email{rodrigoscosta@tecnico.ulisboa.pt}, Catarina Magalhães \email{catarinamagalhaes1972@gmail.com}

  Maintainer:
    Francisco Pascoal}

\keyword{ package }

\examples{
  \donttest{
    library(ulrb)
    # nice is an OTU table in wide format
    head(nice)

    # first, we tidy the "nice" OTU table
    sample_names <- c("ERR2044662", "ERR2044663", "ERR2044664",
                      "ERR2044665", "ERR2044666", "ERR2044667",
                      "ERR2044668", "ERR2044669", "ERR2044670")

    # If data is in wide format, with samples in cols
    nice_tidy <- prepare_tidy_data(nice,
                                   sample_names = sample_names,
                                   samples_in = "cols")

    # second, we apply ulrb algorithm in automatic setting
    nice_classification_results <- define_rb(nice_tidy)

    # third, we plot microbial community and the quality of k-medoids clustering
    plot_ulrb(nice_classification_results, taxa_col = "OTU", plot_all = TRUE)

    # In case you want to inspect the result of a particular sample, do:
    plot_ulrb(nice_classification_results, taxa_col = "OTU", sample_id = "ERR2044662")
        }
  }

