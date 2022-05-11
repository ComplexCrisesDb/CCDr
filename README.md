
<!-- README.md is generated from README.Rmd. Please edit that file -->

# *CCDr* a R package to extend the Complex Crises database (CCDB)

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)

last update: 2022-05-11

Toolbox to extend the *Complex Crises Database* (CCD) the underlying
data of the paper:

Manuel Bétin, Umberto Collodel. The Complex Crises Database: 70 Years of
Macroeconomic Crises. 2021.
<https://halshs.archives-ouvertes.fr/halshs-03268889/>

Find more info and download the database and raw text files from
<https://doi.org/10.7910/DVN/CN0PR9>

Current version: 2.0.0

see NEWS.md for more details on new features

## Installation

The current version of the package is available on github and can be
installed using the devtools package.

``` r
#devtools::install_github("manuelbetin/CCDr")
library(CCDr)
```

## Usage

The package provides several functions to compute term frequencies on
the corpus of reports. Due to the different potential usages and for the
necessity to handle large amounts of data several wrap up functions are
provided to be able to perform the different steps one by one or
sequentially. The packages is constructed in three different blocs:

-   Lexicon: define and prepare categories and keywords
-   Corpus: download, explore and aggregate
-   Term Frequencies: compute the indexes

The main functions are:

-   **ccdr.lexicon()**: provide the list of categories and keywords
-   **scrap.ccdr.files()**: download reports in pdf formats
-   **ccdr.corpus()**: transform pdf into a dataframe of text
-   **ccdr.tfs()**: run the term frequency on the corpus for several
    categories
-   **run.ccdr.tf()**; run the term frequency on locally stored corpus
-   **scrap.ccdr.tfs()** run the term frequency directly downloading the
    files
-   **update.ccdr.tfs()** update the term frequency matrix with new
    categories
