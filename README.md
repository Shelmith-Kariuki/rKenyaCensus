
# rKenyaCensus2019

<!-- badges: start -->
<!-- badges: end -->

The goal of rKenyaCensus2019 is to provide tidy datasets obtained from the 2019 Kenya Population and Housing Census results.

## Installation

You can install the development version of rKenyaCensus2019 from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("Shelmith-Kariuki/rKenyaCensus2019")
```
And the released version from [CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("rKenyaCensus2019")
```
_Note: This package is not yet available on CRAN._


## Example

This is a basic example to show how the datasets can be extracted:

``` r

## Suppose we want to obtain the dataset that shows the distribution of Population by Sex and County. This is Table 2.2 in Volume 1.

## Load the package
library(rKenyaCensus2019)

## Extract the table
df <- V1_T2.2

## To learn more about the dataset, type
?V1_T2.2

```

