
<!-- README.md is generated from README.Rmd. Please edit that file -->

# starch

<!-- badges: start -->

[![R-CMD-check](https://github.com/torfason/starch/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/torfason/starch/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

Read Strava bulk export archives into tidy, analysis-ready tables.
`starch` parses the `activities.csv` index alongside the GPS track files
(GPX, TCX, FIT), reconciles them into a common schema, and writes the
result to Parquet.

## Installation

Install the development version from GitHub:

``` r
# install.packages("pak")
pak::pak("torfason/starch")
```

## Example

``` r
library(starch)

# ... to be documented
```
