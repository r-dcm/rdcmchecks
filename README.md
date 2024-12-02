
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rdcmchecks

<!-- badges: start -->

[![R-CMD-check](https://github.com/r-dcm/rdcmchecks/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/r-dcm/rdcmchecks/actions/workflows/R-CMD-check.yaml)
[![test-coverage](https://github.com/r-dcm/rdcmchecks/actions/workflows/test-coverage.yaml/badge.svg)](https://github.com/r-dcm/rdcmchecks/actions/workflows/test-coverage.yaml)
[![pkgdown](https://github.com/r-dcm/rdcmchecks/actions/workflows/pkgdown.yaml/badge.svg)](https://github.com/r-dcm/rdcmchecks/actions/workflows/pkgdown.yaml)
<!-- badges: end -->

The goal of rdcmchecks is to provide utility functions for checking
arguments provided to functions across the suite of r-dcm packages.
Rather than duplicating code for checking arguments across packages,
check functions can be added to rdcmchecks, which can in turn be
imported to any needed packages.

## Installation

You can install the development version of rdcmchecks from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("r-dcm/rdcmchecks")
```