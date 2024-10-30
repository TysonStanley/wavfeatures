
<!-- README.md is generated from README.Rmd. Please edit that file -->

# `wavfeatures`

<!-- badges: start -->

<!-- badges: end -->

The goal of `wavfeatures` is to extract features of a `.wav` file. For
now, it is just the syllable nuclei.

## Installation

You can install the development version of wavfeatures like so:

``` r
remotes::install_github("tysonstanley/wavfeatures")
```

## Example

This is a basic example:

``` r
library(wavfeatures)
get_syllables_ps(path)
```

Can use other arguments to control the calculations

``` r
get_syllables_ps(path, threshold = -30, keep_intermediates = TRUE)
```
