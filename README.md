
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

# Detect syllables in a WAV file
textgrid_path <- detect_syllables(
  "path/to/audio.wav",
  ignorance_level = 0,
  min_dip = 2
)

# Get the syllable count
syllable_count <- get_syllable_count(textgrid_path)
print(paste("Detected", syllable_count, "syllables"))
```
