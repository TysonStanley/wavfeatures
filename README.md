
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

## Praat Script

The script the syllable nuclei is built on does the following:

Here’s the step-by-step breakdown of what the script does:

### 1. Initial Setup:

Takes input parameters for:

- Ignorance Level/Intensity Median (0 or 2 dB)
- Minimum dip between peaks (2 or 4 dB)
- Directory path containing sound files

Processes all .wav files in the specified directory

### 2. For each sound file, it:

- Reads the sound file
- Removes DC offset (Subtract mean)
- Creates an intensity analysis (50ms window)

### 3. Peak Detection:

- Calculates noise floor (minimum intensity)
- Finds maximum intensity
- Calculates median intensity
- Sets a threshold = median intensity + ignorance level
- Identifies all intensity peaks above this threshold

### 4. Peak Validation:

- Checks each peak to ensure there’s a sufficient dip in intensity
  before it
- Only keeps peaks where the difference between the peak and preceding
  dip is greater than the minimum dip parameter
- This helps distinguish between actual syllables and smaller intensity
  variations

### 5. Pitch Analysis:

- Performs pitch analysis to identify voiced segments
- Only keeps peaks that occur in voiced regions
- This helps eliminate false positives from noise or unvoiced consonants

### 6. TextGrid Creation:

- Creates a TextGrid file with two tiers: “syllables”
- Marks each validated, voiced peak as a point in the TextGrid
- Each point represents the center of a detected syllable
- Points are numbered sequentially (1, 2, 3, etc.)

### 7. The resulting TextGrid will contain:

- Point markers at each detected syllable nucleus
- Each point is labeled with a number indicating its sequence
- The points correspond to peaks in intensity that:
  1)  Are above the threshold
  2)  Have sufficient intensity dip before them
  3)  Occur in voiced regions of speech

### Key considerations:

- The script may miss unstressed syllables (as noted in the comments)
- It works better on clean recordings (less background noise)
- Different settings for Ignorance Level and Minimum dip are recommended
  for filtered vs unfiltered sounds
- The accuracy depends on the quality of the recording and the speaker’s
  articulation

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
