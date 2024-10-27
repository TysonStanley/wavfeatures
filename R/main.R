# Define package structure
#' @title praatsyllable: R Package for Syllable Detection using Praat
#' @description Integrates the Praat Syllable Nuclei script for detecting syllables in audio files
#' @import fs
#' @import stringr
#' @import utils

# Helper function to locate Praat
find_praat <- function() {
  os <- .Platform$OS.type

  if (os == "windows") {
    praat_paths <- c(
      "C:/Program Files/Praat/Praat.exe",
      "C:/Program Files (x86)/Praat/Praat.exe"
    )
    for (path in praat_paths) {
      if (file.exists(path)) return(path)
    }
  } else if (os == "unix") {
    # Check common Unix/Mac locations
    praat_paths <- c("/usr/bin/praat", "/usr/local/bin/praat", "/Applications/Praat.app/Contents/MacOS/Praat")
    for (path in praat_paths) {
      if (file.exists(path)) return(path)
    }
  }

  stop("Praat not found. Please ensure Praat is installed and accessible.")
}

#' Detect syllables in audio files using Praat
#'
#' @param wav_file Path to input WAV file
#' @param ignorance_level Ignorance level/Intensity median (dB). Default 0 for unfiltered sounds, 2 for filtered
#' @param min_dip Minimum dip between peaks (dB). Default 2 for unfiltered sounds, 4 for filtered
#' @param praat_path Optional path to Praat executable
#' @return Path to generated TextGrid file
#' @export
detect_syllables <- function(wav_file,
                             ignorance_level = 0,
                             min_dip = 2,
                             praat_path = NULL) {

  # Input validation
  if (!file.exists(wav_file)) {
    stop("Input WAV file not found: ", wav_file)
  }

  # Find Praat if path not provided
  if (is.null(praat_path)) {
    praat_path <- find_praat()
  }

  # Create temporary script file
  script_path <- tempfile(fileext = ".praat")
  script_content <- system.file("praat", "syllable_nuclei.praat", package = "praatsyllable")
  file.copy(script_content, script_path)

  # Get directory and filename
  wav_dir <- dirname(wav_file)
  wav_name <- basename(wav_file)

  # Build Praat command
  praat_cmd <- sprintf('"%s" --run "%s" %f %f 1 "%s"',
                       praat_path,
                       script_path,
                       ignorance_level,
                       min_dip,
                       wav_dir)

  # Run Praat
  system_result <- system(praat_cmd)
  if (system_result != 0) {
    stop("Praat execution failed with error code: ", system_result)
  }

  # Clean up
  unlink(script_path)

  # Return path to generated TextGrid
  textgrid_path <- fs::path(wav_dir, str_replace(wav_name, "\\.wav$", ".syllables.TextGrid"))
  if (!file.exists(textgrid_path)) {
    stop("TextGrid file not generated: ", textgrid_path)
  }

  return(textgrid_path)
}

#' Read syllable count from TextGrid
#'
#' @param textgrid_path Path to TextGrid file
#' @return Number of syllables detected
#' @export
get_syllable_count <- function(textgrid_path) {
  if (!file.exists(textgrid_path)) {
    stop("TextGrid file not found: ", textgrid_path)
  }

  # Read TextGrid file
  lines <- readLines(textgrid_path)

  # Find points in the syllables tier
  point_lines <- grep("points \\[\\d+\\]:", lines)
  if (length(point_lines) == 0) {
    return(0)
  }

  # Extract number of points from first match
  points_str <- lines[point_lines[1]]
  points <- as.numeric(str_extract(points_str, "\\d+"))

  return(points)
}
