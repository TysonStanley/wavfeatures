#' @importFrom wav2textgrid split_channels
#' @export
wav2textgrid::split_channels

#' @importFrom wav2textgrid get_boundaries
#' @export
wav2textgrid::get_boundaries


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
  script_content <- system.file("praat", "syllable-nuclei.praat", package = "wavfeatures")
  file.copy(script_content, script_path)

  # Get directory and filename
  wav_dir <- dirname(wav_file)
  wav_name <- basename(wav_file)

  # split channels
  chs <- wav2textgrid::split_channels(wav_file)
  fs::dir_create(file.path(wav_dir, "tmp"))
  fs::file_move(chs[[1]], file.path(wav_dir, "tmp"))
  fs::file_move(chs[[2]], file.path(wav_dir, "tmp"))

  # Build Praat command
  praat_cmd <- sprintf('"%s" --run "%s" %f %f 1 "%s"',
                       praat_path,
                       script_path,
                       ignorance_level,
                       min_dip,
                       file.path(wav_dir, "tmp"))

  # Run Praat
  system_result <- system(praat_cmd)
  if (system_result != 0) {
    stop("Praat execution failed with error code: ", system_result)
  }

  # Clean up
  unlink(script_path)

  # Return path to generated TextGrid
  textgrid_path <- fs::path(dirname(chs), str_replace(basename(chs[[1]]), "\\.wav$", ".syllables.TextGrid"))
  return(textgrid_path)
}

#' Calculate speaking turn length
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
  point_lines <- grep("points: size = ", lines)
  if (length(point_lines) == 0) {
    return(0)
  }

  # Extract number of points from first match
  points_str <- lines[point_lines[1]]
  points <- as.numeric(str_extract(points_str, "\\d+"))

  return(points)
}

#' Get Speaking Turn Lengths
#'
#' @param wav_file Path to input WAV file
#' @param min_pitch Minimum pitch (Hz)
#' @param time_step Time step (s)
#' @param threshold silence threshold, default is -45
#' @param min_silent_int Minimum silent interval (s)
#' @param min_sound_int Minimum sounding interval (s)
#'
#' @importFrom wav2textgrid get_boundaries
#' @importFrom dplyr filter
#' @importFrom readtextgrid read_textgrid
#'
#' @return Turn lengths
#' @export
get_turn_duration <- function(wav_file, min_pitch = 100, time_step = 0.0, threshold = -45, min_silent_int = 0.5, min_sound_int = 0.1) {
  if (!file.exists(wav_file)) stop("wav file not found: ", wav_file)

  # folder to place textgrid
  folder = fs::path_dir(wav_file)

  # create textgrid of sounding/silences
  wav2textgrid::get_boundaries(file.path(folder, ""), min_pitch, time_step, threshold, min_silent_int, min_sound_int)

  # Read TextGrid file
  silences = readtextgrid::read_textgrid(fs::dir_ls(folder, regexp = "ch1.wav_silences"))

  # calc total speaking turn lengths
  silences$turn_length = silences$xmax - silences$xmin
  silences = dplyr::filter(silences, text == "sounding")
  return(sum(silences$turn_length))
}


#' Get Syllables Per Second
#'
#' @param wav_file Path to input WAV file
#' @param ignorance_level Ignorance level/Intensity median (dB). Default 0 for unfiltered sounds, 2 for filtered
#' @param min_dip Minimum dip between peaks (dB). Default 2 for unfiltered sounds, 4 for filtered
#' @param praat_path Optional path to Praat executable
#' @param min_pitch Minimum pitch (Hz)
#' @param time_step Time step (s)
#' @param threshold silence threshold, default is -45
#' @param min_silent_int Minimum silent interval (s)
#' @param min_sound_int Minimum sounding interval (s)
#' @param keep_intermediates Keep the tmp folder with the textgrids? TRUE is kept, FALSE deletes the intermediate files
#'
#' @importFrom wav2textgrid get_boundaries
#' @importFrom dplyr filter
#' @importFrom readtextgrid read_textgrid
#'
#' @return Turn lengths
#' @export
get_syllables_ps <- function(
    wav_file, ignorance_level = 0, min_dip = 2, min_pitch = 100,
    time_step = 0.0, threshold = -45, min_silent_int = 0.5, min_sound_int = 0.1,
    praat_path = NULL, keep_intermediates = FALSE
  ){

  # step 1: textgrid syllables counts
  textgrid_path <- detect_syllables(wav_file, ignorance_level, min_dip, praat_path)

  # folder where tmp files live
  folder = dirname(textgrid_path[[1]])

  # step 2: calculate total syllables
  syll_count <- vector("list", length = length(textgrid_path))
  for (i in seq_along(textgrid_path)){
    syll_count[[i]] <- get_syllable_count(textgrid_path[[i]])
  }

  # step 3: calculate speaking duration
  wav_file <- fs::dir_ls(folder, regexp = "wav$")
  turn_length <- vector("list", length = length(syll_count))
  for (i in seq_along(syll_count)){
    turn_length[[i]] <- get_turn_duration(wav_file[[i]], min_pitch, time_step, threshold, min_silent_int, min_sound_int)
  }

  # clean up
  if (! keep_intermediates) fs::dir_delete(folder)

  # calculate and return
  final <- data.frame(count = unlist(syll_count), length = unlist(turn_length))
  final$syll_ps <- final$count/final$length
  return(final)
}
