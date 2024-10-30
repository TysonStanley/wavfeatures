.onLoad <- function(libname = find.package("wavfeatures"),
                    pkgname = "wavfeatures") {

  # global vars
  if (getRversion() >= "2.15.1") {
    utils::globalVariables(c("osVersion", "text"))
  }

  # find praat
  sys = Sys.info()[['sysname']]
  if (sys == "Darwin") path = "/Applications/Praat.app/Contents/MacOS/Praat"
  if (sys == "Linux") path = "/usr/bin/praat"
  if (sys == "Windows") path = "C:/Program Files/Praat.exe"
  options(wav2textgrid.praat.path = path)

  # finish it up
  invisible()
}

.onAttach <- function(libname = find.package("wavfeatures"),
                      pkgname = "wavfeatures") {

  path = getOption("wav2textgrid.praat.path")

  if (file.exists(path)){
    packageStartupMessage(paste("Praat found at", path))
  } else
    packageStartupMessage(paste("Did not find Praat at default location (", path, ").\nPlease run `set_praat_path()` with the path to your Praat application."))

  # finish it up
  invisible()
}
