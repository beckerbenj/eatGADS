
#############################################################################
#' Import Stata data
#'
#' Function to import \code{.dta} files while extracting meta information, e.g. variable and value labels.
#'
#' Stata files (\code{.dta}) store variable and value labels and assign specific formatting to variables. \code{import_stata} imports
#' data from Stata, while storing this meta-information separately in a long format data frame. Time and date variables are converted to character.
#'
#'@param filePath Source file location, ending on \code{.dta}.
#'@param checkVarNames Should variable names be checked for violations of \code{SQLite} and \code{R} naming rules?
#'@param labeledStrings Should strings as labeled values be allowed? This possibly corrupts all labeled values.
#'
#'@return Returns a list with the actual data \code{dat} and a data frame with all meta information in long format \code{labels}.
#'
#'@export
import_stata <- function(filePath, checkVarNames = TRUE, labeledStrings = FALSE) {
  df <- load_stata(filePath = filePath)
  out <- prepare_labels(rawDat = df, checkVarNames = checkVarNames, labeledStrings = labeledStrings)
  out
}

# Load data depending on format ---------------------------------------------------------
# import (keep NAs how they are coded to later mark values as missings but keep them seperatable)
load_stata <- function(filePath) {
  rawDat <- haven::read_stata(file = filePath)
  new_savDat(rawDat)
}
# create S3 object savDat for internal use
new_savDat <- function(rawDat) {
  stopifnot(is.data.frame(rawDat))
  structure(rawDat, class = c("savDat", "data.frame"))
}
