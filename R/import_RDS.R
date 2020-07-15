
#### Import RDS
#############################################################################
#' Import RDS object
#'
#' Function to import \code{.RDS} files while extracting value labels from factors.
#'
#' Factors are integers with labeled variable levels. \code{import_RDS} extracts these labels and stores them in a separate meta data data.frame.
#' See \code{\link{import_DF}} for detailed information.
#'
#'@param filePath Source file location, ending on \code{.RDS}.
#'@param checkVarNames Should variable names be checked for violations of \code{SQLite} and \code{R} naming rules?
#'
#'@return Returns a list with the actual data \code{dat} and a data frame with all meta information in long format \code{labels}.
#'
#'
#'@export
import_RDS <- function(filePath, checkVarNames = TRUE) {
  df <- load_R(filePath = filePath)
  out <- prepare_labels(rawDat = df, checkVarNames = checkVarNames, labeledStrings = FALSE)
  out
}

# Load data depending on format ---------------------------------------------------------
# import (keep NAs how they are coded to later mark values as missings but keep them seperatable)
load_R <- function(filePath) {
  rawDat <- readRDS(file = filePath)
  new_data.frame(rawDat)
}
# create S3 object RDat for internal use
new_data.frame <- function(rawDat) {
  stopifnot(is.data.frame(rawDat))
  rawDat
}
