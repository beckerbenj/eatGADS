
#### Import RDS
#############################################################################
#' Import \code{RDS} file
#'
#' Function to import a \code{data.frame} stored as a \code{.RDS} file while extracting value labels from factors.
#'
#' Factors are integers with labeled variable levels. \code{import_RDS} extracts these labels and stores them in a separate meta data data.frame.
#' See \code{\link{import_DF}} for detailed information. This function is a wrapper around \code{\link{import_DF}}.
#'
#'@param filePath Source file location, ending on \code{.RDS}.
#'@param checkVarNames Should variable names be checked for violations of \code{SQLite} and \code{R} naming rules?
#'
#'@return Returns a list with the actual data \code{dat} and a data frame with all meta information in long format \code{labels}.
#'
#'
#'@export
import_RDS <- function(filePath, checkVarNames = TRUE) {
  rawDat <- readRDS(file = filePath)
  import_DF(rawDat, checkVarNames = checkVarNames)
}

