#### Import SPSS data
#############################################################################
#' Import SPSS data
#'
#' Function to import \code{.sav} files while extracting meta information, e.g. variable and value labels.
#'
#' SPSS files (\code{.sav}) store variable and value labels and assign specific formatting to variables. \code{import_spss} imports
#' data from SPSS, while storing this meta-information separately in a long format data frame. Value labels and missing labels are used
#' to identify missing values (see \code{\link{checkMissings}}). Time and date variables are converted to character.
#'
#'@param filePath Source file location, ending on \code{.sav}.
#'@param checkVarNames Should variable names be checked for violations of \code{SQLite} and \code{R} naming rules?
#'@param labeledStrings Should strings as labeled values be allowed? If \code{"drop"} (default), all labeled strings are dropped and \code{NAs} occur in the meta data. If \code{"transform"}, all underlying values are transformed to numeric. If \code{"keep"}, value labels stay untouched. However, the latter possibly corrupts all labeled values.
#'@param encoding The character encoding used for the file. The default, \code{NULL}, use the encoding specified in the file, but sometimes this value is incorrect and it is useful to be able to override it.
#'
#'@return Returns a list with the actual data \code{dat} and a data frame with all meta information in long format \code{labels}.
#'
#'@examples
#'# Use spss data from within package
#'spss_path <- system.file("extdata", "pisa.zsav", package = "eatGADS")
#'pisa_gads <- import_spss(spss_path)
#'
#'@export
import_spss <- function(filePath, checkVarNames = TRUE, labeledStrings = c("drop", "keep", "transform"), encoding = NULL) {
  labeledStrings <- match.arg(labeledStrings)
  df <- load_spss(filePath = filePath, encoding = encoding)
  out <- prepare_labels(rawDat = df, checkVarNames = checkVarNames, labeledStrings = labeledStrings)
  out
}

# Load data depending on format ---------------------------------------------------------
# import (keep NAs how they are coded to later mark values as missings but keep them seperatable)
load_spss <- function(filePath, encoding = NULL) {
  #browser()
  rawDat <- haven::read_sav(file = filePath, user_na = TRUE, encoding = encoding)
  new_savDat(rawDat)
}
# create S3 object savDat for internal use
new_savDat <- function(rawDat) {
  stopifnot(is.data.frame(rawDat))
  structure(rawDat, class = c("savDat", "data.frame"))
}
