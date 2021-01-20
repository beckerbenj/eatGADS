#### Check consistency of missings
#############################################################################
#' Check and Adjust Missing Coding
#'
#' Function to check if missings are coded and labeled correctly in a \code{GADSdat} object.
#'
#' The function compares value labels \code{"valLabels"} and missing codes \code{"missings"} of a \code{GADSdat} object and its
#' meta data information. Mismatches are reported and can be automatically adjusted.
#'
#'@param GADSdat \code{GADSdat} object imported via \code{eatGADS}.
#'@param missingLabel Single string indicating how missing labels are commonly named in the value labels.
#'@param addMissingCode If \code{TRUE}, missing codes are added according to occurrence of \code{"missingLabel"} in \code{"valLabel"}.
#'@param addMissingLabel If \code{TRUE}, \code{"generic missing"} is added according to occurrence of \code{"miss"} in \code{"missings"}. As often various value labels for missings are used, this argument should be used with great care.
#'
#'@return Returns a \code{GADSdat} object.
#'
#'@examples
#'# Change example data set (create a value label with incorrect missing code)
#' pisa2 <- changeValLabels(pisa, varName = "computer_age",
#'                         value = 5, valLabel = "missing: No computer use")
#'
#' pisa3 <- checkMissings(pisa2)
#'
#'@export
checkMissings <- function(GADSdat, missingLabel = "missing", addMissingCode = TRUE, addMissingLabel = FALSE) {
  UseMethod("checkMissings")
}

#'@export
checkMissings.GADSdat <- function(GADSdat, missingLabel = "missing", addMissingCode = TRUE, addMissingLabel = FALSE) {
  check_GADSdat(GADSdat)
  labels <- GADSdat$labels

  missCode_rows_fail <- which(grepl(missingLabel, labels$valLabel) & (is.na(labels$missings) | labels$missings == "valid"))
  missLabel_rows_fail <- which(labels$missings == "miss" & !grepl(missingLabel, labels$valLabel))

  ## Which variables are affected, how many adjustments are performed
  if(length(missCode_rows_fail) > 0) {
    message("The following variables have value labels including the term '", missingLabel ,"' which are not coded as missing:\n",
            paste(unique(labels[missCode_rows_fail, "varName"]), collapse = ", "))
    if(identical(addMissingCode, TRUE)) labels <- insert_string(df = labels, rows = missCode_rows_fail, col = "missings", string = "miss")
  }

  if(length(missLabel_rows_fail) > 0) {
    message("The following variables have values coded as missings but value labels do not include the term '", missingLabel ,"':\n",
            paste(unique(labels[missLabel_rows_fail, "varName"]), collapse = ", "))
    if(identical(addMissingLabel, TRUE)) labels <- insert_string(df = labels, rows = missLabel_rows_fail, col = "valLabel", string = "generic missing")
  }

  GADSdat$labels <- labels
  GADSdat
}

insert_string <- function(df, rows, col, string) {
  message("'", string, "' is inserted into column ", col, " for ", length(rows), " rows.")
  df[rows, col] <- string
  df
}










