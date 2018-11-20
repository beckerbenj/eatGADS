#### Check consistency of missings
#############################################################################
#' Check and Adjust Missing Coding
#'
#' Function to check if missings are coded and labeled correctly in a GADSdat object.
#'
#' The function compares value labels and missing codes of a GADSdat object and its labels data frame. Missmatches are reported and can be automatically adjusted.
#'
#'@param GADSdat GADSdat object imported via eatGADS.
#'@param missingLabel Characatre how missing labels are named.
#'@param addMissingCode If [TRUE], missing codes are added according to occurence of [missingLabel] in [valLabel].
#'@param addMissingLabel If [TRUE], [generic missing] is added according to occurence of [mis] in [missings].
#'
#'@return Returns a GADSdat object
#'
#'@examples
#'# Example data set
#'to be done
#'
#'@export
checkMissings <- function(GADSdat, missingLabel = "missing", addMissingCode = TRUE, addMissingLabel = TRUE) {
  UseMethod("checkMissings")
}

#'@export
checkMissings.GADSdat <- function(GADSdat, missingLabel = "missing", addMissingCode = TRUE, addMissingLabel = TRUE) {
  check_GADSdat(GADSdat)
  labels <- GADSdat$labels

  missCode_rows_fail <- which(grepl(missingLabel, labels$valLabel) & is.na(labels$missings))
  missLabel_rows_fail <- which(labels$missings == "miss" & !grepl(missingLabel, labels$valLabel))

  ## Which variables are affected, how many adjustments are performed
  if(length(missCode_rows_fail) > 0) {
    message("The following variables have value labels including the term 'missing' which are not coded as missing:\n",
            paste(unique(labels[missCode_rows_fail, "varName"]), collapse = ", "))
    if(identical(addMissingCode, TRUE)) labels <- insert_string(df = labels, rows = missCode_rows_fail, col = "missings", string = "miss")
  }

  if(length(missLabel_rows_fail) > 0) {
    message("The following variables have values coded as missing but value label does not include the term 'missing':\n",
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

