#### Check consistency of missings
#############################################################################
#' Check and Adjust Missing Coding
#'
#' Functions to check if missings are coded and labeled correctly in a \code{GADSdat} object.
#'
#' \code{checkMissings()} compares value labels (\code{valLabels}) and missing tags (\code{missings}) of a \code{GADSdat} object and its
#' meta data information.
#' \code{checkMissingsByValues()} compares labeled values (\code{value}) and missing codes (\code{missings}) of a \code{GADSdat} object
#' and its meta data information.
#' Mismatches are reported and can be automatically adjusted. Note that all checks are only applied to the
#' meta data information, not the actual data. For detecting missing value labels, see \code{\link{checkMissingValLabels}}.
#'
#'@param GADSdat \code{GADSdat} object imported via \code{eatGADS}.
#'@param missingLabel Single regular expression indicating how missing labels are commonly named in the value labels.
#'@param missingValues Numeric vector of values which are commonly used for missing values.
#'@param addMissingCode If \code{TRUE}, missing codes are added according to  \code{missingLabel} or \code{missingValues}.
#'@param addMissingLabel If \code{TRUE}, \code{"generic missing"} is added according to occurrence of \code{"miss"} in \code{"missings"}. As often various value labels for missings are used, this argument should be used with great care.
#'
#'@return Returns a \code{GADSdat} object with - if specified - modified missing tags.
#'
#'@examples
#'# checkMissings
#' pisa2 <- changeValLabels(pisa, varName = "computer_age",
#'                         value = 5, valLabel = "missing: No computer use")
#'
#' pisa3 <- checkMissings(pisa2)
#'
#'# checkMissingsByValues
#' pisa4 <- changeValLabels(pisa, varName = "computer_age",
#'                         value = c(-49, -90, -99), valLabel = c("test1", "test2", "test3"))
#'
#' pisa5 <- checkMissingsByValues(pisa4, missingValues = -50:-99)
#'
#' @describeIn checkMissings compare missing tags and value labels
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
    # this is rarely what users want; in the future, this feature might be dropped from the function
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

#' @describeIn checkMissings compare missing tags and values in a certain range
#'@export
checkMissingsByValues <- function(GADSdat, missingValues = -50:-99, addMissingCode = TRUE) {
  UseMethod("checkMissingsByValues")
}

#'@export
checkMissingsByValues.GADSdat <- function(GADSdat, missingValues = -50:-99, addMissingCode = TRUE) {
  check_GADSdat(GADSdat)
  labels <- GADSdat$labels

  missCode_missing_rows <- which(labels$value %in% missingValues & (is.na(labels$missings) | labels$missings == "valid"))
  missCode_toomuch_rows <- which(labels$missings == "miss" & labels$value %in% missingValues)

  ## Which variables are affected, how many adjustments are performed
  if(length(missCode_missing_rows) > 0) {
    message("The following variables have values in the 'missingValues' range which are not coded as missing:\n",
            paste(unique(labels[missCode_missing_rows, "varName"]), collapse = ", "))
    if(identical(addMissingCode, TRUE)) labels <- insert_string(df = labels, rows = missCode_missing_rows, col = "missings", string = "miss")
  }

  if(length(missCode_toomuch_rows) > 0) {
    message("The following variables have values coded as missings which are outside of the specified 'missingValues' range:\n",
            paste(unique(labels[missCode_toomuch_rows, "varName"]), collapse = ", "))
    # currently no idea how the data could be appropriately adjusted automatically
  }

  GADSdat$labels <- labels
  GADSdat
}









