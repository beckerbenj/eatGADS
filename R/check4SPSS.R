####
#############################################################################
#' Check \code{SPSS} Compliance of Meta Data
#'
#' Function to check if variable labels, value labels and missing codes comply with \code{SPSS} requirements for meta data.
#'
#' The function measures the length of variable labels (\code{"varLabels"}, maximum of 256 characters),
#' value labels (\code{"valLabels"}, maximum of 120 characters) and
#' missing codes (\code{"missings"}, maximum of three missing codes for character variables)
#' of a \code{GADSdat} object and reports back violations on variable level.
#'
#'@param GADSdat \code{GADSdat} object imported via \code{eatGADS}.
#'
#'@return Returns a list with the entries \code{"varLabels"}, \code{"valLabels"} and \code{"missings"}.
#'
#'@examples
#'# Change example data set (create a violating label)
#' pisa2 <- changeVarLabels(pisa, varName = "computer_age",
#'                         varLabel = paste(rep("3", 125), collapse = ""))
#'
#' check4SPSS(pisa2)
#'
#'@export
check4SPSS <- function(GADSdat) {
  UseMethod("check4SPSS")
}

#'@export
check4SPSS.GADSdat <- function(GADSdat) {
  check_GADSdat(GADSdat)
  labels <- GADSdat$labels

  #browser()
  long_varLabels <- unique(labels$varName[!is.na(labels$varLabel) & nchar_4_spss(labels$varLabel) > 256])
  long_valLabels <- unique(labels$varName[!is.na(labels$valLabel) & nchar_4_spss(labels$valLabel) > 120])
  many_missCodes <- "Functionality not yet implemented."

  list(varLabels = long_varLabels,
       valLabels = long_valLabels,
       missings = many_missCodes)
}


nchar_4_spss <- function(x) {
  long_str <- stringi::stri_escape_unicode(x)
  nchar(gsub("\\\\u00", "", long_str), type = "chars")
}

