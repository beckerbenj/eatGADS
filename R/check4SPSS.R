####
#############################################################################
#' Check \code{SPSS} Compliance of Meta Data
#'
#' Function to check if variable names and labels, value labels and missing codes comply with \code{SPSS} requirements for meta data.
#'
#' The function measures the length of variable names (\code{"varNames_length"}, maximum of 64 characters)
#' variable labels (\code{"varLabels"}, maximum of 256 characters),
#' value labels (\code{"valLabels"}, maximum of 120 characters). Furthermore,
#' missing codes are counted (\code{"missings"}, maximum of three missing codes for character variables)
#' and special characters are flagged in variable names (\code{"varNames_special"}).
#' Check results are reported back on variable level, with the exception of \code{"valLabels"}, which is a list
#' with entries per violating variable.
#'
#'@param GADSdat \code{GADSdat} object imported via \code{eatGADS}.
#'
#'@return Returns a list with the entries \code{"varNames_special"}, \code{"varNames_length"},
#'\code{"varLabels"}, \code{"valLabels"} and \code{"missings"}.
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

  spclChar_varNams <- colnames(GADSdat$dat)[grep("[^[:alnum:]_\\$@#]", colnames(GADSdat$dat))]
  long_varNames <- unique(namesGADS(GADSdat)[nchar_4_spss(namesGADS(GADSdat)) > 64])
  long_varLabels <- unique(labels$varName[!is.na(labels$varLabel) & nchar_4_spss(labels$varLabel) > 256])

  long_valLabels <- unique(labels$varName[!is.na(labels$valLabel) & nchar_4_spss(labels$valLabel) > 120])
  if(length(long_valLabels) > 0 ){
    names(long_valLabels) <- long_valLabels
    long_valLabels <- lapply(long_valLabels, function(nam) {
      single_meta <- extractMeta(GADSdat, nam)
      single_long_valLabels <- single_meta[!is.na(single_meta$valLabel) & nchar_4_spss(single_meta$valLabel) > 120, "value"]
    })
  }

  chv <- sapply(GADSdat$dat, is.character)
  misInfo <- unique(labels[which(!is.na(labels$value) & labels$missings == "miss"), c("varName", "value", "valLabel", "missings")])
  many_missCodes <- unlist(lapply(namesGADS(GADSdat), function(v) {
    if(isTRUE(chv[v]) & length(misInfo$value[which(misInfo$varName==v)]) > 3) {
      return(v)
    }
    character()
  }))

  list(varNames_special = spclChar_varNams,
       varNames_length = long_varNames,
       varLabels = long_varLabels,
       valLabels = long_valLabels,
       missings = many_missCodes)
}


nchar_4_spss <- function(x) {
  long_str <- stringi::stri_escape_unicode(x)
  nchar(gsub("\\\\u00", "", long_str), type = "chars")
}

