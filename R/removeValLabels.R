#### Remove value label
#############################################################################
#' Remove value labels and missing tags.
#'
#' Remove meta data for specific values (\code{value}) of a single variable (\code{varName}).
#' This includes value labels and missings tags.
#'
#' If the argument \code{valLabel} is provided, the function checks for \code{value} and \code{valLabel} pairs in the
#' meta data that match both arguments.
#'
#'@param GADSdat \code{GADSdat} object imported via \code{eatGADS}.
#'@param varName Character string of a variable name.
#'@param value Numeric values.
#'@param valLabel [optional] Regular expressions in the value labels corresponding to \code{value}.
#'
#'@return Returns the \code{GADSdat} object with changed meta data.
#'
#'@examples
#'# Remove a label based on value
#'extractMeta(pisa, "schtype")
#'pisa2 <- removeValLabels(pisa, varName = "schtype", value = 1)
#'extractMeta(pisa2, "schtype")
#'
#'# Remove multiple labels based on value
#'extractMeta(pisa, "schtype")
#'pisa3 <- removeValLabels(pisa, varName = "schtype", value = 1:3)
#'extractMeta(pisa3, "schtype")
#'
#'# Remove multiple labels based on value - valLabel combination
#'extractMeta(pisa, "schtype")
#'pisa4 <- removeValLabels(pisa, varName = "schtype",
#'                         value = 1:3, valLabel = c("Gymnasium", "other", "several courses"))
#'extractMeta(pisa4, "schtype")
#'
#'@export
removeValLabels <- function(GADSdat, varName, value, valLabel = NULL) {
  UseMethod("removeValLabels")
}
#'@export
removeValLabels.GADSdat <- function(GADSdat, varName, value, valLabel = NULL) {
  if(!is.character(varName) || !length(varName) == 1) {
    stop("'varName' is not a character vector of length 1.")
  }
  check_vars_in_GADSdat(GADSdat, vars = varName, argName = "varName")

  all_rows <- which(GADSdat$labels$varName == varName)
  remove_rows <- which(GADSdat$labels$varName == varName & GADSdat$labels$value %in% value)

  if(!is.null(valLabel)) {
    if(length(value) != length(valLabel)) stop("'value' and 'valLabel' need to be of identical length.")
    remove_rows <- integer(0)
    for(i in seq_along(value)) {
      remove_rows <- c(remove_rows, which(GADSdat$labels$varName == varName &
                                            GADSdat$labels$value == value[i] &
                                            grepl(valLabel[i], GADSdat$labels$valLabel)))
    }
  }

  if(length(remove_rows) == 0) {
    warning("None of 'value' are labeled 'values'. Meta data are unchanged.")
    return(GADSdat)
  }
  if(length(all_rows) > length(remove_rows)) {
    GADSdat$labels <- GADSdat$labels[-remove_rows, ]
  }
  if(length(all_rows) == length(remove_rows)) {
    if(length(remove_rows) > 1) {
      remove_rows2 <- remove_rows[-1]
      GADSdat$labels <- GADSdat$labels[-remove_rows2, ]
    }
    GADSdat$labels[remove_rows[1], c("value", "valLabel", "missings")] <- NA
    GADSdat$labels[remove_rows[1], c("labeled")] <- "no"
  }
  GADSdat
}

#'@export
removeValLabels.all_GADSdat <- function(GADSdat, varName, value, valLabel = NULL) {
  stop("This method has not been implemented yet")
}



