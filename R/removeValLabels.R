#### Remove value label
#############################################################################
#' Remove value labels.
#'
#' Remove value labels of a variable as part of a \code{GADSdat} or \code{all_GADSdat} object.
#'
#'@param GADSdat \code{GADSdat} object imported via \code{eatGADS}.
#'@param varName Character string of a variable name.
#'@param value Numeric values.
#'
#'@return Returns the \code{GADSdat} object with changed meta data.
#'
#'@examples
#'# Example data set
#'#to be done
#'
#'@export
removeValLabels <- function(GADSdat, varName, value) {
  UseMethod("removeValLabels")
}
#'@export
removeValLabels.GADSdat <- function(GADSdat, varName, value) {
  checkValRemoveInput(varName = varName, value = value, labels = GADSdat$labels)

  all_rows <- which(GADSdat$labels$varName == varName)
  remove_rows <- which(GADSdat$labels$varName == varName & GADSdat$labels$value %in% value)

  if(length(remove_rows) == 0) {
    warning("None of 'value' are labeled 'values'. Meta data are unchanged.")
    return(GADSdat)
  }
  if(length(all_rows) > length(remove_rows)) {
    GADSdat$labels <- GADSdat$labels[-remove_rows, ]
  }
  if(length(all_rows) == length(remove_rows)) {
    remove_rows2 <- remove_rows[-1]
    GADSdat$labels <- GADSdat$labels[-remove_rows2, ]
    GADSdat$labels[remove_rows[1], c("value", "valLabel", "missings")] <- NA
    GADSdat$labels[remove_rows[1], c("labeled")] <- "no"
  }
  GADSdat
}

#'@export
removeValLabels.all_GADSdat <- function(GADSdat, varName, value) {
  stop("This method has not been implemented yet")
}

checkValRemoveInput <- function(varName, value, labels) {
  if(!is.character(varName) || !length(varName) == 1) stop("'varName' is not a character vector of length 1.")
  if(!varName %in% labels$varName) stop("'varName' is not a variable name in the GADSdat.")
  return()
}


