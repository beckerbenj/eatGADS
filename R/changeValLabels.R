#### Change value label
#############################################################################
#' Change value labels.
#'
#' Change value labels of a variable as part of a \code{GADSdat} or \code{all_GADSdat} object.
#'
#' Applied to a \code{GADSdat} or \code{all_GADSdat} object, this function is a wrapper of \code{\link{getChangeMeta}} and
#' \code{\link{applyChangeMeta}}.
#'
#'@param GADSdat \code{GADSdat} object imported via \code{eatGADS}.
#'@param varName Character string of a variable name.
#'@param value Numeric values.
#'@param valLabel Character string of the new value labels.
#'
#'@return Returns the \code{GADSdat} object with changed meta data.
#'
#'@examples
#'# Example data set
#'#to be done
#'
#'@export
changeValLabels <- function(GADSdat, varName, value, valLabel) {
  UseMethod("changeValLabels")
}
#'@export
changeValLabels.GADSdat <- function(GADSdat, varName, value, valLabel) {
  checkValLabelInput(varName = varName, value = value, valLabel = valLabel, labels = GADSdat$labels)
  changeTable <- getChangeMeta(GADSdat, level = "value")
  for(i in seq_along(value)) {
    changeTable[changeTable$varName == varName & changeTable$value == value[i], "valLabel_new"] <- valLabel[i]
  }
  applyChangeMeta(GADSdat, changeTable = changeTable)
}

#'@export
changeValLabels.all_GADSdat <- function(GADSdat, varName, value, valLabel) {
  stop("This method has not been implemented yet")
}

checkValLabelInput <- function(varName, value, valLabel, labels) {
  if(!is.character(varName) || !length(varName) == 1) stop("varName is not a character vector of length 1.")
  if(!varName %in% labels$varName) stop("varName is not a variable name in the GADSdat.")
  if(length(value) != length(valLabel)) stop("value and valLabel are not of identical length.", call. = FALSE)
  if(!all(value %in% labels[labels$varName == varName, "value"])) stop("values are not existing values for the variable.", call. = FALSE)
  return()
}
