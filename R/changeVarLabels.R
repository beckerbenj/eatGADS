#### Change variable label
#############################################################################
#' Change the variable label.
#'
#' Change the variable label of a variable as part of a \code{GADSdat} or \code{all_GADSdat} object.
#'
#' Applied to a \code{GADSdat} or \code{all_GADSdat} object, this function is a wrapper of \code{\link{getChangeMeta}} and
#' \code{\link{applyChangeMeta}}.
#'
#'@param GADSdat \code{GADSdat} object imported via \code{eatGADS}.
#'@param varName Character string of variable names.
#'@param varLabel Character string of the new variable labels.
#'
#'@return Returns the \code{GADSdat} object with changed meta data.
#'
#'@examples
#'# Change one variable label
#' pisa2 <- changeVarLabels(pisa, varName = "repeated",
#'                         varLabel = c("Has a grade been repeated?"))
#'
#'@export
changeVarLabels <- function(GADSdat, varName, varLabel) {
  UseMethod("changeVarLabels")
}
#'@export
changeVarLabels.GADSdat <- function(GADSdat, varName, varLabel) {
  checkVarLabelVector(varName = varName, varLabel = varLabel, dat = GADSdat$dat)
  changeTable <- getChangeMeta(GADSdat, level = "variable")
  for(i in seq_along(varName)) {
    changeTable[changeTable$varName == varName[i], "varLabel_new"] <- varLabel[i]
  }
  applyChangeMeta(GADSdat, changeTable = changeTable)
}

#'@export
changeVarLabels.all_GADSdat <- function(GADSdat, varName, varLabel) {
  stop("This method has not been implemented yet")
}

checkVarLabelVector <- function(varName, varLabel, dat) {
  if(!is.character(varName) || !is.character(varLabel)) stop("varName and varLabel are not character vectors.")
  if(length(varName) != length(varLabel)) stop("varName and varLabel are not of identical length.", call. = FALSE)
  if(!all(varName %in% names(dat))) stop("varName is not a real variable name.", call. = FALSE)
  return()
}
