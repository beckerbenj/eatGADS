
#' Change variable labels.
#'
#' Change variable labels of one or multiple variables as part of a \code{GADSdat} object.
#'
#' Applied to a \code{GADSdat} or \code{all_GADSdat} object, this function is a wrapper
#' of \code{\link{getChangeMeta}} and \code{\link{applyChangeMeta}}.
#'
#'@param GADSdat \code{GADSdat} object imported via \code{eatGADS}.
#'@param varName Character vector of variable names.
#'@param varLabel Character vector of the new variable labels.
#'
#'@return Returns the \code{GADSdat} object with changed meta data.
#'
#'@examples
#' # Change one variable label
#' pisa2 <- changeVarLabels(pisa, varName = "repeated",
#'                          varLabel = c("Has a grade been repeated?"))
#'
#' # Change multiple variable labels
#' pisa2 <- changeVarLabels(pisa, varName = c("repeated", "gender"),
#'                          varLabel = c("Has a grade been repeated?",
#'                                       "Student gender"))
#'
#'@export
changeVarLabels <- function(GADSdat, varName, varLabel) {
  UseMethod("changeVarLabels")
}
#'@export
changeVarLabels.GADSdat <- function(GADSdat, varName, varLabel) {
  check_GADSdat(GADSdat)
  if(!is.character(varName) || !is.character(varLabel)) {
    stop("varName and varLabel are not character vectors.")
  }
  if(length(varName) != length(varLabel)) {
    stop("varName and varLabel are not of identical length.", call. = FALSE)
  }
  check_vars_in_GADSdat(GADSdat, vars = varName, argName = "varName")

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

