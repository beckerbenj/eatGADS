#' Split a character variable.
#'
#' Split a character variable which is part of a \code{GADSdat} object into multiple character variables.
#'
#'@param GADSdat \code{GADSdat} object imported via \code{eatGADS}.
#'@param varName Character string of a variable name.
#'@param pattern Character string of the splitting pattern.
#'
#'@return Returns the \code{GADSdat} object with with the changed variable.
#'
#'
#'@export
stringSplit <- function(GADSdat, varName, pattern) {
  UseMethod("stringSplit")
}
#'@export
stringSplit.GADSdat <- function(GADSdat, varName, pattern) {
  check_GADSdat(GADSdat)
  check_vars_in_GADSdat(GADSdat, vars = varName, argName = "varName")

  ## split

  ## update meta

  ## reuse meta

  GADSdat_out
}


