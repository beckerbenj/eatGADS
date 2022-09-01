####
#############################################################################
#' Create a variable.
#'
#' Create an empty variable as part of a \code{GADSdat} object.
#'
#'@param GADSdat \code{GADSdat} object imported via \code{eatGADS}.
#'@param varName Name of the variable to be cloned.
#'
#'@return Returns a \code{GADSdat}.
#'
#'@examples
#' # create a new variable
#' pisa_new <- createVariable(pisa, varName = "new_variable")
#'
#'@export
createVariable <- function(GADSdat, varName) {
  UseMethod("createVariable")
}
#'@export
createVariable.GADSdat <- function(GADSdat, varName) {
  check_GADSdat(GADSdat)
  if(varName %in% namesGADS(GADSdat)) stop("'",  varName, "' is already an existing variable in the 'GADSdat'.")

  dat_only <- GADSdat$dat
  dat_only[[varName]] <- NA

  suppressMessages(GADSdat2 <- updateMeta(GADSdat, newDat = dat_only))
  GADSdat2
}
