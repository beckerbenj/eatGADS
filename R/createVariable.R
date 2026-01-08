####
#############################################################################
#' Create a variable.
#'
#' Create an empty variable as part of a \code{GADSdat} object.
#'
#'@param GADSdat \code{GADSdat} object imported via \code{eatGADS}.
#'@param varName Name of the variable to be cloned.
#'@param checkVarName Logical. Should \code{varName} be checked by \code{\link{checkVarNames}}?
#'
#'@return Returns a \code{GADSdat}.
#'
#'@examples
#' # create a new variable
#' pisa_new <- createVariable(pisa, varName = "new_variable")
#'
#'@export
createVariable <- function(GADSdat, varName, checkVarName = TRUE) {
  UseMethod("createVariable")
}
#'@export
createVariable.GADSdat <- function(GADSdat, varName, checkVarName = TRUE) {
  check_GADSdat(GADSdat)
  check_logicalArgument(checkVarName)
  if(varName %in% namesGADS(GADSdat)) {
    stop("'",  varName, "' is already an existing variable in the 'GADSdat'.")
  }
  if(checkVarName){
    varName <- checkVarNames(varName)
  }

  dat_only <- GADSdat$dat
  dat_only[[varName]] <- NA

  # perform checkVarNames in this function, as updateMeta provides more messages than desired
  suppressMessages(GADSdat2 <- updateMeta(GADSdat, newDat = dat_only, checkVarNames = FALSE))
  GADSdat2
}
