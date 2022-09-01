####
#############################################################################
#' Clone a variable.
#'
#' Clone a variable as part of a \code{GADSdat} object.
#'
#' The variable is simply duplicated and assigned a new name.
#'
#'@param GADSdat \code{GADSdat} object imported via \code{eatGADS}.
#'@param varName Name of the variable to be cloned.
#'@param new_varName Name of the new variable.
#'
#'@return Returns a \code{GADSdat}.
#'
#'@examples
#' # duplicate the variable schtype
#' pisa_new <- cloneVariable(pisa, varName = "schtype", new_varName = "schtype_new")
#'
#'@export
cloneVariable <- function(GADSdat, varName, new_varName) {
  UseMethod("cloneVariable")
}
#'@export
cloneVariable.GADSdat <- function(GADSdat, varName, new_varName) {
  check_GADSdat(GADSdat)
  check_vars_in_GADSdat(GADSdat, vars = varName)
  if(new_varName %in% namesGADS(GADSdat)) stop("'",  new_varName, "' is already an existing variable in the 'GADSdat'.")

  dat_only <- GADSdat$dat
  dat_only[[new_varName]] <- dat_only[[varName]]

  suppressMessages(GADSdat2 <- updateMeta(GADSdat, newDat = dat_only))
  GADSdat3 <- reuseMeta(GADSdat2, varName = new_varName, other_GADSdat = GADSdat, other_varName = varName)
  GADSdat3
}
