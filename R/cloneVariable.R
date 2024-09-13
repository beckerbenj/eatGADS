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
#'@param label_suffix Suffix added to variable label for the newly created variable in the \code{GADSdat}.
#'@param checkVarName Logical. Should \code{new_varName} be checked by \code{\link{checkVarNames}}?
#'
#'@return Returns a \code{GADSdat}.
#'
#'@examples
#' # duplicate the variable schtype
#' pisa_new <- cloneVariable(pisa, varName = "schtype", new_varName = "schtype_new")
#'
#'@export
cloneVariable <- function(GADSdat, varName, new_varName, label_suffix = "", checkVarName = TRUE) {
  UseMethod("cloneVariable")
}
#'@export
cloneVariable.GADSdat <- function(GADSdat, varName, new_varName, label_suffix = "", checkVarName = TRUE) {
  check_GADSdat(GADSdat)
  check_vars_in_GADSdat(GADSdat, vars = varName)
  check_logicalArgument(checkVarName, argName = checkVarName)
  if(new_varName %in% namesGADS(GADSdat)) {
    stop("'",  new_varName, "' is already an existing variable in the 'GADSdat'.")
  }
  ##<Note>
  ## alternativley, cloneVariable() could be extended to accept new_varName == varName. The only action
  ## performed would be to append label_suffix. This would allow using the function more straightforward
  ## in functions such as autoRecode(). As of now, only autoRecode() is using cloneVariable(), therefore
  ## such an extension is postponed.
  ##</Note>

  dat_only <- GADSdat$dat
  dat_only[[new_varName]] <- dat_only[[varName]]

  suppressMessages(GADSdat2 <- updateMeta(GADSdat, newDat = dat_only, checkVarNames = checkVarName))
  if(checkVarName) {
    new_varName <- checkVarNames(new_varName)
  }
  GADSdat3 <- reuseMeta(GADSdat2, varName = new_varName, other_GADSdat = GADSdat, other_varName = varName)
  GADSdat4 <- append_varLabel(GADSdat3, varName = new_varName, label_suffix = label_suffix)

  GADSdat4
}
