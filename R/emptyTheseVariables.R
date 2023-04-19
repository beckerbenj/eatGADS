
#############################################################################
#' Set variables to \code{NA}.
#'
#' Set all values within one or multiple variables to \code{NA}.
#'
#'@param GADSdat A \code{GADSdat} object.
#'@param vars Character vector of variable names which should be set to \code{NA}.
#'@param label_suffix Suffix added to variable labels for the affected variables in the \code{GADSdat}.
#'
#'@return Returns the recoded \code{GADSdat}.
#'
#'@examples
#' # empty multiple variables
#' pisa2 <- emptyTheseVariables(pisa, vars = c("idstud", "idschool"))
#'@export
emptyTheseVariables <- function(GADSdat, vars, label_suffix = "") {
  UseMethod("emptyTheseVariables")
}

#'@export
emptyTheseVariables.GADSdat <- function(GADSdat, vars, label_suffix = "") {
  check_GADSdat(GADSdat)
  if(!is.character(vars) || length(vars) < 1) {
    stop("'vars' needs to be character vector of at least length 1.")
  }
  check_vars_in_GADSdat(GADSdat, vars = vars)

  for(nam in vars) {
    GADSdat$dat[[nam]][] <- NA
    GADSdat <- append_varLabel(GADSdat, varName = nam, label_suffix = label_suffix)
  }

  GADSdat
}
