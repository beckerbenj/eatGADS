
#############################################################################
#' Set variables to \code{NA}.
#'
#' Set all values within one or multiple variables to \code{NA}.
#'
#'@param GADSdat A \code{GADSdat} object.
#'@param vars Character vector of variable names which should be set to \code{NA}.
#'
#'@return Returns the recoded \code{GADSdat}.
#'
#'@examples
#' # empty multiple variables
#' pisa2 <- emptyTheseVariables(pisa, vars = c("idstud", "idschool"))
#'
#'
#'@export
emptyTheseVariables <- function(GADSdat, vars) {
  UseMethod("emptyTheseVariables")
}

#'@export
emptyTheseVariables.GADSdat <- function(GADSdat, vars) {
  check_GADSdat(GADSdat)
  if(!is.character(vars) || length(vars) < 1) stop("'vars' needs to be character vector of at least length 1.")
  check_vars_in_GADSdat(GADSdat, vars = vars)

  for(nam in vars) {
    GADSdat$dat[[nam]][] <- NA
  }
  GADSdat
}
