#### Remove variables from a GADSdat
#############################################################################
#' Remove variables from a GADSdat.
#'
#' Remove variables and their meta data from a \code{GADSdat} object.
#'
#' Wraps removing the variable from the data.frame in the \code{GADSdat} object and \code{\link{updateMeta}}.
#'
#'@param GADSdat \code{GADSdat} object.
#'@param vars A character vector containing the variables to be removed.
#'
#'@return Returns a GADSdat object.
#'
#'@examples
#'# Example data set
#'#to be done
#'
#'@export
removeVars <- function(GADSdat, vars) {
  UseMethod("removeVars")
}
#'@export
removeVars.GADSdat <- function(GADSdat, vars) {
  check_GADSdat(GADSdat)
  if(!all(vars %in% namesGADS(GADSdat))) stop("All 'vars' have to be variables in the GADSdat.")

  new_dat <- GADSdat$dat[, !names(GADSdat$dat) %in% vars, drop = FALSE]
  updateMeta(GADSdat, newDat = new_dat)
}
