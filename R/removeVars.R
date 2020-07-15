#### Remove variables from a GADSdat
#############################################################################
#' Extract or remove variables from a \code{GADSdat}.
#'
#' Extract or remove variables and their meta data from a \code{GADSdat} object.
#'
#' Both functions simply perform the variable removal or extraction from the underlying \code{data.frame}
#' in the \code{GADSdat} object followed by calling \code{\link{updateMeta}}.
#'
#'@param GADSdat \code{GADSdat} object.
#'@param vars A character vector containing the variables names in the \code{GADSdat}.
#'
#'@return Returns a \code{GADSdat} object.
#'
#'@examples
#'# Example data set
#'#to be done
#'
#'@export
extractVars <- function(GADSdat, vars) {
  UseMethod("extractVars")
}
#'@export
extractVars.GADSdat <- function(GADSdat, vars) {
  check_GADSdat(GADSdat)
  if(!all(vars %in% namesGADS(GADSdat))) stop("All 'vars' have to be variables in the GADSdat.")

  new_dat <- GADSdat$dat[, names(GADSdat$dat) %in% vars, drop = FALSE]
  updateMeta(GADSdat, newDat = new_dat)
}

#' @export
#' @rdname extractVars
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
