####
#############################################################################
#' Reorder a single variable in a \code{GADSdat}.
#'
#' Deprecated. Please use \code{\link{relocateVariable}} instead.
#'
#'@param GADSdat A \code{GADSdat} object.
#'@param var Character string of the variable name which should be sorted.
#'@param after Character string of the variable name after which \code{var} should be inserted. If \code{NULL}, \code{var} is inserted at the beginning of the
#'\code{GADSdat}.
#'
#'@export
insertVariable <- function(GADSdat, var, after = NULL) {
  stop("'insertVariable()' has been deprecated, please use 'relocateVariable()' instead.")
}



