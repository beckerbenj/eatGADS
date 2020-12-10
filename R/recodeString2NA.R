
#### Empty strings to NA
#############################################################################
#' Recode a string to \code{NA}.
#'
#' Deprecated, use \code{\link{recode2NA}} instead..
#'
#'@param GADSdat A \code{GADSdat} object.
#'@param recodeVars Character vector of variable names which should be recoded.
#'@param string Which string should be recoded to \code{NA}?
#'
#'@return Returns the recoded \code{GADSdat}.
#'
#'
#'@export
recodeString2NA <- function(GADSdat, recodeVars = namesGADS(GADSdat), string = "") {
  stop("This function is deprecated. Use recode2NA() instead.")
}

