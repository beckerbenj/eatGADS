#### Check existence of a value
#############################################################################
#' Check for a specific value
#'
#' Function to look for occurrences of a specific value in a \code{GADSdat}.
#'
#' The function checks occurrences of a specific value in a set of variables (default: all variables) in the \code{GADSdat} and outputs a vector
#' containing the count of occurrences for all variables in which the value occurs.
#'
#'@param GADSdat \code{GADSdat} object imported via \code{eatGADS}.
#'@param vars Character vector with the variable names to which \code{checkValue} should be applied.
#'@param value Single string indicating how missing labels are commonly named in the value labels.
#'
#'@return A named integer.
#'
#'@examples
#'# for all variables in the data
#'checkValue(pisa, value = 99)
#'
#'# only for specific variables in the data
#'checkValue(pisa, vars = c("idschool", "g8g9"), value = 99)
#'
#'@export
checkValue <- function(GADSdat, vars = namesGADS(GADSdat), value) {
  UseMethod("checkValue")
}

#'@export
checkValue.GADSdat <- function(GADSdat, vars = namesGADS(GADSdat), value) {
  check_GADSdat(GADSdat)
  if(!length(value) == 1) stop("'value' needs to be of length 1.")
  if(!is.character(vars) || length(vars) < 1) stop("'vars' needs to be a character of at least length 1.")
  check_vars_in_GADSdat(GADSdat, vars = vars)

  name_indicator <- logical(length(vars))
  names(name_indicator) <- vars

  for(nam in vars) {
    name_indicator[nam] <- length(which(GADSdat[["dat"]][, nam] == value))
  }

  name_indicator_out <- name_indicator[name_indicator > 0]
  if(length(name_indicator_out) == 0) return(integer(0))
  name_indicator_out
}
