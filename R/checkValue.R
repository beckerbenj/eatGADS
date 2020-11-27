#### Check existence of a value
#############################################################################
#' Check for a specific value
#'
#' Function to look for occurrences of a specific value in a \code{GADSdat}.
#'
#' The function checks occurrences of a specific value in all variables in the \code{GADSdat} and outputs a vector
#' containing the count of occurrences for all variables in which the value occurs.
#'
#'@param GADSdat \code{GADSdat} object imported via \code{eatGADS}.
#'@param value Single string indicating how missing labels are commonly named in the value labels.
#'
#'@return A named integer.
#'
#'@examples
#'checkValue(pisa, 99)
#'
#'@export
checkValue <- function(GADSdat, value) {
  UseMethod("checkValue")
}

#'@export
checkValue.GADSdat <- function(GADSdat, value) {
  check_GADSdat(GADSdat)
  if(!length(value) == 1) stop("'value' needs to be of length 1.")

  name_indicator <- logical(length(namesGADS(GADSdat)))
  names(name_indicator) <- namesGADS(GADSdat)

  for(nam in namesGADS(GADSdat)) {
    name_indicator[nam] <- length(which(GADSdat[["dat"]][, nam] == value))
  }

  name_indicator_out <- name_indicator[name_indicator > 0]
  if(length(name_indicator_out) == 0) return(integer(0))
  name_indicator_out
}
