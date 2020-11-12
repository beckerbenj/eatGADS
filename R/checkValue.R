#### Check existence of a value
#############################################################################
#' Check for a specific value
#'
#' Function to look for occurrences of a specific value in a \code{GADSdat}.
#'
#' The function checks occurrences of a specific value in all variables in the \code{GADSdat} and outputs a message
#' containing a list of variables in which the value occurs.
#'
#'@param GADSdat \code{GADSdat} object imported via \code{eatGADS}.
#'@param value Single string indicating how missing labels are commonly named in the value labels.
#'
#'@return Returns \code{NULL}.
#'
#'@examples
#'# Example data set
#'#to be done
#'
#'@export
checkValue <- function(GADSdat, value) {
  UseMethod("checkValue")
}

#'@export
checkValue.GADSdat <- function(GADSdat, value) {
  check_GADSdat(GADSdat)

  name_indicator <- logical(length(namesGADS(GADSdat)))
  names(name_indicator) <- namesGADS(GADSdat)
  for(nam in namesGADS(GADSdat)) {
    name_indicator[nam] <- value %in% GADSdat[["dat"]][, nam]
  }

  if(any(name_indicator)) {
    message("The following variables have occurences of 'value' in the GADSdat:\n",
            paste(names(name_indicator)[name_indicator], collapse = ", "))
  }

  return(NULL)
}
