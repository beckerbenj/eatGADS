
#### As numeric
#############################################################################
#' Transform string to numeric.
#'
#' Transform a string variable within a \code{GADSdat} or \code{all_GADSdat} object to a numeric variable.
#'
#' Applied to a \code{GADSdat} or \code{all_GADSdat} object, this function uses \code{\link[eatTools]{asNumericIfPossible}} to
#' change the variable class and changes the \code{format} column in the meta data.
#'
#'@param GADSdat \code{GADSdat} object imported via \code{eatGADS}.
#'@param varName Character string of a variable name.
#'
#'@return Returns the \code{GADSdat} object with with the changed variable.
#'
#'
#'@export
stringAsNumeric <- function(GADSdat, varName) {
  UseMethod("stringAsNumeric")
}
#'@export
stringAsNumeric.GADSdat <- function(GADSdat, varName) {
  check_GADSdat(GADSdat)
  if(!varName %in% namesGADS(GADSdat)) stop("varName is not a variable in the GADSdat.")

  GADSdat$dat[[varName]] <- eatTools::catch_asNumericIfPossible(x = GADSdat$dat[[varName]], warn = paste("Some or all values for ", varName,
                                                    " cannot be coerced to numeric and are therefore changed to NA. \n", sep = ""),
                                                    maintain.factor.scores = TRUE, force.string = TRUE, transform.factors = TRUE)
  GADSdat_out <- changeSPSSformat(GADSdat, varName = varName, format = "F10")

  check_var_type(GADSdat_out)
  GADSdat_out
}


