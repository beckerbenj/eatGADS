####
#############################################################################
#' Assimilate value labels.
#'
#' Assimilate value labels of multiple variables as part of a \code{GADSdat} or \code{all_GADSdat} object.
#'
#' Assimilation can be performed using all existing value labels or a look up table containing at least all existing value labels.
#'
#'@param GADSdat \code{GADSdat} object imported via \code{eatGADS}.
#'@param varNames Character string of a variable name.
#'@param lookup Look up \code{data.frame}.
#'
#'@return Returns the \code{GADSdat} object with changed meta data and recoded values.
#'
#'@examples
#'# Change existing value labels
#' pisa2 <- changeValLabels(pisa, varName = "repeated",
#'                         value = c(1, 2),
#'                         valLabel = c("no grade repetition", "grade repitition"))
#'
#'# Add value label to unlabeled value
#' mtcars_g <- import_DF(mtcars)
#' mtcars_g2 <- changeValLabels(mtcars_g, varName = "cyl",
#'                              value = c(4, 6, 8),
#'                              valLabel = c("four", "six", "eight"))
#'
#'@export
assimilateValLabels <- function(GADSdat, varNames, lookup = NULL) {
  UseMethod("assimilateValLabels")
}
#'@export
assimilateValLabels.GADSdat <- function(GADSdat, varNames, lookup = NULL) {
  check_vars_in_GADSdat(GADSdat, vars = varNames)

  # extract GADSdat only including variables
  suppressMessages(fac_GADS <- extractVars(GADSdat, vars = varNames))

  # apply value labels
  fac_df <- extractData(fac_GADS, convertMiss = FALSE, convertLabels = "character")

  # (if lookup does not exist, create it from all variables)
  # (if lookup exists, check whether all values are mentioned in lookup)
  if(!is.null(lookup)) stop("Lookup argument is currently not supported.")

  # multiChar2fac on all resulting variables
  char_gads <- import_DF(fac_df)
  newVars_gads <- multiChar2fac(char_gads, vars = namesGADS(char_gads), var_suffix = "", label_suffix = "")

  # merge data and meta data into old GADSdat
  suppressMessages(GADSdat_removed <- removeVars(GADSdat, vars = varNames))
  #browser()
  # cbind Method, orderlike
  GADSdat_unorderd <- cbind(GADSdat_removed, newVars_gads)
  GADSdat_out <- orderLike(GADSdat_unorderd, newOrder = namesGADS(GADSdat))

  # restore missing value labels?
  # maybe using recodeGADSdat and the first variable in varNames?

  # is lookup a specific lookup or just a list of character entries

  # restore specific value label order?

  GADSdat_out
}

#'@export
changeValLabels.all_GADSdat <- function(GADSdat, varName, value, valLabel) {
  stop("This method has not been implemented yet")
}



