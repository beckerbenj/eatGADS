
#### Check Names
#############################################################################
#' Check names for \code{SQLite} conventions.
#'
#' Applies variable names changes to \code{GADSdat} or \code{all_GADSdat} objects.
#'
#' Illegal names in a \code{SQLite} data base include \code{SQLite} keywords (see \code{\link[eatDB]{sqlite_keywords}}) and names
#' with a \code{"."} in it.
#'
#'@param GADSdat \code{GADSdat} or \code{all_GADSdat} object imported via eatGADS.
#'
#'@return Returns the original object with updated variable names.
#'
#'@examples
#'# Change example data set (create an invalid variable name)
#' pisa2 <- changeVarNames(pisa, oldNames = "computer_age",
#'                         newNames = "computer.age")
#'
#' pisa3 <- checkVarNames(pisa2)
#'
#'@export
checkVarNames <- function(GADSdat) {
  UseMethod("checkVarNames")
}
#'@export
checkVarNames.GADSdat <- function(GADSdat) {
  check_GADSdat(GADSdat)
  GADSdat[["labels"]][, "varName"] <- sapply(GADSdat[["labels"]][, "varName"], transf_names)
  names(GADSdat[["dat"]]) <- sapply(names(GADSdat[["dat"]]), transf_names)
  GADSdat
}
#'@export
checkVarNames.all_GADSdat <- function(GADSdat) {
  check_all_GADSdat(GADSdat)
  GADSdat[["allLabels"]][, "varName"] <- sapply(GADSdat[["allLabels"]][, "varName"], transf_names)
  GADSdat[["datList"]] <- lapply(GADSdat[["datList"]], function(df) {
    names(df) <- sapply(names(df), transf_names)
    df
  })
  GADSdat
}

