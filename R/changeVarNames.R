
#### Change Variable names
#############################################################################
#' Change Variable Names.
#'
#' Change variable names of a \code{GADSdat} or \code{all_GADSdat} object.
#'
#' Applied to a \code{GADSdat} or \code{all_GADSdat} object, this function is a wrapper of \code{\link{getChangeMeta}} and
#' \code{\link{applyChangeMeta}}
#'
#'@param GADSdat \code{GADSdat} object imported via \code{eatGADS}.
#'@param oldNames Vector containing the old variable names.
#'@param newNames Vector containing the new variable names, in identical order as \code{oldNames}.
#'
#'@return Returns the \code{GADSdat} object with changed variable names.
#'
#'@examples
#'# Change multiple variable name
#' pisa2 <- changeVarNames(pisa, oldNames = c("idstud", "idschool"),
#'                         newNames = c("IDstud", "IDschool"))
#'
#'@export
changeVarNames <- function(GADSdat, oldNames, newNames) {
  UseMethod("changeVarNames")
}
#### Note: changeVarNames.all_GADSdat could be blueprint for other changes on all_GADSdat level!
#'@export
changeVarNames.all_GADSdat <- function(GADSdat, oldNames, newNames) {
  changeDF <- data.frame(oldNames = oldNames, newNames = newNames, stringsAsFactors = FALSE)
  out <- list()
  for(i in names(GADSdat[["datList"]])) {
    GADSdat_single <- extractGADSdat(GADSdat, name = i)
    changeDF_single <- changeDF[changeDF$oldNames %in% names(GADSdat[["datList"]][[i]]), ]
    out[[i]] <- changeVarNames(GADSdat = GADSdat_single, oldNames = changeDF_single[["oldNames"]], newNames = changeDF_single[["newNames"]])
  }
  do.call(mergeLabels, out)
}
#'@export
changeVarNames.GADSdat <- function(GADSdat, oldNames, newNames) {
  checkNamesVectors(oldNames = oldNames, newNames = newNames, dat = GADSdat[["dat"]])
  changeTable <- getChangeMeta(GADSdat, level = "variable")
  for(i in seq_along(oldNames)) {
    changeTable[changeTable$varName == oldNames[i], "varName_new"] <- newNames[i]
  }
  applyChangeMeta(GADSdat, changeTable = changeTable)
}


checkNamesVectors <- function(oldNames, newNames, dat) {
  if(length(oldNames) != length(newNames)) stop("oldNames and newNames are not of identical length.", call. = FALSE)
  if(!(is.character(oldNames) && is.character(newNames))) stop("oldNames and newNames are not character vectors.", call. = FALSE)
  if(any(!oldNames %in% names(dat))) stop("varName in oldNames is not a real variable name.", call. = FALSE)
  return()
}
