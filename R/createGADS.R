
#### Create GADS using eatDB
#############################################################################
#' Create an \code{eatGADS} data base.
#'
#' Creates a relational data base containing hierarchically stored data with meta information (e.g. value and variable labels).
#'
#' Uses \code{\link[eatDB]{createDB}} from the \code{eatDB} package to create a relational data base. For details on how to define
#' keys see the documentation of \code{\link[eatDB]{createDB}}.
#'
#'@param allList An object created via \code{\link{mergeLabels}}.
#'@param pkList List of primary keys.
#'@param fkList List of foreign keys.
#'@param filePath Path to the db file to write (including name); has to end on '.db'.
#'
#'@return Creates a data base in the given path, returns \code{NULL}.
#'
#'@examples
#'# see createDB vignette
#'
#'@export
createGADS <- function(allList, pkList, fkList, filePath) {
  UseMethod("createGADS")
}

#'@export
createGADS.all_GADSdat <- function(allList, pkList, fkList, filePath) {
  eatDB::createDB(dfList = allList$datList, pkList = pkList, fkList = fkList, metaData = allList$allLabels, filePath = filePath)
}
#'@export
createGADS.GADSdat <- function(allList, pkList, fkList, filePath) {
  allList_dat <- list(allList$dat)
  names(allList_dat) <- names(pkList)
  eatDB::createDB(dfList = allList_dat, pkList = pkList, metaData = allList$labels, filePath = filePath)
}
