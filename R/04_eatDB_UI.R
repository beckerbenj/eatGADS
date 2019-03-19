
#### Create GADS using eatDB
#############################################################################
#' Create GADS data base.
#'
#' Creates a relational data base.
#'
#' Uses createDB from the eatDB package to create a relational data base. For details on how to define keys see the documentation of createDB.
#'
#'@param allList An object created via mergeLabels.
#'@param pkList List of primary keys.
#'@param fkList List of foreign keys.
#'@param filePath Path to the db file to write (including name); has to end on '.db'.
#'
#'@return Creates a data base in the given path, returns NULL.
#'
#'@examples
#'# see createDB
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
createGADS.GADSdat <- function(allList, pkList, filePath) {
  allList_dat <- list(allList$dat)
  names(allList_dat) <- names(pkList)
  eatDB::createDB(dfList = allList_dat, pkList = pkList, metaData = allList$labels, filePath = filePath)
}


#### Get Names from GADS
#############################################################################
#' Variables names of a GADS data base.
#'
#' Returns a list of all variable names included in the GADS data base.
#'
#' Extracts names of all variables included in the relational data base, structured as a list with the individual data tables as list elements.
#'
#'@param filePath Path of an existing GADS data base.
#'
#'@return Returns a named list of variable names per data table.
#'
#'@examples
#'\dontrun{
#'varNames <- namesGADS("t:/_R_Tutorials/R_Workshops/04_eatPakete/minigads_2010.db")
#'
#'# all variable names sorted by data table
#'varNames
#'
#'# variables in a specific data table
#'varNames$allDat
#'
#'}
#'
#'@export
namesGADS <- function(filePath) {
  eatDB::dbNames(filePath = filePath, includeMeta = FALSE)
}


#### Get Labels from Gads
#############################################################################
#' Labels from relational db.
#'
#' Returns the variable and value labels of all variables in the GADS
#'
#' Variable, value and missing labels as stored in the original SPSS-files and factors from R files are converted to long format for storage in the data base. labelsGADS returns them as a long format data frame.
#'
#'
#'@param filePath Path of the existing db file.
#'
#'@return Returns a long format data frame including variable names, labels, values, value labels and missing labels.
#'
#'@examples
#'\dontrun{
#'metaData <- labelsGADS("t:/_R_Tutorials/R_Workshops/04_eatPakete/minigads_2010.db")
#'View(metaData)
#'}
#'
#'@export
labelsGADS <- function(filePath) {
  eatDB::dbSingleDF(dfName = "Meta_data", filePath = filePath)
}


#### Get data from Gads
#############################################################################
#' Get data from GADS data base.
#'
#'Extracts variables from a GADS data base. Only the specified variables are extracted. Note that this selection determines the format of the \code{data.frame} that is extracted.
#'
#' See \code{\link{createDB}} and \code{\link{dbPull}} for further explanation of the query and merging processes.
#'
#'@param vSelect Variables
#'@param filePath Path of the existing db file.
#'
#'@return Returns a GADSdat object.
#'
#'@examples
#'\dontrun{
#'gads10 <- getGADS(vSelect = c("idstud", "wgt", "jkzone", "jkrep", "imp", "domain", "score"),
#'                  filePath = "t:/_R_Tutorials/R_Workshops/04_eatPakete/minigads_2010.db")
#'# View Meta Data
#'metaData <- extractMeta(gads10)
#'# Extract Data for Analysis
#'dat <- extractData(gads10)
#'}
#'
#'@export
getGADS <- function(vSelect = NULL, filePath) {
  GADSdat <- eatDB::dbPull(vSelect = vSelect, filePath = filePath)
  allLabels <- labelsGADS(filePath = filePath)
  selectLabels <- allLabels[allLabels$varName %in% names(GADSdat), , drop = FALSE]
  # drop irrelevant data_table column and duplicate meta data from different data tables
  selectLabels <- unique(selectLabels[, !names(selectLabels) %in% "data_table"])
  new_GADSdat(dat = GADSdat, labels = selectLabels)
}


#### Get data from Gads fast
#############################################################################
#' Get data from GADS data base fast from server directory.
#'
#' Extracts variables from a GADS data base. Only the specified variables are extracted. Note that this selection determines the format of the \code{data.frame} that is extracted. CAREFUL: This function uses a local temporary directory to speed up loading the GADS from a server and caches the data base locally for a running windows session. Use \code{\link{clean_cache}} to clean up this temporary directory before terminating the running \code{R} session.
#'
#' A random temporary directory is used for caching the data base and should be removed, when the computer is restarted. See \code{\link{createDB}} and \code{\link{dbPull}} for further explanation of the query and merging processes.
#'
#'@param vSelect Variables
#'@param filePath Path of the existing db file.
#'@param tempPath Local directory in which the dataBase can temporarily be stored. Using the default is recommended.
#'
#'@return Returns a GADSdat object.
#'
#'@examples
#'\dontrun{
#'gads10 <- getGADS_fast(vSelect = c("idstud", "wgt", "jkzone", "jkrep", "imp", "domain", "score"),
#'                       filePath = "t:/_R_Tutorials/R_Workshops/04_eatPakete/minigads_2010.db")
#'# View Meta Data
#'metaData <- extractMeta(gads10)
#'# Extract Data for Analysis
#'dat <- extractData(gads10)
#'}
#'
#'@export
getGADS_fast <- function(vSelect = NULL, filePath, tempPath = tempdir()) {
  # checks for tempPath
  if(!is.character(tempPath) || length(tempPath) != 1) stop("tempPath is not a character vector of length 1.")
  if(!file.exists(tempPath)) stop("tempPath is not an existing directory.")
  if(file.access(tempPath, mode = 2) != 0) stop("User has no writing permission for tempPath.")

  fileName <- eatTools::halveString(filePath, "/", first = FALSE)[[2]]
  tempFile <- file.path(tempPath, fileName)
  tempFile <- paste(tempPath, fileName, sep = "/")
  # if (length(vSelect) >10)browser()
  # create copy
  if(!file.exists(tempFile)) {
    cat("Copy file to local directory...\n")
    if(file.exists(tempFile)) stop(tempFile, "is an existing file and can not be used as local copy.")
    file.copy(from = filePath, to = tempFile, overwrite = FALSE, recursive = FALSE)
  } else cat("Using cached data base...\n")

  #
  cat("Pull data from GADS db...\n")
  GADSdat <- getGADS(vSelect = vSelect, filePath = tempFile)
  GADSdat
}




#### Clean cache
#############################################################################
#' Clean temporary cache.
#'
#' Cleans the temporary cache, speficied by tempdir(). This function should always be executed at the end of an \code{\link{R}} session if \code{\link{getGADS_fast}} or \code{\link{getTrendGADS}} with \code{fast = TRUE} has been used.
#'
#' tbd
#'
#'@param tempPath Local directory in which the data base was temporarily be stored.
#'
#'@return Returns nothing.
#'
#'@examples
#'\dontrun{
#'clean_cache()
#'}
#'
#'@export
clean_cache <- function(tempPath = tempdir()) {
  cat("Scanning temporary directory:\n", tempdir(), "\n")
  cont <- list.files(path = tempPath, full.names = TRUE)
  nam <- list.files(path = tempPath)
  cat("The following files are in the directory:\n")
  print(nam)
  answ <- readline("Should all these files be deleted? y/n: ")
  if(identical(answ, "y")) {
    cat("Cleaning temporary directory... \n")
    unlink2 <- function(x) unlink(x, recursive = TRUE)
    do.call(unlink2, list(cont))
    message("All files deleted.")
  } else {
    message("No files deleted.")
  }
return()
}




