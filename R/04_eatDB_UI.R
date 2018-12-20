
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



#### Get Names from GADS
#############################################################################
#' Variables names of a GADS data base.
#'
#' Returns a list of all variable names included in the GADS data base.
#'
#' Extracts names of all variables included in the relational data base, structured as list with the individual data tables as list elements.
#'
#'@param filePath Path of an existing GADS data base.
#'
#'@return Returns a named list of variable names per data table.
#'
#'@examples
#'# not run:
#'# dbNames(filePath = "exampleDB.db")
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
#'# not run
#'# labelsGADS(filePath = "example.db")
#'
#'@export
labelsGADS <- function(filePath) {
  eatDB::dbSingleDF(dfName = "Meta_data", filePath = filePath)
}


#### Get data from Gads
#############################################################################
#' Get data from GADS data base.
#'
#' Extracts variables from a GADS data base.
#'
#' See createDB and dbPull for further explanation of the query and merging processes.
#'
#'@param vSelect Variables
#'@param filePath Path of the existing db file.
#'
#'@return Returns a GADSdat object.
#'
#'@examples
#'# See vignette.
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
#' Extracts variables from a GADS data base. Uses a local temporary directory to speed up loading the GADS from a server.
#'
#' See createDB and dbPull for further explanation of the query and merging processes.
#'
#'@param vSelect Variables
#'@param filePath Path of the existing db file.
#'@param tempPath Local directory in which the dataBase can temporarily be stored.
#'
#'@return Returns a GADSdat object.
#'
#'@examples
#'# See vignette.
#'
#'@export
getGADS_fast <- function(vSelect = NULL, filePath, tempPath) {
  # checks for tempPath
  if(!is.character(tempPath) || length(tempPath) != 1) stop("tempPath is not a character vector of length 1.")
  if(!file.exists(tempPath)) stop("tempPath is not an existing directory.")
  if(file.access(tempPath, mode = 2) != 0) stop("User has no writing permission for tempPath.")

  # create copy
  cat("Copy file to local directory...\n")
  fileName <- eatTools::halveString(filePath, "/", first = FALSE)[[2]]
  tempFile <- paste(tempPath, fileName, sep = "/")
  if(file.exists(tempFile)) stop(tempFile, "is an existing file and can not be used as local copy.")
  file.copy(from = filePath, to = tempFile, overwrite = FALSE, recursive = FALSE)
  # remove on exit
  on.exit(file.remove(tempFile))

  #
  cat("Pull data from GADS db...\n")
  GADSdat <- getGADS(vSelect = vSelect, filePath = tempFile)
  # remove File
  cat("Remove temporary data base...\n")
  #
  GADSdat
}

