
#### Create Linking Error data base
#############################################################################
#' Create data base for Linking Errors.
#'
#' Create a data base that includes linking errors for trend reports.
#'
#' Creates a single data table (with meta data) including the linking errors which are later used in \code{\link{getTrendGADS}}.
#'
#'@param LEs GADSdat object with linking errors.
#'@param IDvars Primary keys in the data frame.
#'@param filePath Path of the existing db file.
#'
#'@return Create a data base.
#'
#'@examples
#'# See vignette.
#'
#'@export
createLEs <- function(LEs, IDvars, filePath) {
  UseMethod("createLEs")
}
#'@export
createLEs.GADSdat <- function(LEs, IDvars, filePath) {
  pkList <- list(LEs = IDvars)
  eatDB::createDB(dfList = list(LEs = LEs$dat), pkList = pkList, metaData = LEs$labels, filePath = filePath)
}



#### Get data from two Gads and Linking Errors
#############################################################################
#' Get data for trend reports.
#'
#' Extracts variables from a two GADS data base and a linking error data base.
#'
#' This function extracts data from two GADS data bases and a linking error data base. The data is merged and can further be used via \code{\link{extractData}}. See createDB and dbPull for further explanation of the query and merging processes.
#'
#'@param vSelect Variables
#'@param filePath1 Path of the first GADS db file.
#'@param filePath2 Path of the second GADS db file.
#'@param lePath Path of the linking error db file. If NULL, no linking errors are added to the data.
#'@param years A numeric vector of length 2. The first year corresponds to filePath1, the second year to filePath2.
#'
#'@return Returns a GADSdat object.
#'
#'@examples
#'# See vignette.
#'
#'@export
getTrendGADS <- function(vSelect = NULL, filePath1, filePath2, lePath = NULL, years) {
  # Check for uniqueness of data bases used
  if(is.null(lePath)) {
    if(length(unique(c(filePath1, filePath2))) != 2) stop("All file arguments have to point to different files.")
    } else {
      if(length(unique(c(filePath1, filePath2, lePath))) != 3) stop("All file arguments have to point to different files.")
    }
  if(!(length(years) == 2 && as.numeric(years))) stop("years has to be a numeric vector of length 2.")
  # check if vSelect in both GADS
  checkTrendGADS(filePath1 = filePath1, filePath2 = filePath2)

  # load both
  g1 <- getGADS(vSelect = vSelect, filePath = filePath1)
  g2 <- getGADS(vSelect = vSelect, filePath = filePath2)

  # rbind, add year
  g1 <- add_year(g1, years[1])
  g2 <- add_year(g2, years[2])
  g_dat <- rbind(g1[["dat"]], g2[["dat"]])
  label_list <- lapply(list(g1, g2), function(x) x$labels)
  l_dat <- merge_labels_dfs(label_list, name = years)
  g <- new_GADSdat(dat = g_dat, labels = l_dat)

  # add linking errors (tbd!!!!!!)
  if(!is.null(lePath)) {
    les <- getGADS(filePath = lePath)
    les[["labels"]][, "data_table"] <- "LEs"
    le_keys <- eatDB::dbKeys(lePath)[["pkList"]][[1]]
    old_g <- g
    # if(identical(years, c(2010, 2015))) browser()
    g[["dat"]] <- merge(g[["dat"]], les[["dat"]], by = le_keys)
    g[["labels"]] <- rbind(g[["labels"]], les[["labels"]])
  }
  g
}

## Check compatability of trend data bases
checkTrendGADS <- function(filePath1, filePath2, lePath = NULL) {
  # check levels and keys
  k1 <- eatDB::dbKeys(filePath1)
  k2 <- eatDB::dbKeys(filePath2)

  pkEquals <- unlist(Map(function(pk1, pk2) identical(pk1, pk2), pk1 = k1$pkList, pk2 = k2$pkList))
  fkEquals <- unlist(Map(function(fk1, fk2) identical(fk1, fk2), fk1 = k1$fkList, fk2 = k2$fkList))
  if(!all(pkEquals)) stop("Trend data bases must have the same primary key structure.")
  if(!all(fkEquals)) stop("Trend data bases must have the same foreign key structure.")

  #
  return()
}

add_year <- function(GADSdat, year) {
  old_GADSdat <- GADSdat
  GADSdat[["dat"]][, "year"] <- year
  GADSdat <- suppressMessages(updateMeta(old_GADSdat, GADSdat[["dat"]]))
  GADSdat[["labels"]][GADSdat[["labels"]]$varName == "year", "varLabel"] <- "Trendvariable, indicating the year of the assessment."
  GADSdat

}

