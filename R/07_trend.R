
#### Get data from two Gads and Linking Errors
#############################################################################
#' Get data for trend reports.
#'
#' Extracts variables from a two GADS data base and a linking error data base. Data can then be extracted from the \code{GADSdat} object via \code{\link{extractData}}. For extracting meta data from a db file or the \code{GADSdat} object see \code{\link{extractMeta}}.
#'
#' This function extracts data from two GADS data bases and a linking error data base. All data bases have to be created via \code{\link{createGADS}}. The two GADS are joined via \code{rbind} and a variable \code{year} is added, corresponding to the argument \code{years}. If \code{lePath} is specified, linking errors are also extracted and then merged to the GADS data. Make sure to also extract the key variables necessary for merging the linkning errors (the domain variable for all linking errors, additionally the competence level variable for linking errors for competence levels). The \code{GADSdat} object can then further be used via \code{\link{extractData}}. See \code{\link[eatDB]{createDB}} and \code{\link[eatDB]{dbPull}} for further explanation of the querying and merging processes.
#'
#'@param filePath1 Path of the first GADS db file.
#'@param filePath2 Path of the second GADS db file.
#'@param lePath Path of the linking error db file. If NULL, no linking errors are added to the data.
#'@param vSelect Variables from both GADS to be selected (as character vector).
#'@param leSelect Names of linking errors to be selected (as character vector).
#'@param years A numeric vector of length 2. The first year corresponds to filePath1, the second year to filePath2.
#'@param tempPath The directory, in which both GADS will be temporarily stored. Using the default is heavily recommended.
#'
#'@return Returns a GADSdat object.
#'
#'@examples
#'\dontrun{
#'trend_gads <- getTrendGADS(filePath1 = "t:/_R_Tutorials/R_Workshops/04_eatPakete/minigads_2010.db",
#'                           filePath2 = "t:/_R_Tutorials/R_Workshops/04_eatPakete/minigads_2015.db",
#'                           lePath = "t:/_R_Tutorials/R_Workshops/04_eatPakete/les_2010_2015.db",
#'                           years = c(2010, 2015),
#'                           vSelect = c("idstud", "wgt", "jkzone", "jkrep", "imp", "domain", "score"),
#'                           leSelect = c("leScore", "domain"))
#'}
#'
#'@export
getTrendGADS <- function(filePath1, filePath2, lePath = NULL, vSelect = NULL, leSelect = NULL, years, tempPath = tempdir()) {
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
  g1 <- getGADS_fast(vSelect = vSelect, filePath = filePath1, tempPath = tempPath)
  g2 <- getGADS_fast(vSelect = vSelect, filePath = filePath2, tempPath = tempPath)

  # rbind, add year
  g1 <- add_year(g1, years[1])
  g2 <- add_year(g2, years[2])
  g_dat <- rbind(g1[["dat"]], g2[["dat"]])
  label_list <- lapply(list(g1, g2), function(x) x$labels)
  l_dat <- merge_labels_dfs(label_list, name = years)
  gads_trend <- new_GADSdat(dat = g_dat, labels = l_dat)

  # add linking errors (tbd!!!!!!)
  if(!is.null(lePath)) {
    les <- getGADS(filePath = lePath, vSelect = leSelect)
    le_keys <- eatDB::dbKeys(lePath)[["pkList"]]
    gads_trend <- merge_LEs(gads_trend = gads_trend, les = les, le_keys = le_keys)
  }
  gads_trend
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

merge_LEs <- function(gads_trend, les, le_keys) {
  les[["labels"]][, "data_table"] <- "LEs"
  le_keys <- unique(unlist(le_keys))
  curr_le_keys <- le_keys[le_keys %in% names(les[["dat"]])]
  old_g <- gads_trend
  # if(identical(years, c(2010, 2015))) browser()
  if(any(!curr_le_keys %in% names(gads_trend[["dat"]]))) stop("Incorrect linking error variables specified for GADS data.")
  gads_trend[["dat"]] <- merge(gads_trend[["dat"]], les[["dat"]], by = curr_le_keys)
  gads_trend[["labels"]] <- rbind(gads_trend[["labels"]], les[["labels"]])
  gads_trend
}


#### Checks structure of trend gads and linking errors
#############################################################################
#' Checks structure of trend gads and linking errors.
#'
#' tbd
#'
#' tbd
#'
#'@param filePath1 Path of the first GADS db file.
#'@param filePath2 Path of the second GADS db file.
#'@param lePath Path of the linking error db file. If NULL, no linking errors are added to the data.
#'
#'@return Create a data base.
#'
#'@examples
#'# See vignette.
#'
#'@export
checkTrendStructure <- function(filePath1, filePath2, lePath) {
  # tbd
}


