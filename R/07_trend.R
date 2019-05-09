
#### Get data from two Gads and Linking Errors
#############################################################################
#' Get data for trend reports.
#'
#' Extracts variables from two GADS data bases and a linking error data base. Data can then be extracted from the \code{GADSdat} object via \code{\link{extractData}}. For extracting meta data from a db file or a \code{GADSdat} object see \code{\link{extractMeta}}.
#'
#' This function extracts data from two GADS data bases and a linking error data base. All data bases have to be created via \code{\link{createGADS}}. The two GADS are joined via \code{rbind} and a variable \code{year} is added, corresponding to the argument \code{years}. If \code{lePath} is specified, linking errors are also extracted and then merged to the GADS data. Make sure to also extract the key variables necessary for merging the linking errors (the domain variable for all linking errors, additionally the competence level variable for linking errors for competence levels). The \code{GADSdat} object can then further be used via \code{\link{extractData}}. See \code{\link[eatDB]{createDB}} and \code{\link[eatDB]{dbPull}} for further explanation of the querying and merging processes. To speed up the data loading, \code{\link{getGADS_fast}} can be used. In this case, use \code{\link{clean_cache}} to clean up this temporary directory before terminating the running \code{R} session.
#'
#'@param filePath1 Path of the first GADS db file.
#'@param filePath2 Path of the second GADS db file.
#'@param lePath Path of the linking error db file. If NULL, no linking errors are added to the data.
#'@param vSelect Variables from both GADS to be selected (as character vector).
#'@param leSelect Names of linking errors to be selected (as character vector).
#'@param years A numeric vector of length 2. The first elements corresponds to filePath1, the second element to filePath2.
#'@param fast Should \code{\link{getGADS_fast}} be used for data loading instead of \code{\link{getGADS}}? Using the default is heavily recommended.
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
#'
#'# Extract Data
#'dat <- extractData(trend_gads)
#'
#'# Extract Meta Data
#'extractMeta(trend_gads)
#'
#'}
#'
#'@export
getTrendGADS <- function(filePath1, filePath2, lePath = NULL, vSelect = NULL, leSelect = NULL, years, fast = FALSE, tempPath = tempdir()) {
  # Check for uniqueness of data bases used
  if(is.null(lePath)) {
    if(length(unique(c(filePath1, filePath2))) != 2) stop("All file arguments have to point to different files.")
    } else {
      if(length(unique(c(filePath1, filePath2, lePath))) != 3) stop("All file arguments have to point to different files.")
    }
  if(!(length(years) == 2 && as.numeric(years))) stop("years has to be a numeric vector of length 2.")
  # check if vSelect in both GADS
  checkTrendGADS(filePath1 = filePath1, filePath2 = filePath2)

  if(!identical(fast, TRUE)) {
    g1 <- getGADS(vSelect = vSelect, filePath = filePath1)
    g2 <- getGADS(vSelect = vSelect, filePath = filePath2)
  } else {
    g1 <- getGADS_fast(vSelect = vSelect, filePath = filePath1, tempPath = tempPath)
    g2 <- getGADS_fast(vSelect = vSelect, filePath = filePath2, tempPath = tempPath)
  }

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
  if(!all(pkEquals)) stop("Trend data bases must have the same primary key structure:")
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
#' Checks compatability of twp GADS data bases.
#'
#' This function checks if both data bases perform identical joins via foreign keys, if they contain the same variable names and if these variables have the same value labels. Results of this comparison are reported on data table level as messages and as an output list.
#'
#' An error is thrown if the key structure or the data table structure differs between the two data bases. Differences regarding meta data for missing value labels and for variables labels (and formatting) are ignored.
#'
#'@param filePath1 Path of the first GADS db file.
#'@param filePath2 Path of the second GADS db file.
#'
#'@return Returns a report list.
#'
#'@examples
#'# See vignette.
#'
#'@export
checkTrendStructure <- function(filePath1, filePath2) {
  checkTrendGADS(filePath1, filePath2, lePath)
  # Variables
  n1 <- namesGADS(filePath1)
  n2 <- namesGADS(filePath2)

  var_comp <- lapply(names(n1), function(dt_name) {
    message("Checking names for data table ", dt_name, "...")
    compare_and_order(set1 = n1[[dt_name]], set2 = n2[[dt_name]], name1 = "data base 1", name2 = "data base 2", FUN = message)
  })
  names(var_comp) <- names(n1)
  # Meta Data
  meta1 <- extractMeta(filePath1)
  meta2 <- extractMeta(filePath2)

  meta_comp <- lapply(names(n1), function(dt_name) {
    # if(dt_name == "NoImp") browser()
    meta_single1 <- meta1[meta1$data_table == dt_name & meta1$varName %in% var_comp[[dt_name]][["in_both_ordered"]], ]
    meta_single2 <- meta2[meta2$data_table == dt_name & meta2$varName %in% var_comp[[dt_name]][["in_both_ordered"]], ]
    message("Checking meta data for data table ", dt_name, "...")
    compare_meta(meta1 = meta_single1, meta2 = meta_single2)
  })
  names(meta_comp) <- names(n1)

  list("Variable Comparison" = var_comp, "Meta Data Comparison" = meta_comp)
}

## Compare meta data
compare_meta <- function(meta1, meta2) {
  diff_in_meta <- character()
  for(nam in unique(meta1$varName)) {
    var_meta1 <- meta1[meta1$varName == nam, c("value", "valLabel", "missings")]
    var_meta2 <- meta2[meta2$varName == nam, c("value", "valLabel", "missings")]

    # eliminate all possible irrelevant causes for inequality
    var_meta1 <- var_meta1[order(var_meta1$value), ]
    var_meta2 <- var_meta2[order(var_meta2$value), ]
    var_meta1 <- var_meta1[var_meta1$missings == "valid" | is.na(var_meta1$missings), ]
    var_meta2 <- var_meta2[var_meta2$missings == "valid" | is.na(var_meta2$missings), ]
    row.names(var_meta1) <- row.names(var_meta2) <- NULL

    # treat unlabeled variables as no value labels given
    if(nrow(var_meta1) == 1 && all(is.na(var_meta1[1, ]))) var_meta1 <- var_meta1[-1, ]
    if(nrow(var_meta2) == 1 && all(is.na(var_meta2[1, ]))) var_meta2 <- var_meta2[-1, ]

    test_eq <- all.equal(var_meta1, var_meta2)
    if(!identical(test_eq, TRUE)) diff_in_meta <- c(diff_in_meta, nam)
  }
  if(length(diff_in_meta) > 0 ) message("The following variables have different meta data on value level: ", paste(diff_in_meta, collapse = ", "))
  diff_in_meta
}




