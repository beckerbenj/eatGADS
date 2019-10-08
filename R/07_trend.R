#### Checks structure of trend gads
#############################################################################
#' Checks compatability of two GADS data bases.
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
  check_keyStrcuture_TrendGADS(filePath1, filePath2)
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



#### Checks structure of trend gads and linking errors
#############################################################################
#' Checks compatability of GADS data bases with a linking error data base.
#'
#' This function checks if a linking error data base is compatible with the two trend GADS data bases. For checking the compatability of two GADS data bases see \code{\link{checkTrendStructure}}.
#'
#' This function inspects whether all linking error variables correspond to variables in the GADS data base and if the key variables also correspond to existing variables in the trend GADS data bases.
#'
#'@param filePath1 Path of the first GADS db file.
#'@param filePath2 Path of the second GADS db file.
#'@param lePath Path of the linking error GADS db file.
#'
#'@return Returns a report list.
#'
#'@examples
#'# See vignette.
#'
#'@export
checkLEStructure <- function(filePath1, filePath2, lePath) {
  nam1 <- namesGADS(filePath1)
  nam2 <- namesGADS(filePath2)
  namLE <- namesGADS(lePath)

  ## all Linking error variables in trend gads data bases (without LE_)?
  LE_variables <- grep("^LE_", unlist(namLE), value = TRUE)
  dep_variables <- gsub(pattern = "^LE_", replacement = "", LE_variables)

  dep_notIn_nam1 <- setdiff(dep_variables, unlist(nam1))
  dep_notIn_nam2 <- setdiff(dep_variables, unlist(nam2))
  if(length(dep_notIn_nam1) > 0) message("The following variables have linking errors but are not variables in data base 1: ",
                                         dep_notIn_nam1)
  if(length(dep_notIn_nam2) > 0) message("The following variables have linking errors but are not variables in data base 2: ",
                                         dep_notIn_nam2)

  ## all other variables should be primary keys
  le_pks <- unlist(eatDB::dbKeys(lePath)$pkList)
  le_keys <- setdiff(unlist(namLE), LE_variables)
  if(!all(le_pks %in% le_keys) || !all(le_keys %in% le_pks)) message("The linking error data base contains variables other than linking errors and key variables.")

  # all primary keys should be in trend gads data bases
  key_notIn_nam1 <- setdiff(le_pks, unlist(nam1))
  key_notIn_nam2 <- setdiff(le_pks, unlist(nam2))
  if(length(key_notIn_nam1) > 0) message("The following variables are key variables in the Linking Error data base but are not variables in data base 1: ", key_notIn_nam1)
  if(length(key_notIn_nam2) > 0) message("The following variables are key variables in the Linking Error data base but are not variables in data base 2: ", key_notIn_nam2)

  list(dep_notIn_nam1 = dep_notIn_nam1, dep_notIn_nam2 = dep_notIn_nam2,
       key_notIn_nam1 = key_notIn_nam1, key_notIn_nam2 = key_notIn_nam2)
}




#### Get data from two Gads and Linking Errors
#############################################################################
#' Get data for trend reports.
#'
#' Extracts variables from two GADS data bases and a linking error data base. Data can then be extracted from the \code{GADSdat} object via \code{\link{extractData}}. For extracting meta data from a db file or a \code{GADSdat} object see \code{\link{extractMeta}}. To speed up the data loading, \code{\link{getGADS_fast}} is used pre default.
#'
#' This function extracts data from two GADS data bases and a linking error data base. All data bases have to be created via \code{\link{createGADS}}. The two GADS are joined via \code{rbind} and a variable \code{year} is added, corresponding to the argument \code{years}. If \code{lePath} is specified, linking errors are also extracted and then merged to the GADS data. Make sure to also extract the key variables necessary for merging the linking errors (the domain variable for all linking errors, additionally the competence level variable for linking errors for competence levels). The \code{GADSdat} object can then further be used via \code{\link{extractData}}. See \code{\link[eatDB]{createDB}} and \code{\link[eatDB]{dbPull}} for further explanation of the querying and merging processes.
#'
#'@param filePath1 Path of the first GADS db file.
#'@param filePath2 Path of the second GADS db file.
#'@param lePath Path of the linking error db file. If NULL, no linking errors are added to the data.
#'@param vSelect Variables from both GADS to be selected (as character vector).
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
#'                           vSelect = c("idstud", "wgt", "jkzone", "jkrep", "imp", "domain", "score"))
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
getTrendGADS <- function(filePath1, filePath2, lePath = NULL, vSelect = NULL, years, fast = TRUE, tempPath = tempdir()) {
  # Check for uniqueness of data bases used
  if(is.null(lePath)) {
    if(length(unique(c(filePath1, filePath2))) != 2) stop("All file arguments have to point to different files.")
  } else {
    if(length(unique(c(filePath1, filePath2, lePath))) != 3) stop("All file arguments have to point to different files.")
  }
  if(!(length(years) == 2 && is.numeric(years))) stop("years has to be a numeric vector of length 2.")
  # check if vSelect in both GADS
  check_keyStrcuture_TrendGADS(filePath1 = filePath1, filePath2 = filePath2)

  # prepare vSelect for GADS (unique variables are allowed!)
  if(is.null(vSelect)) {
    vSelect <- unique(c(unlist(namesGADS(filePath1)), unlist(namesGADS(filePath2))))
  }
  vSelect1 <- list(in_gads = vSelect)
  vSelect2 <- list(in_gads = vSelect)

  vSelect1 <- check_vSelect(filePath1, vSelect = vSelect)
  vSelect2 <- check_vSelect(filePath2, vSelect = vSelect)
  not_in_both_gads <- intersect(vSelect1$not_in_gads, vSelect2$not_in_gads)
  if(length(not_in_both_gads) > 0) stop("Variables ", not_in_both_gads, " are in neither of both data bases.")
  if(!length(vSelect1$in_gads) > 0) stop("No variables from first data base selected.")
  if(!length(vSelect2$in_gads) > 0) stop("No variables from second data base selected.")
  # warn about added missings
  if(length(vSelect1$not_in_gads) > 0) warning(paste0("The following variables are not in GADS ", years[1],": ", vSelect1$not_in_gads,". NAs will be inserted if data is extracted."))
  if(length(vSelect2$not_in_gads) > 0) warning(paste0("The following variables are not in GADS ", years[2],": ", vSelect2$not_in_gads,". NAs will be inserted if data is extracted."))

  if(!identical(fast, TRUE)) {
    g1 <- getGADS(vSelect = vSelect1$in_gads, filePath = filePath1)
    g2 <- getGADS(vSelect = vSelect2$in_gads, filePath = filePath2)
  } else {
    cat(" -----  Loading first GADS ----- \n")
    g1 <- getGADS_fast(vSelect = vSelect1$in_gads, filePath = filePath1, tempPath = tempPath)
    cat(" -----  Loading second GADS ----- \n")
    g2 <- getGADS_fast(vSelect = vSelect2$in_gads, filePath = filePath2, tempPath = tempPath)
  }

  # add year
  g1 <- add_year(g1, years[1])
  g2 <- add_year(g2, years[2])

  # add linking errors (automatic variable selection)
  LEs <- NULL
  if(!is.null(lePath)) {
    leSelect <- make_leSelect(lePath = lePath, vSelect = vSelect)
    if(is.null(leSelect) || length(leSelect) > 0) LEs <- getGADS(filePath = lePath, vSelect = leSelect)
    else message("No linking errors for chosen variables available.")
  }

  gList <- list(g1, g2, LEs)
  names(gList) <- c(paste0("gads", years), "LEs")

  gads_trend <- do.call(mergeLabels, gList)
  class(gads_trend) <- c("trend_GADSdat", "all_GADSdat", "list")

  gads_trend
}


check_trend_GADSdat <- function(trend_GADSdat) {
  if(is.null(trend_GADSdat$datList[["LEs"]])) {
    trend_GADSdat$datList <- trend_GADSdat$datList[1:2]
  }
  check_all_GADSdat(trend_GADSdat)
}


## Check compatability of trend data bases
check_keyStrcuture_TrendGADS <- function(filePath1, filePath2) {
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

# select variables relevant for each gads
check_vSelect <- function(filePath, vSelect) {
  nam <- namesGADS(filePath)
  in_gads <- intersect(vSelect, unlist(nam))
  not_in_gads <- setdiff(vSelect, unlist(nam))
  list(in_gads = in_gads, not_in_gads = not_in_gads)
}

add_year <- function(GADSdat, year) {
  old_GADSdat <- GADSdat
  GADSdat[["dat"]][, "year"] <- year
  GADSdat <- suppressMessages(updateMeta(old_GADSdat, GADSdat[["dat"]]))
  GADSdat[["labels"]][GADSdat[["labels"]]$varName == "year", "varLabel"] <- "Trendvariable, indicating the year of the assessment"
  GADSdat

}

# automaticall generate variable selection for linking error data base
make_leSelect <- function(lePath, vSelect) {
  namLE <- namesGADS(lePath)
  if(is.null(vSelect)) return(NULL)
  LE_variables <- grep("^LE_", unlist(namLE), value = TRUE)
  dep_variables <- gsub(pattern = "^LE_", replacement = "", LE_variables)
  out <- LE_variables[dep_variables %in% vSelect]
  names(out) <- NULL
  out
}
