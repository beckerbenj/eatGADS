#### Check consistency of missings
#############################################################################
#' Check and Adjust Missing Coding
#'
#' Function to check if missings are coded and labeled correctly in a GADSdat object.
#'
#' The function compares value labels \code{"valLabels"} and missing codes \code{"missings"} of a \code{GADSdat} object and its meta data information. Missmatches are reported and can be automatically adjusted.
#'
#'@param GADSdat \code{GADSdat} object imported via \code{eatGADS}.
#'@param missingLabel Single string indicating how missing labels are commonly named in the value labels.
#'@param addMissingCode If \code{TRUE}, missing codes are added according to occurence of \code{"missingLabel"} in \code{"valLabel"}.
#'@param addMissingLabel If \code{TRUE}, \code{"generic missing"} is added according to occurence of \code{"mis"} in \code{"missings"}.
#'
#'@return Returns a GADSdat object.
#'
#'@examples
#'# Example data set
#'#to be done
#'
#'@export
checkMissings <- function(GADSdat, missingLabel = "missing", addMissingCode = TRUE, addMissingLabel = TRUE) {
  UseMethod("checkMissings")
}

#'@export
checkMissings.GADSdat <- function(GADSdat, missingLabel = "missing", addMissingCode = TRUE, addMissingLabel = TRUE) {
  check_GADSdat(GADSdat)
  labels <- GADSdat$labels

  missCode_rows_fail <- which(grepl(missingLabel, labels$valLabel) & is.na(labels$missings))
  missLabel_rows_fail <- which(labels$missings == "miss" & !grepl(missingLabel, labels$valLabel))

  ## Which variables are affected, how many adjustments are performed
  if(length(missCode_rows_fail) > 0) {
    message("The following variables have value labels including the term 'missing' which are not coded as missing:\n",
            paste(unique(labels[missCode_rows_fail, "varName"]), collapse = ", "))
    if(identical(addMissingCode, TRUE)) labels <- insert_string(df = labels, rows = missCode_rows_fail, col = "missings", string = "miss")
  }

  if(length(missLabel_rows_fail) > 0) {
    message("The following variables have values coded as missing but value label does not include the term 'missing':\n",
            paste(unique(labels[missLabel_rows_fail, "varName"]), collapse = ", "))
    if(identical(addMissingLabel, TRUE)) labels <- insert_string(df = labels, rows = missLabel_rows_fail, col = "valLabel", string = "generic missing")
  }

  GADSdat$labels <- labels
  GADSdat
}

insert_string <- function(df, rows, col, string) {
  message("'", string, "' is inserted into column ", col, " for ", length(rows), " rows.")
  df[rows, col] <- string
  df
}



#### Extract Change Meta data
#############################################################################
#' Extract table for Meta Data Changes.
#'
#' Function to obtain a data frame from a \code{GADSdat} object for meta data changes.
#'
#' Changes to values are currently not supported.
#'
#'@param GADSdat \code{GADSdat} object imported via eatGADS.
#'@param changeCol Names of columns on which changes are to be specified.
#'
#'@return Returns the complete meta data sheet with additional columns for changes in meta data.
#'
#'@examples
#'# Example data set
#'#to be done
#'
#'@export
getChangeMeta <- function(GADSdat, changeCol = c("varName", "varLabel", "format", "display_width", "labeled", "value", "valLabel", "missings")) {
  UseMethod("getChangeMeta")
}

#'@export
getChangeMeta.GADSdat <- function(GADSdat, changeCol = c("varName", "varLabel", "format", "display_width",
                                                         "labeled", "valLabel", "missings")) {
  check_GADSdat(GADSdat)
  if("value" %in% changeCol) stop("Changes to values are currently not supported. Remove 'value' from changeCol.")
  if(!all(changeCol %in% names(GADSdat$labels))) stop("At least on variable name supplied in changeCol is not an actual column of the meta data table.", call. = FALSE)
  newCols <- paste(changeCol, "_new", sep = "")
  labels <- GADSdat$labels
  for(n in newCols) labels[, n] <- NA
  labels
}

#### Apply Change Meta data
#############################################################################
#' Apply Meta Data Changes.
#'
#' Function to apply meta data changes to a \code{GADSdat} object specified by a change table extracted by \code{\link{getChangeMeta}}.
#'
#' Values for which the change columns contain \code{NA} remain unchanged. Changes to values are currently not supported.
#'
#'@param GADSdat GADSdat object imported via eatGADS.
#'@param changeTable Change table as provided by \code{\link{getChangeMeta}}.
#'
#'@return Returns the modified \code{GADSdat} object.
#'
#'@examples
#'# Example data set
#'#to be done
#'
#'@export
applyChangeMeta <- function(GADSdat, changeTable) {
  UseMethod("applyChangeMeta")
}

#'@export
applyChangeMeta.GADSdat <- function(GADSdat, changeTable) {
  check_GADSdat(GADSdat)
  check_changeTable(GADSdat, changeTable)

  dat <- GADSdat$dat
  labels <- GADSdat$labels

  # 01) variable names (changes in data and in meta data)
  changeNameDF <- unique(changeTable[!is.na(changeTable[, "varName_new"]), c("varName", "varName_new")])
  if(any(!changeNameDF$varName %in% names(dat))) stop("varName in the changeTable is not a real variable name.")
  for(i in changeNameDF[, "varName"]) {
    names(dat)[names(dat) == i] <- changeNameDF[changeNameDF$varName == i, "varName_new"]
    labels[labels$varName == i, "varName"] <- changeNameDF[changeNameDF$varName == i, "varName_new"]
  }

  # optional: function for value recoding if needed
  # 02)  for all but varNames and values (changes only in meta data)
  change_vars <- grep("_new", names(changeTable), value = TRUE)
  simpleChanges <- changeTable[, change_vars[!change_vars %in% c("varName_new")], drop = FALSE]
  # NA in changeTable: value is kept; otherwise changed
  for(i in names(simpleChanges)) {
    oldName <- strsplit(i, "_")[[1]][1]
    labels[, oldName] <- ifelse(is.na(simpleChanges[, i]), yes = labels[, oldName], no = simpleChanges[, i])
  }

  new_GADSdat(dat = dat, labels = labels)
}


# check change Table, also in relation to GADSdat object
check_changeTable <- function(GADSdat, changeTable) {
  if(!is.data.frame(changeTable)) stop("changeTable is not a data.frame.")
  oldChangeTable <- changeTable[, seq(ncol(GADSdat$labels))]
  newChangeTable <- changeTable[, (ncol(GADSdat$labels)+1):ncol(changeTable), drop = FALSE]

  if(!identical(GADSdat$labels, oldChangeTable)) stop("GADSdat and changeTable are not compatible. Columns without '_new' should not be changed in the changeTable.", call. = FALSE)

  if(any(!grepl("_new", names(newChangeTable)))) stop("Illegal additional column names in changeTable.", call. = FALSE)
  if("value_new" %in% names(newChangeTable)) stop("Illegal additional column names in changeTable.", call. = FALSE)

  varLevel_vars <- names(newChangeTable[!grepl("valLabel|missings", names(newChangeTable))])
  for(i in unique(changeTable$varName)) {
    varLevel_df <- changeTable[changeTable$varName == i, varLevel_vars, drop = FALSE]
    varLevel_df <- unique(varLevel_df)
    if(nrow(varLevel_df) > 1) stop("Variable ", i, " has varying changes on variable level.", call. = FALSE)
  }
  return()
}


#### Change Variable names
#############################################################################
#' Change Variable Names.
#'
#' Change variable names of a \code{GADSdat} object.
#'
#' Applied to a \code{GADSdat} object, this function is a wrapper of \code{\link{getChangeMeta}} and \code{\link{applyChangeMeta}}
#'
#'@param GADSdat GADSdat object imported via eatGADS.
#'@param oldNames Vector containing the old variable names.
#'@param newNames Vector containing the new variable names, in identical order as oldNames.
#'
#'@return Returns the GADSdat object with changed variable names.
#'
#'@examples
#'# Example data set
#'#to be done
#'
#'@export
changeVarNames <- function(GADSdat, oldNames, newNames) {
  UseMethod("changeVarNames")
}

#'@export
changeVarNames.GADSdat <- function(GADSdat, oldNames, newNames) {
  if(length(oldNames) != length(newNames)) stop("oldNames and newNames are not of identical length.", call. = FALSE)
  if(!(is.character(oldNames) && is.character(newNames))) stop("oldNames and newNames are not character vectors.", call. = FALSE)
  if(any(!oldNames %in% names(GADSdat$dat))) stop("varName in oldNames is not a real variable name.", call. = FALSE)
  changeTable <- getChangeMeta(GADSdat, changeCol = "varName")
  for(i in seq_along(oldNames)) {
    changeTable[changeTable$varName == oldNames[i], "varName_new"] <- newNames[i]
  }
  applyChangeMeta(GADSdat, changeTable = changeTable)
}



