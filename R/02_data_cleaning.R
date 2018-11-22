#### Check consistency of missings
#############################################################################
#' Check and Adjust Missing Coding
#'
#' Function to check if missings are coded and labeled correctly in a GADSdat object.
#'
#' The function compares value labels and missing codes of a GADSdat object and its labels data frame. Missmatches are reported and can be automatically adjusted.
#'
#'@param GADSdat GADSdat object imported via eatGADS.
#'@param missingLabel Characatre how missing labels are named.
#'@param addMissingCode If [TRUE], missing codes are added according to occurence of [missingLabel] in [valLabel].
#'@param addMissingLabel If [TRUE], [generic missing] is added according to occurence of [mis] in [missings].
#'
#'@return Returns a GADSdat object
#'
#'@examples
#'# Example data set
#'to be done
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



#### Change Meta data
#############################################################################
#' Extract table for Meta Data Changes.
#'
#' Function to obtain a data frame from a GADSdat object for meta data changes. Changes to value are currently not supported.
#'
#' The function ....
#'
#'@param GADSdat GADSdat object imported via eatGADS.
#'@param changeCol Names of columns on which changes are to be specified.
#'
#'@return Returns the complete meta data sheet with additional columns for changes in meta data.
#'
#'@examples
#'# Example data set
#'to be done
#'
#'@export
getChangeMeta <- function(GADSdat, changeCol = c("varName", "varLabel", "format", "display_width", "class", "value", "valLabel", "missings")) {
  UseMethod("getChangeMeta")
}

#'@export
getChangeMeta.GADSdat <- function(GADSdat, changeCol = c("varName", "varLabel", "format", "display_width",
                                                         "class", "valLabel", "missings")) {
  check_GADSdat(GADSdat)
  if("value" %in% changeCol) stop("Changes to values are currently not supported. Remove 'value' from changeCol.")
  if(!all(changeCol %in% names(GADSdat$labels))) stop("At least on variable name supplied in changeCol is not an actual column of the meta data table.", call. = FALSE)
  newCols <- paste(changeCol, "_new", sep = "")
  labels <- GADSdat$labels
  for(n in newCols) labels[, n] <- NA
  labels
}


#' Apply Meta Data Changes.
#'
#' Function to apply meta data changes to a GADSdat object specified by a change table extracted by getChangeMeta.
#'
#' The function ....
#'
#'@param GADSdat GADSdat object imported via eatGADS.
#'@param changeTable Change table.
#'
#'@return Returns the complete meta data sheet with additional columns for changes in meta data.
#'
#'@examples
#'# Example data set
#'to be done
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
  for(i in changeNameDF[, "varName"]) {
    names(dat)[names(dat) == i] <- changeNameDF[changeNameDF$varName == i, "varName_new"]
    labels[labels$varName == i, "varName"] <- changeNameDF[changeNameDF$varName == i, "varName_new"]
  }

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
  oldChangeTable <- changeTable[, seq(ncol(GADSdat$labels))]
  newChangeTable <- changeTable[, (ncol(GADSdat$labels)+1):ncol(changeTable), drop = FALSE]

  if(!identical(GADSdat$labels, oldChangeTable)) stop("GADSdat and changeTable are not compatible. Columns without '_new' should not be changed in the changeTable.", call. = FALSE)

  if(any(!grepl("_new", names(newChangeTable)))) stop("Illegal additional column names in changeTable.", call. = FALSE)
  if("value_new" %in% names(newChangeTable)) stop("Illegal additional column names in changeTable.", call. = FALSE)

  varLevel_vars <- names(newChangeTable[!grepl("valLabel|missings", names(newChangeTable))])
  for(i in unique(changeTable$varName)) {
    varLevel_df <- changeTable[changeTable$varName == i, varLevel_vars, drop = FALSE]
    varLevel_df <- unique(varLevel_df)
    if(ncol(varLevel_df) > 1) stop("Variable ", i, " has varying changes on variable level.", call. = FALSE)
  }
  return()
}







