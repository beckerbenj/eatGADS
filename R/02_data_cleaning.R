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

  missCode_rows_fail <- which(grepl(missingLabel, labels$valLabel) & (is.na(labels$missings) | labels$missings == "valid"))
  missLabel_rows_fail <- which(labels$missings == "miss" & !grepl(missingLabel, labels$valLabel))

  ## Which variables are affected, how many adjustments are performed
  if(length(missCode_rows_fail) > 0) {
    message("The following variables have value labels including the term '", missingLabel ,"' which are not coded as missing:\n",
            paste(unique(labels[missCode_rows_fail, "varName"]), collapse = ", "))
    if(identical(addMissingCode, TRUE)) labels <- insert_string(df = labels, rows = missCode_rows_fail, col = "missings", string = "miss")
  }

  if(length(missLabel_rows_fail) > 0) {
    message("The following variables have values coded as missing but value label does not include the term '", missingLabel ,"':\n",
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



#### Extract Change Meta data on Variable Level
#############################################################################
#' Extract table for Meta Data Changes.
#'
#' Function to obtain a data frame from a \code{GADSdat} object for for changes to meta data on variable or on value level.
#'
#' Changes on variable level include variable names (\code{varName}), to variable labels (\code{varLabel}), SPSS format ((\code{format})) and display width (\code{display_width}). Changes on value level include values (\code{value}), to value labels (\code{valLabel}) and missing codes (\code{missings}).
#'
#'@param GADSdat \code{GADSdat} object imported via eatGADS.
#'@param level \code{variable} or \code{value}.
#'
#'@return Returns the meta data sheet for all variables including the corresponding change columns.
#'
#'@examples
#'# Example data set
#'#to be done
#'
#'@export
getChangeMeta <- function(GADSdat, level = "variable") {
  UseMethod("getChangeMeta")
}

#'@export
getChangeMeta.GADSdat <- function(GADSdat, level = "variable") {
  check_GADSdat(GADSdat)
  labels <- GADSdat[["labels"]]
  if(identical(level, "variable")) {
    oldCols <- c("varName", "varLabel", "format", "display_width", "labeled")
    newCols <- paste0(oldCols, "_new")
    for(n in newCols) labels[, n] <- NA
    change_sheet <- unique(labels[, c(oldCols, newCols)])
    return(new_varChanges(change_sheet))
  }
  if(identical(level, "value")) {
    oldCols <- c("value", "valLabel", "missings")
    newCols <- paste0(oldCols, "_new")
    for(n in newCols) labels[, n] <- NA
    change_sheet <- labels[, c("varName", oldCols, newCols)]
    return(new_valChanges(change_sheet))
  }
  stop("Invalid level argument.")
}

new_varChanges <- function(df) {
  stopifnot(is.data.frame(df))
  structure(df, class = c("varChanges", "data.frame"))
}
check_varChanges <- function(changeTable) {
  if(!is.data.frame(changeTable)) stop("changeTable is not a data.frame.")
  colNames <- c("varName", "varLabel", "format", "display_width", "labeled")
  colNames <- c(colNames, paste0(colNames, "_new"))
  if(any(!names(changeTable) %in% colNames)) stop("Irregular column names in changeTable.")
  # tbd: content checks for format and display width
  return()
}

new_valChanges <- function(df) {
  stopifnot(is.data.frame(df))
  structure(df, class = c("valChanges", "data.frame"))
}
check_valChanges <- function(changeTable) {
  if(!is.data.frame(changeTable)) stop("changeTable is not a data.frame.")
  # Columns
  oldCols <- c("value", "valLabel", "missings")
  newCols <- paste0(oldCols, "_new")
  colNames <- c("varName", oldCols, newCols)
  if(any(!names(changeTable) %in% colNames)) stop("Irregular column names in changeTable.")
  # Values in columns
  if(!all(changeTable[, "missings_new"] %in% c("miss", "valid") | is.na(changeTable[, "missings_new"]))) {
    stop("Irregular values in 'missings_new' column.")
  }
  if(is.character(changeTable[, "value_new"])) stop("String values can not be given value labels.")
  return()
}



#### Apply Change Meta data
#############################################################################
#' Apply Meta Data Changes.
#'
#' Function to apply meta data changes to a \code{GADSdat} object specified by a change table extracted by \code{\link{getChangeMeta}}.
#'
#' Values for which the change columns contain \code{NA} remain unchanged. Changes to values are currently not supported.
#'
#'@param changeTable Change table as provided by \code{\link{getChangeMeta}}.
#'@param GADSdat GADSdat object imported via eatGADS.
#'
#'@return Returns the modified \code{GADSdat} object.
#'
#'@examples
#'# Example data set
#'#to be done
#'
#'@export
applyChangeMeta <- function(changeTable, GADSdat) {
  UseMethod("applyChangeMeta")
}

#'@export
applyChangeMeta.varChanges <- function(changeTable, GADSdat) {
  check_GADSdat(GADSdat)
  check_varChanges(changeTable)
  check_changeTable(GADSdat, changeTable)
  dat <- GADSdat$dat
  labels <- GADSdat$labels
  # 01)  for all but varNames
  simple_change_vars <- setdiff(grep("_new", names(changeTable), value = TRUE), c("varName", "varName_new"))
  simpleChanges <- changeTable[, c("varName", simple_change_vars), drop = FALSE]
  # loop over variable names and column names, overwrite if not NA
  for(i in simpleChanges[, "varName"]) {
    simpleChange <- simpleChanges[simpleChanges[, "varName"] == i, ]
    for(k in simple_change_vars) {
      oldName <- strsplit(k, "_")[[1]][1]
      if(!is.na(simpleChange[, k])) labels[labels[, "varName"] == i, oldName] <- simpleChange[, k]
    }
  }
  # 02) variable names (changes in data and in meta data)
  changeNameDF <- changeTable[!is.na(changeTable[, "varName_new"]), c("varName", "varName_new")]
  for(i in changeNameDF[, "varName"]) {
    names(dat)[names(dat) == i] <- changeNameDF[changeNameDF$varName == i, "varName_new"]
    labels[labels$varName == i, "varName"] <- changeNameDF[changeNameDF$varName == i, "varName_new"]
  }

  new_GADSdat(dat = dat, labels = labels)
}

#'@export
applyChangeMeta.valChanges <- function(changeTable, GADSdat) {
  check_GADSdat(GADSdat)
  check_valChanges(changeTable)
  check_changeTable(GADSdat, changeTable)
  dat <- GADSdat$dat
  labels <- GADSdat$labels
  # 01) values
  if(sum(!is.na(changeTable[, "value_new"])) > 0) stop("Changes to values are not implemented yet.")
  # 02) valueLabels und missings
  simple_change_vars <- c("valLabel_new", "missings_new")
  simpleChanges <- changeTable[, c("varName", simple_change_vars), drop = FALSE]
  # loop over rows and column names, overwrite if not NA
  for(i in seq(nrow(simpleChanges))) {
    simpleChange <- simpleChanges[i, ]
    for(k in simple_change_vars) {
      oldName <- strsplit(k, "_")[[1]][1]
      if(!is.na(simpleChange[, k])) labels[i, oldName] <- simpleChange[, k]
    }
  }
  new_GADSdat(dat = dat, labels = labels)
}


# check change Table, also in relation to GADSdat object
check_changeTable <- function(GADSdat, changeTable) {
  newVars <- grep("_new", names(changeTable), value = TRUE)
  oldVars <- setdiff(names(changeTable), newVars)
  oldDat <- unique(GADSdat$labels[, oldVars])
  newDat <- unique(changeTable[, oldVars])
  class(newDat) <- "data.frame"

  if(!identical(oldDat, newDat)) stop("GADSdat and changeTable are not compatible. Columns without '_new' should not be changed in the changeTable.", call. = FALSE)
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
  changeTable <- getChangeMeta(GADSdat, level = "variable")
  for(i in seq_along(oldNames)) {
    changeTable[changeTable$varName == oldNames[i], "varName_new"] <- newNames[i]
  }
  applyChangeMeta(GADSdat, changeTable = changeTable)
}



#### Update Meta
#############################################################################
#' Update Meta data frame.
#'
#' If the data of a \code{GADSdat} or a \code{all_GADSdat} has changed, update assimilates the corresponding meta data set. Careful, this is a development version and should be only used with great care!
#'
#' tbd.
#'
#'@param GADSdat \code{GADSdat} or \code{all_GADSdat} object imported via eatGADS.
#'@param newDat Data frame oder list of data frames with the modified data.
#'
#'@return Returns the original object with updated meta data (and removes factors from the data).
#'
#'@examples
#'# Example data set
#'#to be done
#'
#'@export
updateMeta <- function(GADSdat, newDat) {
  UseMethod("updateMeta")
}
#'@export
updateMeta.GADSdat <- function(GADSdat, newDat) {
  check_GADSdat(GADSdat)
  stopifnot(is.data.frame(newDat))
  labels <- GADSdat[["labels"]]
  labels <- remove_rows_meta(labels = labels, allNames = names(newDat))

  addData <- add_rows_meta(labels = labels, newDat = newDat)
  addLabels <- addData[["labels"]]
  labels <- rbind(labels, addLabels) # Reihenfolge der Variablen, ist das wichtig?
  ## replace variables that have been imported newly
  newDat[, names(addData[["dat"]])] <- addData[["dat"]]

  mod_GADSdat <- new_GADSdat(dat = newDat, labels = labels)
  check_GADSdat(mod_GADSdat)
  mod_GADSdat
}
#'@export
updateMeta.all_GADSdat <- function(GADSdat, newDat) {
  check_all_GADSdat(GADSdat)
  stopifnot(is.list(newDat) && all(sapply(newDat, is.data.frame)))
  labels <- GADSdat[["allLabels"]]

  mod_single_GADSdats <- lapply(names(GADSdat[["datList"]]), function(i) {
    message("Analyzing data table ", i, ":")
    single_GADSdat <- new_GADSdat(dat = GADSdat[["datList"]][[i]], labels = labels[labels[, "data_table"] == i, names(labels) != "data_table"])
    updateMeta(single_GADSdat, newDat = newDat[[i]])
  })
  names(mod_single_GADSdats) <- names(newDat)
  mod_all_GADSdat <- do.call(mergeLabels, mod_single_GADSdats)
  check_all_GADSdat(mod_all_GADSdat)
  mod_all_GADSdat
}


### 1) remove unncessary rows from meta data
remove_rows_meta <- function(labels, allNames) {
  old_vars <- unique(labels[, "varName"][!labels[, "varName"] %in% allNames])
  if(!length(old_vars) > 0) {
    message("No rows removed from meta data.")
    return(labels)
  }
  message("Removing the following rows from meta data: ", paste(old_vars, collapse = ", "))
  labels[!labels[, "varName"] %in% old_vars, ]
}

### 2) add necessary rows
add_rows_meta <- function(labels, newDat) {
  new_vars <- unique(names(newDat)[!names(newDat) %in% labels[, "varName"]])
  if(!length(new_vars) > 0) {
    message("No rows added to meta data.")
    return(new_GADSdat(dat = data.frame(), labels = data.frame()))
  }
  message("Adding meta data for the following variables: ", paste(new_vars, collapse = ", "))
  addDat <- newDat[, new_vars, drop = FALSE]
  import_DF(addDat)
}



#### Check Names
#############################################################################
#' Check names for SQLite conventions.
#'
#' Applies variable names changes to \code{GADSdat} or \code{all_GADSdat} objects.
#'
#' tbd.
#'
#'@param GADSdat \code{GADSdat} or \code{all_GADSdat} object imported via eatGADS.
#'
#'@return Returns the original object with updated variable names.
#'
#'@examples
#'# Example data set
#'#to be done
#'
#'@export
checkVarNames <- function(GADSdat) {
  UseMethod("checkVarNames")
}
#'@export
checkVarNames.GADSdat <- function(GADSdat) {
  check_GADSdat(GADSdat)
  GADSdat[["labels"]][, "varName"] <- sapply(GADSdat[["labels"]][, "varName"], transf_names)
  names(GADSdat[["dat"]]) <- sapply(names(GADSdat[["dat"]]), transf_names)
  GADSdat
}
#'@export
checkVarNames.all_GADSdat <- function(GADSdat) {
  check_all_GADSdat(GADSdat)
  GADSdat[["allLabels"]][, "varName"] <- sapply(GADSdat[["allLabels"]][, "varName"], transf_names)
  GADSdat[["datList"]] <- lapply(GADSdat[["datList"]], function(df) {
    names(df) <- sapply(names(df), transf_names)
    df
  })
  GADSdat
}



