#### Update Meta
#############################################################################
#' Update Meta data frame.
#'
#' If the data of a \code{GADSdat} or a \code{all_GADSdat} has changed, update assimilates the corresponding meta data set. Factors are transformed to numerical and their levels added to the meta data set. Careful, this is a development version and should be only used with great care!
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


#### Reuse Meta
#############################################################################
#' Use meta data for a variable from another GADSdat.
#'
#' Transfer meta information from one GADSdat to another.
#'
#' tbd.
#'
#'@param GADSdat \code{GADSdat} object imported via eatGADS.
#'@param varName Name of the variable that should get the new meta data.
#'@param other_GADSdat \code{GADSdat} object imported via eatGADS including the desired meta information. Can also be a GADS db or an \code{all_GADSdat} object.
#'@param other_varName Name of the variable that should get the new meta data in the \code{other_GADSdat}.
#'@param missingLabels How should meta data for missing values be treated? If NULL, missings are transfered as all other labels. If "drop", missing labels are dropped (useful for imputed data). If "leave", missing labels remain untouched.
#'
#'@return Returns the original object with updated meta data.
#'
#'@examples
#'# Example data set
#'#to be done
#'
#'@export
reuseMeta <- function(GADSdat, varName, other_GADSdat, other_varName = NULL, missingLabels = NULL) {
  UseMethod("reuseMeta")
}
#'@export
reuseMeta.GADSdat <- function(GADSdat, varName, other_GADSdat, other_varName = NULL, missingLabels = NULL) {
  if(!is.null(missingLabels) && !missingLabels %in% c("drop", "leave")) stop("Invalid input for argument missingLabels.")
  if(!varName %in% names(GADSdat$dat)) stop("varName is not a variable in the GADSdat.")
  # extract meta data
  if(is.null(other_varName)) other_varName <- varName
  new_meta <- extractMeta(other_GADSdat, other_varName)
  # compatability with meta data from all_GADSdat or data base
  new_meta <- new_meta[, names(new_meta) != "data_table"]
  new_meta[, "varName"] <- varName

  remove_rows <- which(GADSdat$labels$varName == varName)

  # special missing value labels treatment
  if(identical(missingLabels, "drop")) new_meta <- drop_missing_labels(new_meta)
  if(identical(missingLabels, "leave")) {
    new_meta <- drop_missing_labels(new_meta)
    remove_rows <- which(GADSdat$labels$varName == varName & GADSdat$labels$missings != "miss")
    if(identical(new_meta$labeled, "no")) new_meta <- new_meta[-1, ]
  }

  # insert new meta information, remove old, sort
  labels <- GADSdat$labels
  if(length(remove_rows) > 0) labels <- labels[-remove_rows, ]
  labels <- rbind(labels, new_meta)
  labels <- labels[order(match(labels$varName,names(GADSdat$dat))), ]
  row.names(labels) <- NULL

  out <- new_GADSdat(dat = GADSdat$dat, labels = labels)
  check_GADSdat(out)
  out
}

# drop missing value labels from meta data for a single variable
drop_missing_labels <- function(meta) {
  if(length(unique(meta$varName)) != 1) stop("This function only works for meta information of a single variable.")
  meta_new <- meta[which(meta$missings == "valid"), ]
  if(nrow(meta_new) == 0) {
    meta_new <- meta[1, ]
    meta_new$missings <- meta_new$valLabel <- NA_character_
    meta_new$value <- NA_integer_
    meta_new$labeled <- "no"
  }
  row.names(meta_new) <- NULL
  meta_new
}



# ------------------------------------------------------------------------------------------------------------------------


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
#'@export
getChangeMeta.all_GADSdat <- function(GADSdat, level = "variable") {
  check_all_GADSdat(GADSdat)
  changeSheet_list <- lapply(names(GADSdat$datList), function(nam ) {
    single_GADSdat <- extractGADSdat(GADSdat, name = nam)
    getChangeMeta(single_GADSdat, level = level)
  })
  names(changeSheet_list) <- names(GADSdat$datList)
  changeSheet_list
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
#' Values for which the change columns contain \code{NA} remain unchanged.
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
  # 01) values in data
  dat <- recode_dat(dat = dat, changeTable = changeTable)
  # 02) valueLabels, missings and values in labels
  labels <- recode_labels(labels = labels, changeTable = changeTable)

  new_GADSdat(dat = dat, labels = labels)
}

#'@export
applyChangeMeta.list <- function(changeTable, GADSdat) {
  check_all_GADSdat(GADSdat)
  singleGADS_list <- lapply(names(GADSdat$datList), function(nam ) {
    extractGADSdat(GADSdat, name = nam)
  })
  singleGADS_mod <- Map(applyChangeMeta, changeTable = changeTable, GADSdat = singleGADS_list)
  do.call(mergeLabels, singleGADS_mod)
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

recode_dat <- function(dat, changeTable) {
  value_df <- changeTable[!is.na(changeTable$value_new), c("varName", "value", "value_new")]
  for(nam in unique(value_df[, "varName"])) {
    single_value_df <- value_df[value_df[, "varName"] == nam, ]
    # initialize new vector
    newVec <- dat[, nam]
    for(ro in seq_len(nrow(single_value_df))) {
      oldValue <- single_value_df[ro, "value"]
      newValue <- single_value_df[ro, "value_new"]
      newVec[which(dat[, nam] == oldValue)] <- newValue
    }
    dat[, nam] <- newVec
  }
  dat
}

recode_labels <- function(labels, changeTable) {
  simple_change_vars <- c("valLabel_new", "missings_new", "value_new")
  simpleChanges <- changeTable[, c("varName", simple_change_vars), drop = FALSE]
  # loop over rows and column names, overwrite if not NA
  for(i in seq(nrow(simpleChanges))) {
    simpleChange <- simpleChanges[i, ]
    for(k in simple_change_vars) {
      oldName <- strsplit(k, "_")[[1]][1]
      if(!is.na(simpleChange[, k])) labels[i, oldName] <- simpleChange[, k]
    }
  }
  labels
}

#### Change Variable names
#############################################################################
#' Change Variable Names.
#'
#' Change variable names of a \code{GADSdat} or \code{all_GADSdat} object.
#'
#' Applied to a \code{GADSdat} or \code{all_GADSdat} object, this function is a wrapper of \code{\link{getChangeMeta}} and \code{\link{applyChangeMeta}}
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
#### Note: changeVarNames.all_GADSdat could be blueprint for other changes on all_GADSdat level!
#'@export
changeVarNames.all_GADSdat <- function(GADSdat, oldNames, newNames) {
  changeDF <- data.frame(oldNames = oldNames, newNames = newNames, stringsAsFactors = FALSE)
  out <- list()
  for(i in names(GADSdat[["datList"]])) {
    GADSdat_single <- extractGADSdat(GADSdat, name = i)
    changeDF_single <- changeDF[changeDF$oldNames %in% names(GADSdat[["datList"]][[i]]), ]
    out[[i]] <- changeVarNames(GADSdat = GADSdat_single, oldNames = changeDF_single[["oldNames"]], newNames = changeDF_single[["newNames"]])
  }
  do.call(mergeLabels, out)
}
#'@export
changeVarNames.GADSdat <- function(GADSdat, oldNames, newNames) {
  checkNamesVectors(oldNames = oldNames, newNames = newNames, dat = GADSdat[["dat"]])
  changeTable <- getChangeMeta(GADSdat, level = "variable")
  for(i in seq_along(oldNames)) {
    changeTable[changeTable$varName == oldNames[i], "varName_new"] <- newNames[i]
  }
  applyChangeMeta(GADSdat, changeTable = changeTable)
}


checkNamesVectors <- function(oldNames, newNames, dat) {
  if(length(oldNames) != length(newNames)) stop("oldNames and newNames are not of identical length.", call. = FALSE)
  if(!(is.character(oldNames) && is.character(newNames))) stop("oldNames and newNames are not character vectors.", call. = FALSE)
  if(any(!oldNames %in% names(dat))) stop("varName in oldNames is not a real variable name.", call. = FALSE)
  return()
}



#### Recode variable
#############################################################################
#' Recode a variable.
#'
#' Recode a variable as part of a \code{GADSdat} or \code{all_GADSdat} object.
#'
#' Applied to a \code{GADSdat} or \code{all_GADSdat} object, this function is a wrapper of \code{\link{getChangeMeta}} and \code{\link{applyChangeMeta}}
#'
#'@param GADSdat GADSdat object imported via eatGADS.
#'@param varName Name of the variable to be recoded.
#'@param oldValues Vector containing the old values.
#'@param newValues Vector containing the new values, in identical order as oldNames.
#'
#'@return Returns the GADSdat object with changed variable names.
#'
#'@examples
#'# Example data set
#'#to be done
#'
#'@export
recodeGADS <- function(GADSdat, varName, oldValues, newValues) {
  UseMethod("recodeGADS")
}
#'@export
recodeGADS.GADSdat <- function(GADSdat, varName, oldValues, newValues) {
  checkRecodeVectors(oldValues = oldValues, newValues = newValues, varName = varName, dat = GADSdat$dat)
  changeTable <- getChangeMeta(GADSdat, level = "value")
  for(i in seq_along(oldValues)) {
    changeTable[changeTable$varName == varName & changeTable$value == oldValues[i], "value_new"] <- newValues[i]
  }
  applyChangeMeta(GADSdat, changeTable = changeTable)
}

#'@export
recodeGADS.all_GADSdat <- function(GADSdat, varName, oldValues, newValues) {
  check_all_GADSdat(GADSdat)
  singleGADS_list <- lapply(names(GADSdat$datList), function(nam ) {
    singleGADS <- extractGADSdat(GADSdat, name = nam)
    if(varName %in% names(singleGADS$dat)) singleGADS <- recodeGADS(singleGADS, varName = varName, oldValues = oldValues, newValues = newValues)
    singleGADS
  })
  names(singleGADS_list) <- names(GADSdat$datList)
  do.call(mergeLabels, singleGADS_list)
}

checkRecodeVectors <- function(oldValues, newValues, varName, dat) {
  if(length(oldValues) != length(newValues)) stop("oldValues and newValues are not of identical length.", call. = FALSE)
  if(!varName %in% names(dat)) stop("varName is not a real variable name.", call. = FALSE)
  return()
}



#### Change variable label
#############################################################################
#' Change the variable label.
#'
#' Change the variable label of a variable as part of a \code{GADSdat} or \code{all_GADSdat} object.
#'
#' Applied to a \code{GADSdat} or \code{all_GADSdat} object, this function is a wrapper of \code{\link{getChangeMeta}} and \code{\link{applyChangeMeta}}
#'
#'@param GADSdat GADSdat object imported via eatGADS.
#'@param varName Character string of variable names.
#'@param varLabel Character string of the new variable labels.
#'
#'@return Returns the GADSdat object with changed meta data..
#'
#'@examples
#'# Example data set
#'#to be done
#'
#'@export
changeVarLabels <- function(GADSdat, varName, varLabel) {
  UseMethod("changeVarLabels")
}
#'@export
changeVarLabels.GADSdat <- function(GADSdat, varName, varLabel) {
  checkVarLabelVector(varName = varName, varLabel = varLabel, dat = GADSdat$dat)
  changeTable <- getChangeMeta(GADSdat, level = "variable")
  for(i in seq_along(varName)) {
    changeTable[changeTable$varName == varName[i], "varLabel_new"] <- varLabel[i]
  }
  applyChangeMeta(GADSdat, changeTable = changeTable)
}

#'@export
changeVarLabels.all_GADSdat <- function(GADSdat, varName, varLabel) {
  stop("This method has not been implemented yet")
}

checkVarLabelVector <- function(varName, varLabel, dat) {
  if(!is.character(varName) || !is.character(varLabel)) stop("varName and varLabel are not character vectors.")
  if(length(varName) != length(varLabel)) stop("varName and varLabel are not of identical length.", call. = FALSE)
  if(!all(varName %in% names(dat))) stop("varName is not a real variable name.", call. = FALSE)
  return()
}

