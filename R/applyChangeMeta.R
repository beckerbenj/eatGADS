
#### Apply Change Meta data
#############################################################################
#' Apply Meta Data Changes.
#'
#' Function to apply meta data changes to a \code{GADSdat} object specified by a change table extracted by \code{\link{getChangeMeta}}.
#'
#' Values for which the change columns contain \code{NA} remain unchanged.
#'
#'@param changeTable Change table as provided by \code{\link{getChangeMeta}}.
#'@param GADSdat \code{GADSdat} object imported via \code{eatGADS}.
#'
#'@return Returns the modified \code{GADSdat} object.
#'
#'@examples
#'# Change a variable name and label
#'varChangeTable <- getChangeMeta(pisa, level = "variable")
#'varChangeTable[1, c("varName_new", "varLabel_new")] <- c("IDstud", "Person ID")
#'pisa2 <- applyChangeMeta(varChangeTable, GADSdat = pisa)
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
  check_format_vector(changeTable$format_new)

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

  out_GADSdat <- new_GADSdat(dat = dat, labels = labels)
  check_GADSdat(out_GADSdat)
  out_GADSdat
}

#'@export
applyChangeMeta.valChanges <- function(changeTable, GADSdat) {
  check_GADSdat(GADSdat)
  check_valChanges(changeTable)
  # check_changeTable(GADSdat, changeTable) ### removed; substitute with checks that work around new value labels...
  dat <- GADSdat$dat
  labels <- GADSdat$labels
  # 01) values in data
  dat <- recode_dat(dat = dat, changeTable = changeTable)
  # 02) valueLabels, missings and values in labels
  labels <- recode_labels(labels = labels, changeTable = changeTable)

  # 03) if variable was unlabeled before, set to labeled
  labels2 <- update_labeled_col(labels)

  out_GADSdat <- new_GADSdat(dat = dat, labels = labels)
  check_GADSdat(out_GADSdat)
  out_GADSdat
}


## Workaround for tibbles or data.frames, as readxl is recommended for excel re-import (or other excel imports might be used)
#'@export
applyChangeMeta.tbl_df <- function(changeTable, GADSdat) {
  if(all(c("varName", "varName_new") %in% names(changeTable))) changeTable_new <- new_varChanges(changeTable)
  if(all(c("value", "value_new") %in% names(changeTable))) changeTable_new <- new_valChanges(changeTable)
  applyChangeMeta(changeTable_new, GADSdat = GADSdat)
}
#'@export
applyChangeMeta.data.frame <- function(changeTable, GADSdat) {
  if(all(c("varName", "varName_new") %in% names(changeTable))) changeTable_new <- new_varChanges(changeTable)
  if(all(c("value", "value_new") %in% names(changeTable))) changeTable_new <- new_valChanges(changeTable)
  applyChangeMeta(changeTable_new, GADSdat = GADSdat)
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

  row.names(oldDat) <- row.names(newDat) <- NULL
  #if(length(namesGADS(GADSdat)) > 50) browser()

  for(i in names(oldDat)) {
    if(length(which(oldDat[i] != newDat[i]))) stop("GADSdat and changeTable are not compatible in column '", i, "'. Columns without '_new' should not be changed in the changeTable.", call. = FALSE)
  }

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
  labels <- expand_labels(labels, new_varName_vec = changeTable$varName)

  changeTable <- changeTable[order(match(changeTable$varName, unique(labels$varName))), ]
  simple_change_vars <- c("valLabel_new", "missings_new", "value_new")
  simpleChanges <- changeTable[, c("varName", simple_change_vars), drop = FALSE]
  simpleChanges[, "row_num_in_labels"] <- seq(nrow(simpleChanges))
  simpleChanges <- simpleChanges[!(is.na(simpleChanges$valLabel_new) &
                                     is.na(simpleChanges$missings_new) &
                                     is.na(simpleChanges$value_new)), ]
  if(nrow(simpleChanges) == 0) return(labels)

  # loop over rows and column names, overwrite if not NA
  for(i in seq(nrow(simpleChanges))) {
    simpleChange <- simpleChanges[i, ]
    for(k in simple_change_vars) {
      oldName <- strsplit(k, "_")[[1]][1]
      if(!is.na(simpleChange[, k])) labels[simpleChange$row_num_in_labels, oldName] <- simpleChange[, k]
    }
  }

  # fix labeled column
  # important: this should change a variable to "labeled" to export it later properly to spss
  labels[, "labeled"] <- ifelse(!is.na(labels[, "value"]), yes = "yes", no = labels[, "labeled"])

  labels
}

# if value labels are added, this function adds the necessary rows in the labels df, that are later filled with new values & labels
expand_labels <- function(labels, new_varName_vec) {
  old_order <- unique(labels$varName)
  new_row_list <- by(labels, labels$varName, function(labels_sub) {
    i <- unique(labels_sub$varName)
    no_rows_2add <- sum(new_varName_vec == i) - nrow(labels_sub)
    if(no_rows_2add > 0) {
      #browser()
      new_sub_label_rows <- (nrow(labels_sub) + 1):(nrow(labels_sub) + no_rows_2add)
      labels_sub[new_sub_label_rows, ] <- labels_sub[1, ]
      labels_sub[new_sub_label_rows, c("value", "valLabel", "missings")] <- NA
      return(labels_sub[new_sub_label_rows, ])
    }
  NULL
  })
  labels_out <- rbind(labels, do.call(rbind, new_row_list))
  labels_out[order(match(labels_out$varName, old_order)), ]
}


# updates the labeled column in the meta data according to the value column
update_labeled_col <- function(labels) {
  labels$labeled <- ifelse(is.na(labels$value), yes = "no", no = "yes")
  labels
}
