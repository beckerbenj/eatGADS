
#### Apply Change Meta data
#############################################################################
#' Apply Meta Data Changes.
#'
#' Function to apply meta data changes to a \code{GADSdat} object specified by a change table extracted by \code{\link{getChangeMeta}}.
#'
#' Values for which the change columns contain \code{NA} remain unchanged. If changes are performed on value levels, recoding into
#' existing values can occur. In these cases, \code{existingMeta} determines how the resulting meta data conflicts are handled,
#' either raising an error if any occur (\code{"stop"}), keeping the original meta data for the value (\code{"value"}) or using the meta
#' data in the \code{changeTable} or, if incomplete, from the recoded value (\code{"value_new"}).
#'
#'@param changeTable Change table as provided by \code{\link{getChangeMeta}}.
#'@param GADSdat \code{GADSdat} object imported via \code{eatGADS}.
#'@param existingMeta If values are recoded, which meta data should be used (see details)?
#'@param ... further arguments passed to or from other methods.
#'
#'@return Returns the modified \code{GADSdat} object.
#'
#'@examples
#'# Change a variable name and label
#'varChangeTable <- getChangeMeta(pisa, level = "variable")
#'varChangeTable[1, c("varName_new", "varLabel_new")] <- c("IDstud", "Person ID")
#'
#'pisa2 <- applyChangeMeta(varChangeTable, GADSdat = pisa)
#'
#'@export
applyChangeMeta <- function(changeTable, GADSdat, ...) {
  UseMethod("applyChangeMeta")
}

#'@rdname applyChangeMeta
#'@export
applyChangeMeta.varChanges <- function(changeTable, GADSdat, ...) {
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

#'@rdname applyChangeMeta
#'@export
applyChangeMeta.valChanges <- function(changeTable, GADSdat, existingMeta = c("stop", "value", "value_new"), ...) {
  check_GADSdat(GADSdat)
  check_valChanges(changeTable)
  existingMeta <- match.arg(existingMeta)
  # check_changeTable(GADSdat, changeTable) ### removed; substitute with checks that work around new value labels...
  dat <- GADSdat$dat
  labels <- GADSdat$labels
  # 01) values in data
  dat <- recode_dat(dat = dat, changeTable = changeTable)
  # 02) valueLabels, missings and values in labels
  labels <- recode_labels(labels = labels, changeTable = changeTable, existingMeta = existingMeta)

  # 03) if variable was unlabeled before, set to labeled
  labels2 <- update_labeled_col(labels)

  out_GADSdat <- new_GADSdat(dat = dat, labels = labels2)
  check_GADSdat(out_GADSdat)
  out_GADSdat
}


## Workaround for tibbles or data.frames, as readxl is recommended for excel re-import (or other excel imports might be used)
#'@export
applyChangeMeta.tbl_df <- function(changeTable, GADSdat, ...) {
  if(all(c("varName", "varName_new") %in% names(changeTable))) changeTable_new <- new_varChanges(changeTable)
  if(all(c("value", "value_new") %in% names(changeTable))) changeTable_new <- new_valChanges(changeTable)
  applyChangeMeta(changeTable_new, GADSdat = GADSdat)
}
#'@export
applyChangeMeta.data.frame <- function(changeTable, GADSdat, ...) {
  if(all(c("varName", "varName_new") %in% names(changeTable))) changeTable_new <- new_varChanges(changeTable)
  if(all(c("value", "value_new") %in% names(changeTable))) changeTable_new <- new_valChanges(changeTable)
  applyChangeMeta(changeTable_new, GADSdat = GADSdat)
}


#'@export
applyChangeMeta.list <- function(changeTable, GADSdat, ...) {
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

recode_labels <- function(labels, changeTable, existingMeta) {
  labels <- expand_labels(labels, new_varName_vec = changeTable$varName)

  changeTable <- changeTable[order(match(changeTable$varName, unique(labels$varName))), ]
  simple_change_vars <- c("valLabel_new", "missings_new", "value_new")
  simpleChanges <- changeTable[, c("varName", "value", simple_change_vars), drop = FALSE]
  simpleChanges[, "row_num_in_labels"] <- seq(nrow(simpleChanges))
  simpleChanges <- simpleChanges[!(is.na(simpleChanges$valLabel_new) &
                                     is.na(simpleChanges$missings_new) &
                                     is.na(simpleChanges$value_new)), ]
  if(nrow(simpleChanges) == 0) return(labels)

  labels_list <- split(labels, factor(labels$varName, levels = unique(labels$varName)))
  labels <- do.call(rbind, lapply(labels_list, function(single_labels) {
    var_name <- unique(single_labels$varName)
    if(!var_name %in% simpleChanges$varName) return(single_labels)

    single_simpleChanges <- simpleChanges[simpleChanges$varName == var_name, ]

    ## Deal with meta data conflicts:
    # if values will be recoded into each other, do not raise meta data conflict!
    existing_value_vec <- !is.na(single_simpleChanges$value_new) & single_simpleChanges$value_new %in% single_labels$value
    values_to_be_recoded <- labels[single_simpleChanges[, "row_num_in_labels"], "value"]
    existing_value_vec <- existing_value_vec & !single_simpleChanges$value_new %in% values_to_be_recoded
    remove_rows <- numeric()

    if(any(existing_value_vec)){
      if(identical(existingMeta, "stop")) {
        all_values <- single_simpleChanges[existing_value_vec, "value_new"]
        stop("Values in 'value_new' with existing meta data in variable ", var_name, ": ",
                                               paste(all_values, collapse = ", "))
      }
      if(identical(existingMeta, "value")) {
        # remove meta data of value which is being recoded
        remove_value_meta <- single_simpleChanges[existing_value_vec, "value_new"]
        remove_rows <- which(single_labels$value %in% remove_value_meta)
        dups <- duplicated(remove_value_meta)
        if(any(dups)) stop("Multiple values are recoded into ", remove_value_meta[dups], " for variable ", var_name, ". Value meta data can thus not be used from 'value'. Set 'existingMeta' to 'value_new'.")
      }
      if(identical(existingMeta, "value_new")) {
        # remove meta data of value which is being recoded
        remove_value_meta <- single_simpleChanges[existing_value_vec, "value_new"]
        remove_rows <- which(single_labels$value %in% remove_value_meta)
        # if no new value labels or missing codes are specified, recode new meta data rows based on old ones
        single_simpleChanges[existing_value_vec, "valLabel_new"] <- ifelse(is.na(single_simpleChanges[existing_value_vec, "valLabel_new"]),
                                                         yes = single_labels[remove_rows, "valLabel"],
                                                         no = single_simpleChanges[existing_value_vec, "valLabel_new"])
        single_simpleChanges[existing_value_vec, "missings_new"] <- ifelse(is.na(single_simpleChanges[existing_value_vec, "missings_new"]),
                                                                           yes = single_labels[remove_rows, "missings"],
                                                                           no = single_simpleChanges[existing_value_vec, "missings_new"])
      }
    }

    ## Actual recoding: loop over rows and column names, overwrite if not NA
    old_single_labels <- single_labels
    for(i in seq(nrow(single_simpleChanges))) {
      simpleChange <- single_simpleChanges[i, ]
      for(k in simple_change_vars) {
        oldName <- strsplit(k, "_")[[1]][1]
        #if(sum(match(old_single_labels$value, simpleChange$value), na.rm = T) > 1) browser()
        if(!is.na(simpleChange[, k])) single_labels[match(simpleChange$value, old_single_labels$value), oldName] <- simpleChange[, k]
        # for expand labels multiple "values" match NAs -> set these to other values after full recode circle
        if(oldName == "value" && is.na(simpleChange$value)) old_single_labels[match(simpleChange$value, old_single_labels$value), "value"] <- -9999
      }
    }

  if(length(remove_rows) > 0) single_labels <- single_labels[-remove_rows, ]
    sort_value_labels(single_labels)
  }))

  # fix labeled column
  # important: this should change a variable to "labeled" to export it later properly to spss
  labels[, "labeled"] <- ifelse(!is.na(labels[, "value"]), yes = "yes", no = labels[, "labeled"])
  # unique rows necessary because of multiple recodes into the same value
  labels <- unique(labels)
  rownames(labels) <- NULL
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

sort_value_labels <- function(value_labels) {
  value_labels <- value_labels[order(value_labels[, c("value")]), ]
  row.names(value_labels) <- NULL
  value_labels
}

