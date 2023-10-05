
#### Apply Change Meta data
#############################################################################
#' Apply Meta Data Changes.
#'
#' Function to apply meta data changes to a \code{GADSdat} object specified by a change table extracted by \code{\link{getChangeMeta}}.
#'
#' Values for which the change columns contain \code{NA} remain unchanged. If changes are performed on value levels, recoding into
#' existing values can occur. In these cases, \code{existingMeta} determines how the resulting meta data conflicts are handled,
#' either raising an error if any occur (\code{"stop"}),
#' keeping the original meta data for the value (\code{"value"}),
#' using the meta data in the \code{changeTable} and, if incomplete, from the recoded value (\code{"value_new"}),
#' or leaving the respective meta data untouched (\code{"ignore"}).
#'
#' Furthermore, one might recode multiple old values in the same new value. This is currently only possible with
#' \code{existingMeta = "drop"}, which drops all related meta data on value level, or
#' \code{existingMeta = "ignore"}, which leaves all related meta data on value level untouched.
#'
#'@param changeTable Change table as provided by \code{\link{getChangeMeta}}.
#'@param GADSdat \code{GADSdat} object imported via \code{eatGADS}.
#'@param checkVarNames Logical. Should new variable names be checked by \code{\link{checkVarNames}}?
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
#'@export
applyChangeMeta <- function(changeTable, GADSdat, ...) {
  UseMethod("applyChangeMeta")
}

#'@rdname applyChangeMeta
#'@export
applyChangeMeta.varChanges <- function(changeTable, GADSdat, checkVarNames = TRUE, ...) {
  check_GADSdat(GADSdat)
  changeTable <- check_varChanges(changeTable, checkVarNames = checkVarNames)
  check_changeTable(GADSdat, changeTable)
  check_format_vector(changeTable$format_new)
  check_logicalArgument(checkVarNames, argName = checkVarNames)

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
  check_GADSdat_varLevel_meta(out_GADSdat)
  out_GADSdat
}

#'@rdname applyChangeMeta
#'@export
applyChangeMeta.valChanges <- function(changeTable, GADSdat, existingMeta = c("stop", "value", "value_new", "drop", "ignore"), ...) {
  check_GADSdat(GADSdat)
  changeTable <- check_valChanges(changeTable)
  existingMeta <- match.arg(existingMeta)
  # check_changeTable(GADSdat, changeTable) ### removed; substitute with checks that work around new value labels...
  test <- compare_and_order(namesGADS(GADSdat), name1 = "the 'GADSdat' but in the 'changeTable'",
                            set2 = unique(changeTable$varName), name2 = "the 'changeTable' but in the 'GADSdat'", FUN = stop)
  # tbd: check for variable-value combination!

  dat <- GADSdat$dat
  labels <- GADSdat$labels
  # 01) values in data
  dat <- recode_dat(dat = dat, changeTable = changeTable)
  # 02) valueLabels, missings and values in labels
  labels2 <- recode_labels(labels = labels, changeTable = changeTable, existingMeta = existingMeta)

  out_GADSdat <- new_GADSdat(dat = dat, labels = labels2)
  check_GADSdat(out_GADSdat)
  #check_GADSdat_varLevel_meta(out_GADSdat)
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
  test <- compare_and_order(namesGADS(GADSdat), name1 = "the 'GADSdat' but in the 'changeTable'",
                    set2 = unique(changeTable$varName), name2 = "the 'changeTable' but in the 'GADSdat'", FUN = stop)

  newVars <- grep("_new", names(changeTable), value = TRUE)
  oldVars <- setdiff(names(changeTable), newVars)
  oldDat <- unique(GADSdat$labels[, oldVars])
  newDat <- unique(changeTable[, oldVars])
  class(newDat) <- "data.frame"

  row.names(oldDat) <- row.names(newDat) <- NULL

  for(i in names(oldDat)) {
    unequal_rows <- which(oldDat[[i]] != newDat[[i]] |
                            (is.na(oldDat[[i]]) & !is.na(newDat[[i]])) |
                            (!is.na(oldDat[[i]]) & is.na(newDat[[i]])))
    if(length(unequal_rows) > 0) stop("GADSdat and changeTable are not compatible in column '", i, "' and row(s) ",
                                  paste(unequal_rows, collapse = ", "),
                                  ". Columns without '_new' should not be changed in the changeTable.", call. = FALSE)
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
  changeTable <- changeTable[order(match(changeTable$varName, unique(labels$varName))), ]
  simple_change_vars <- c("valLabel_new", "missings_new", "value_new")
  simpleChanges <- changeTable[, c("varName", "value", simple_change_vars), drop = FALSE]
  simpleChanges[, "row_num_in_labels"] <- seq(nrow(simpleChanges))
  simpleChanges <- simpleChanges[!(is.na(simpleChanges$valLabel_new) &
                                     is.na(simpleChanges$missings_new) &
                                     is.na(simpleChanges$value_new)), ]
  if(nrow(simpleChanges) == 0) return(labels)

  # split labels and merge & sort later
  vars_with_changes <- unique(simpleChanges$varName)
  varName_in_simpleChanges <- labels$varName %in% vars_with_changes
  untouched_labels <- labels[!varName_in_simpleChanges, ]
  modify_labels <- labels[varName_in_simpleChanges, ]
  #browser()
  modify_labels <- expand_labels(modify_labels, new_varName_vec = changeTable$varName[changeTable$varName %in% vars_with_changes])

  modify_labels_list <- split(modify_labels, factor(modify_labels$varName, levels = unique(modify_labels$varName)))
  modify_labels_list2 <- lapply(modify_labels_list, function(single_labels) {
    var_name <- unique(single_labels$varName)
    if(!var_name %in% simpleChanges$varName) return(single_labels)

    single_simpleChanges <- simpleChanges[simpleChanges$varName == var_name, ]

    ## Deal with meta data conflicts:
    values_being_recoded <- labels[single_simpleChanges[, "row_num_in_labels"], "value"]

    # relevant conflicts: existing values that are not themselves being recoded
    existing_value_logical <- !is.na(single_simpleChanges$value_new) &
      single_simpleChanges$value_new %in% single_labels$value &
      !single_simpleChanges$value_new %in% values_being_recoded
    existing_value_vec <- single_simpleChanges$value_new[existing_value_logical]
    remove_rows <- numeric()

    # meta data conflicts only with new values
    recode_values <- single_simpleChanges$value_new[!is.na(single_simpleChanges$value_new)]
    only_new_recode_values <- recode_values[!recode_values %in% existing_value_vec]
    dup_recode_values <- unique(only_new_recode_values[duplicated(only_new_recode_values)])
    dup_recode_values <- dup_recode_values[!dup_recode_values %in% single_simpleChanges[existing_value_logical, "value"]]
    new_dup_value_vec <- !is.na(single_simpleChanges$value_new) & single_simpleChanges$value_new %in% dup_recode_values

    # ignore behavior: reset all changes for duplicate and existing value labels & missing tags
    if(identical(existingMeta, "ignore")) {
      single_simpleChanges[new_dup_value_vec | existing_value_logical, "value_new"] <- NA
    }

    if(length(dup_recode_values) > 0 && !all(dup_recode_values %in% existing_value_vec)){
      if(!(identical(existingMeta, "drop") || identical(existingMeta, "ignore"))) {
        all_values <- existing_value_vec
        stop("Duplicated values in 'value_new' causing conflicting meta data in variable ", var_name, ": ",
             paste(dup_recode_values, collapse = ", "), ". Use existingMeta = 'drop' or 'ignore' to drop all related meta data.")
      }
      # drop behavior
      if(identical(existingMeta, "drop")) {
        remove_value_meta <- single_simpleChanges[new_dup_value_vec, "value"]
        drop_meta_rows <- which(single_labels$value %in% remove_value_meta)
        single_labels[drop_meta_rows, c("valLabel", "missings")] <- NA
      }
    }

    # meta data conflicts with old values (and optionally new values)
    if(any(existing_value_logical)){
      if(identical(existingMeta, "stop")) {
        all_values <- existing_value_vec
        stop("Values in 'value_new' with existing meta data in variable ", var_name, ": ",
                                               paste(all_values, collapse = ", "))
      }
      if(identical(existingMeta, "value")) {
        # remove meta data of value which is being recoded
        remove_value_meta <- existing_value_vec
        remove_rows <- which(single_labels$value %in% remove_value_meta)
        dups <- duplicated(remove_value_meta)
        if(any(dups)) stop("Multiple values are recoded into ", remove_value_meta[dups], " for variable ",
                           var_name, ". Value meta data can thus not be used from 'value'. Set existingMeta = 'value_new'.")
      }
      if(identical(existingMeta, "value_new")) {
        # remove meta data of value which is being recoded
        remove_value_meta <- existing_value_vec
        remove_rows <- which(single_labels$value %in% remove_value_meta)
        # if no new value labels or missing codes are specified, recode new meta data rows based on old ones
        single_simpleChanges[existing_value_logical, "valLabel_new"] <- ifelse(is.na(single_simpleChanges[existing_value_logical, "valLabel_new"]),
                                                         yes = single_labels[remove_rows, "valLabel"],
                                                         no = single_simpleChanges[existing_value_logical, "valLabel_new"])
        single_simpleChanges[existing_value_logical, "missings_new"] <- ifelse(is.na(single_simpleChanges[existing_value_logical, "missings_new"]),
                                                                           yes = single_labels[remove_rows, "missings"],
                                                                           no = single_simpleChanges[existing_value_logical, "missings_new"])
      }
      if(identical(existingMeta, "drop")) {
        #remove_value_meta <- single_simpleChanges[existing_value_logical, "value_new"]
        remove_value_meta <- existing_value_vec
        remove_value_meta <- unique(c(remove_value_meta, single_simpleChanges[existing_value_logical, "value"]))
        drop_meta_rows <- which(single_labels$value %in% remove_value_meta)
        single_labels[drop_meta_rows, c("valLabel", "missings")] <- NA
      }
    }

    #if(identical(existingMeta, "ignore")) browser()
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
    # unique rows necessary because of multiple recodes into the same value
    single_labels[, "missings"] <- ifelse(!is.na(single_labels$value) & is.na(single_labels$missings),
                                       yes = "valid", no = single_labels$missings)
    unique(sort_value_labels(single_labels))
  })
  modify_labels <- do.call(rbind, modify_labels_list2)
  labels_new <- data.table::as.data.table(rbind(untouched_labels, modify_labels))
  labels_new <- as.data.frame(labels_new[order(match(labels_new$varName, unique(labels$varName))), ])

  # fix labeled column
  # important: this should change a variable to "labeled" to export it later properly to spss
  labels_new[, "labeled"] <- ifelse(!is.na(labels_new[, "value"]), yes = "yes", no = labels_new[, "labeled"])

  rownames(labels_new) <- NULL
  labels_new
}

# if value labels are added, this function adds the necessary rows in the labels df, that are later filled with new values & labels
expand_labels <- function(labels, new_varName_vec) {
  if(nrow(labels) == length(new_varName_vec)) return(labels)

  old_order <- unique(labels$varName)
  new_row_list <- by(labels, labels$varName, function(labels_sub) {
    i <- unique(labels_sub$varName)
    no_rows_2add <- sum(new_varName_vec == i) - nrow(labels_sub)
    if(no_rows_2add > 0) {
      #browser()
      new_sub_label_rows <- (nrow(labels_sub) + 1):(nrow(labels_sub) + no_rows_2add)
      labels_sub[new_sub_label_rows, ] <- labels_sub[1, ]
      labels_sub[new_sub_label_rows, c("value", "valLabel")] <- NA
      labels_sub[new_sub_label_rows, "missings"] <- "valid"
      return(labels_sub[new_sub_label_rows, ])
    }
  NULL
  })
  labels_out <- rbind(labels, do.call(rbind, new_row_list))
  labels_out[order(match(labels_out$varName, old_order)), ]
}


# updates the labeled column in the meta data according to the value column
# currently not in use, was to strict (problems when some values were NA (originally strings) in labels, some not)
update_labeled_col <- function(labels) {
  labels$labeled <- ifelse(is.na(labels$value), yes = "no", no = "yes")
  labels
}

sort_value_labels <- function(value_labels) {
  value_labels <- value_labels[order(value_labels[, c("value")]), ]
  row.names(value_labels) <- NULL
  value_labels
}

