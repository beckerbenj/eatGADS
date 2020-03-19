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
#'@param addMissingLabel If \code{TRUE}, \code{"generic missing"} is added according to occurence of \code{"mis"} in \code{"missings"}. As often various value labels for missings are used, this argument should be used with great care.
#'
#'@return Returns a GADSdat object.
#'
#'@examples
#'# Example data set
#'#to be done
#'
#'@export
checkMissings <- function(GADSdat, missingLabel = "missing", addMissingCode = TRUE, addMissingLabel = FALSE) {
  UseMethod("checkMissings")
}

#'@export
checkMissings.GADSdat <- function(GADSdat, missingLabel = "missing", addMissingCode = TRUE, addMissingLabel = FALSE) {
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
    message("The following variables have values coded as missings but value labels do not include the term '", missingLabel ,"':\n",
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




#### Merge GADSdat objects
#############################################################################
#' Merge two GADSdat objects into a single GADSdat object.
#'
#' Is a secure way to merge the data and the meta data of two GADSdat objects. Only a very specific way of merging is possible.
#'
#' If there are variables duplicate (the variables specified in by are excempt), these variables are removed from y. The data is merged via a full join. The meta data is joined for the remaining variables via rbind.
#'
#'@param x \code{GADSdat} object imported via eatGADS.
#'@param y \code{GADSdat} object imported via eatGADS.
#'@param by A character vector.
#'@param all A character vector (either a full join or an inner join).
#'@param all.x See merge.
#'@param all.y See merge.
#'@param ... Further arguments are currently not supported but have to be included for R CMD checks.
#'
#'@return Returns a GADSdat object.
#'
#'@examples
#'# Example data set
#'#to be done
#'
#'@export
merge.GADSdat <- function(x, y, by, all = TRUE, all.x = all, all.y = all, ...) {
  check_GADSdat(x)
  check_GADSdat(y)
  if(!is.character(by)) stop(by, " is not a character vector.")
  if(!all(by %in% names(x$dat))) stop(by, " is not a variable in x.")
  if(!all(by %in% names(y$dat))) stop(by, " is not a variable in y.")
  # drop double variables from y
  y_vars <- c(by, names(y$dat)[!names(y$dat) %in% names(x$dat)])
  if(!length(y_vars) > length(by)) stop("y does not contain unique variables.")

  newDat <- merge(x$dat, y$dat[, y_vars], by = by, all = all, all.x = all.x, all.y = all.y)
  newLabels <- rbind(x$labels, y$labels[y$labels$varName %in% y_vars[!y_vars %in% by], ])

  newGADS <- new_GADSdat(dat = newDat, labels = newLabels)
  check_GADSdat(GADSdat = newGADS)
  newGADS
}


#### Order like a character vector
#############################################################################
#' Order the variables in a GADSdat.
#'
#' Order the variables in a GADSdat according to a character vector. If there are discrepancies between the two sets, a warning is issued.
#'
#' The variables in the \code{dat} and in the \code{labels} section are ordered. Variables not contained in the character vector are moved to the end of the data.
#'
#'@param GADSdat \code{GADSdat} object imported via eatGADS.
#'@param newOrder A character vector containing the order of variables.
#'
#'@return Returns a GADSdat object.
#'
#'@examples
#'# Example data set
#'#to be done
#'
#'@export
orderLike <- function(GADSdat, newOrder) {
  UseMethod("orderLike")
}
#'@export
orderLike.GADSdat <- function(GADSdat, newOrder) {
  check_GADSdat(GADSdat)
  if(!is.character(newOrder) || !length(newOrder) > 0) stop("newOrder is not a character vector.")

  comp <- compare_and_order(set1 = names(GADSdat$dat), set2 = newOrder, name1 = "GADSdat", name2 = "new Order")

  newDat <- GADSdat$dat[, c(comp[["in_both_ordered"]], comp[["not_in_set2"]])]
  newLabels <- GADSdat$labels[order(match(GADSdat$labels$varName, names(newDat))), ]
  new_GADSdat(dat = newDat, labels = newLabels)
}


compare_and_order <- function(set1, set2, name1 = "set1", name2 = "set2", FUN = warning) {
  not_in_set1 <- setdiff(set2, set1)
  not_in_set2 <- setdiff(set1, set2)
  in_both_ordered <- intersect(set2, set1)

  call. <- FALSE
  if(identical(FUN, message)) call. <- NULL

  if(length(not_in_set1) > 0) FUN("The following variables are not in ", name1, ": ", paste(not_in_set1, collapse = ", "), call. = call.)
  if(length(not_in_set2) > 0) FUN("The following variables are not in ", name2, ": ", paste(not_in_set2, collapse = ", "), call. = call.)
  list(not_in_set1 = not_in_set1, not_in_set2 = not_in_set2, in_both_ordered = in_both_ordered)
}



#### Remove variables from a GADSdat
#############################################################################
#' Remove variables from a GADSdat.
#'
#' Remove variables and their meta data from a \code{GADSdat} object.
#'
#' Wraps removing the variable from the data.frame in the \code{GADSdat} object and \code{\link{updateMeta}}.
#'
#'@param GADSdat \code{GADSdat} object.
#'@param vars A character vector containing the variables to be removed.
#'
#'@return Returns a GADSdat object.
#'
#'@examples
#'# Example data set
#'#to be done
#'
#'@export
removeVars <- function(GADSdat, vars) {
  UseMethod("removeVars")
}
#'@export
removeVars.GADSdat <- function(GADSdat, vars) {
  check_GADSdat(GADSdat)
  if(!all(vars %in% namesGADS(GADSdat))) stop("All 'vars' have to be variables in the GADSdat.")

  new_dat <- GADSdat$dat[, !names(GADSdat$dat) %in% vars, drop = FALSE]
  updateMeta(GADSdat, newDat = new_dat)
}

