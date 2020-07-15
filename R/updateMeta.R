#### Update Meta
#############################################################################
#' Update Meta data frame.
#'
#' If the data of a \code{GADSdat} or a \code{all_GADSdat} has changed, update assimilates the corresponding meta data set.
#' Factors are transformed to numerical and their levels added to the meta data set. Careful, this is a development version and should
#' be only used with great care!
#'
#' tbd.
#'
#'@param GADSdat \code{GADSdat} or \code{all_GADSdat} object imported via \code{eatGADS}.
#'@param newDat \code{data.frame} or list of \code{data.frames} with the modified data.
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

