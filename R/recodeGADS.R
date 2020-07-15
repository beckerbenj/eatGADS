#### Recode variable
#############################################################################
#' Recode a variable.
#'
#' Recode a variable as part of a \code{GADSdat} or \code{all_GADSdat} object.
#'
#' Applied to a \code{GADSdat} or \code{all_GADSdat} object, this function is a wrapper of \code{\link{getChangeMeta}} and \code{\link{applyChangeMeta}}
#'
#'@param GADSdat \code{GADSdat} object imported via \code{eatGADS}.
#'@param varName Name of the variable to be recoded.
#'@param oldValues Vector containing the old values.
#'@param newValues Vector containing the new values, in identical order as \code{oldNames}.
#'@param newValueLabels Named vector containing new value labels for the new values. All new values have to get labels. Slightly experimental implementation.
#'
#'@return Returns the \code{GADSdat} object with changed variable names.
#'
#'@examples
#'# Example data set
#'#to be done
#'
#'@export
recodeGADS <- function(GADSdat, varName, oldValues, newValues, newValueLabels = NULL) {
  UseMethod("recodeGADS")
}
#'@export
recodeGADS.GADSdat <- function(GADSdat, varName, oldValues, newValues, newValueLabels = NULL) {
  checkRecodeVectors(oldValues = oldValues, newValues = newValues, varName = varName, dat = GADSdat$dat)
  changeTable <- getChangeMeta(GADSdat, level = "value")
  for(i in seq_along(oldValues)) {
    changeTable[changeTable$varName == varName & changeTable$value == oldValues[i], "value_new"] <- newValues[i]
  }
  out <- applyChangeMeta(GADSdat, changeTable = changeTable)

  ### modify these parts so they use the enhanced applyChangeMeta functionality?
  if(!is.null(newValueLabels)) {
    checkNewValueLabels(newValueLabels = newValueLabels, newValues = newValues)
    checkNewValueLabels(newValueLabels = newValueLabels, newValues = unique(out$labels[out$labels$varName == varName, "value"]))
    labels_without_var <- out$labels[out$labels$varName != varName, ]
    blank_label <- out$labels[out$labels$varName == varName, ][1, ]

    for(i in unique(newValues)) {
      new_value_label <- blank_label
      new_value_label[, "value"] <- i
      new_value_label[, "valLabel"] <- newValueLabels[as.character(i)]
      labels_without_var <- rbind(labels_without_var, new_value_label)
    }
    labels_without_var <- labels_without_var[order(match(labels_without_var$varName, names(out$dat))), ]
    out <- new_GADSdat(out$dat, labels = labels_without_var)
  }
  out
}

#'@export
recodeGADS.all_GADSdat <- function(GADSdat, varName, oldValues, newValues, newValueLabels = NULL) {
  check_all_GADSdat(GADSdat)
  singleGADS_list <- lapply(names(GADSdat$datList), function(nam ) {
    singleGADS <- extractGADSdat(GADSdat, name = nam)
    if(varName %in% names(singleGADS$dat)) singleGADS <- recodeGADS(singleGADS, varName = varName, oldValues = oldValues, newValues = newValues,
                                                                    newValueLabels = newValueLabels)
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

checkNewValueLabels <- function(newValueLabels, newValues) {
  if(!is.character(newValueLabels)) stop("newValueLabels is not a character.")
  if(any(duplicated(names(newValueLabels)))) stop("Duplicated values in newValueLabels.")
  if(length(names(newValueLabels)) == 0) stop("neValueLabels needs to be named.")
  compare_and_order(set1 = names(newValueLabels), set2 = unique(newValues), FUN = stop)
}
