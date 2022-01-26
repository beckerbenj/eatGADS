#### Merge GADSdat objects
#############################################################################
#' Bind two \code{GADSdat} objects into a single \code{GADSdat} object by columns.
#'
#' Is a secure way to \code{cbind} the data and the meta data of two \code{GADSdat} objects. Currently, only limited merging options are supported.
#'
#' If there are duplicate variables (except the variables specified in the \code{by} argument), these variables are removed from y.
#' The meta data is joined for the remaining variables via \code{rbind}.
#'
#'@param ... Multiple \code{GADSdat} objects imported via \code{eatGADS}.
#'@param deparse.level Argument is ignored in this method.
#'
#'@return Returns a \code{GADSdat} object.
#'
#'
#'@export
cbind.GADSdat <- function(..., deparse.level = 1) {
  GADSdat_list <- list(...)
  nam_list <- lapply(GADSdat_list, function(GADSdat) {
    check_GADSdat(GADSdat)
    namesGADS(GADSdat)
  })

  all_nam <- unlist(nam_list)
  duplicate_nam <- unique(all_nam[duplicated(all_nam)])
  if(length(duplicate_nam) > 0) stop("The following variables are in multiple GADSdats: ", paste(duplicate_nam, collapse = ", "))

  dat_list <- lapply(GADSdat_list, function(GADSdat) GADSdat[["dat"]])
  labels_list <- lapply(GADSdat_list, function(GADSdat) GADSdat[["labels"]])

  newDat <- do.call(cbind, dat_list)
  newLabels <- do.call(rbind, labels_list)

  newGADS <- new_GADSdat(dat = newDat, labels = newLabels)
  check_GADSdat(GADSdat = newGADS)
  newGADS
}
