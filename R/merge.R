#### Merge GADSdat objects
#############################################################################
#' Merge two \code{GADSdat} objects into a single \code{GADSdat} object.
#'
#' Is a secure way to merge the data and the meta data of two \code{GADSdat} objects. Currently, only limited merging options are supported.
#'
#' If there are duplicate variables (except the variables specified in the \code{by} argument), these variables are removed from y.
#' The meta data is joined for the remaining variables via \code{rbind}.
#'
#'@param x \code{GADSdat} object imported via \code{eatGADS}.
#'@param y \code{GADSdat} object imported via \code{eatGADS}.
#'@param by A character vector.
#'@param all A character vector (either a full join or an inner join).
#'@param all.x See merge.
#'@param all.y See merge.
#'@param ... Further arguments are currently not supported but have to be included for \code{R CMD} checks.
#'
#'@return Returns a \code{GADSdat} object.
#'
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
