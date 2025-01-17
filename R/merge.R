#### Merge GADSdat objects
#############################################################################
#' Merge two \code{GADSdat} objects into a single \code{GADSdat} object.
#'
#' Is a secure way to merge the data and the meta data of two \code{GADSdat} objects.
#' Currently, only limited merging options are supported.
#'
#' If there are duplicate variables (except the variables specified in the \code{by} argument), these variables are removed from y.
#' The meta data is joined for the remaining variables via \code{rbind}.
#'
#' The function supports automatically recoding missing values created through merging with a designated missing code
#' (\code{missingValue}) and a value label (\code{missingValLabel}).
#'
#'@param x \code{GADSdat} object imported via \code{eatGADS}.
#'@param y \code{GADSdat} object imported via \code{eatGADS}.
#'@param by A character vector.
#'@param all A character vector (either a full join or an inner join).
#'@param all.x See merge.
#'@param all.y See merge.
#'@param missingValue A numeric value that is used to replace missing values introduced through the merge.
#'@param missingValLabel The value label that is assigned to all variables into which \code{missingValue} is inserted.
#'@param ... Further arguments are currently not supported but have to be included for \code{R CMD} checks.
#'
#'@return Returns a \code{GADSdat} object.
#'
#'
#'@export
merge.GADSdat <- function(x, y, by, all = TRUE, all.x = all, all.y = all,
                          missingValue = NULL, missingValLabel = NULL, ...) {
  check_GADSdat(x)
  check_GADSdat(y)
  if(!is.character(by)) {
    stop(by, " is not a character vector.")
  }
  check_vars_in_GADSdat(x, vars = by, argName = "by", GADSdatName = "x")
  check_vars_in_GADSdat(y, vars = by, argName = "by", GADSdatName = "y")

  # drop duplicate variables from y
  y_vars <- c(by, names(y$dat)[!names(y$dat) %in% names(x$dat)])
  if(!length(y_vars) > length(by)) {
    stop("y does not contain variables distinct from x.")
  }

  # replace NAs in original data sets to be able to recode them after merging
  if(!is.null(missingValue)) {
    check_numericArgument(missingValue, argName = "missingValue")

    if(any(!is.na(x$dat)) || any(!is.na(y$dat))) {
      intermediate_code <- -99.9989 ## default value
      while (any(!is.na(x$dat) & x$dat == intermediate_code) || any(!is.na(y$dat) & y$dat == intermediate_code)) {
        intermediate_code <- stats::runif(1, -1e4, -1e2)  # Generate a random value in a wide range
      }
      x$dat[is.na(x$dat)] <- intermediate_code
      y$dat[is.na(y$dat)] <- intermediate_code
    }
  }

  newDat <- merge(x$dat, y$dat[, y_vars], by = by, all = all, all.x = all.x, all.y = all.y, ...)
  newLabels <- rbind(x$labels, y$labels[y$labels$varName %in% y_vars[!y_vars %in% by], ])
  newGADS <- new_GADSdat(dat = newDat, labels = newLabels)
  check_GADSdat(GADSdat = newGADS)

  # replace NAs introduced through merging with missingValue and recode original NAs back
  if(!is.null(missingValue)) {
    vars_with_missingValue <- colnames(newGADS$dat)[apply(is.na(newGADS$dat), 2, any)]
    newGADS$dat[is.na(newGADS$dat)] <- missingValue
    newGADS$dat[newGADS$dat == intermediate_code] <- NA

    for(single_var in vars_with_missingValue) {
      newGADS <- changeMissings(newGADS, varName = single_var, value = missingValue, missings = "miss")
    }

    if(!is.null(missingValLabel) && length(vars_with_missingValue) > 0) {
      check_characterArgument(missingValLabel, argName = "missingValLabel")
      for(single_var in vars_with_missingValue) {
        newGADS <- changeValLabels(newGADS, varName = single_var, value = missingValue, valLabel = missingValLabel)
      }
    }
  }
  newGADS
}
