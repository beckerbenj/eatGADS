
#############################################################################
#' Check uniqueness of a variable.
#'
#' Function to check if a variable is unique for all cases of an identifier variable.
#'
#' For example if missing values are multiple imputed and data is stored in a long format, checking the uniqueness of a variable
#' within an identifier can be tricky. This function automates this task.
#'
#'@param GADSdat \code{GADSdat} object imported via \code{eatGADS}.
#'@param varName Single string indicating how missing labels are commonly named in the value labels.
#'@param idVar If \code{TRUE}, missing codes are added according to occurrence of \code{"missingLabel"} in \code{"valLabel"}.
#'
#'@return Returns either \code{TRUE} if the variable is unique within each value for \code{idVar} or \code{GADSdat} object including
#' the not unique cases.
#'
#'@examples
#'## create an example GADSdat
#'iris2 <- iris
#'iris2$Species <- as.character(iris2$Species)
#'gads <- import_DF(iris2, checkVarNames = FALSE)
#'
#'## check uniqueness
#'checkUniqueness(gads, varName = "Sepal.Length", idVar = "Species")
#'
#'@export
checkUniqueness <- function(GADSdat, varName, idVar) {
  UseMethod("checkUniqueness")
}

#'@export
checkUniqueness.GADSdat <- function(GADSdat, varName, idVar) {
  check_GADSdat(GADSdat)
  check_vars_in_GADSdat(GADSdat, vars = c(varName, idVar))

  dat <- GADSdat$dat
  if(nrow(dat) == length(unique(dat[[idVar]]))) stop("'idVar' is unique per row in 'GADSdat' and checking for uniqueness is obsolete.")

  out_list <- by(dat, dat[, idVar], function(subdat) {
    if(length(unique(subdat[[varName]])) != 1) return(subdat)
    NULL
  })
  #browser()

  out_df <- do.call(rbind, out_list)
  if(is.null(out_df)) return(TRUE)

  row.names(out_df) <- NULL
  out_df
}

