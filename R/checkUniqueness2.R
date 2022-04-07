
#############################################################################
#' Check uniqueness of a variable.
#'
#' Function to check if a variable is unique for all cases of an identifier variable. This is a fast and more efficient version of
#' \code{\link{checkUniqueness}} which always returns a logical of length one.
#'
#' For example if missing values are multiple imputed and data is stored in a long format, checking the uniqueness of a variable
#' within an identifier can be tricky. This function automates this task.
#'
#'@param GADSdat \code{GADSdat} object imported via \code{eatGADS}.
#'@param varName Single string containing the variable name for which the check should be performed.
#'@param idVar Single string containing the identifier variable name.
#'
#'@return Returns a logical of length one.
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
checkUniqueness2 <- function(GADSdat, varName, idVar, impVar) {
  UseMethod("checkUniqueness2")
}

#'@export
checkUniqueness2.GADSdat <- function(GADSdat, varName, idVar, impVar) {
  check_GADSdat(GADSdat)
  check_vars_in_GADSdat(GADSdat, vars = c(varName, idVar, impVar))

  checkUniqueness2(GADSdat$dat, varName = varName, idVar = idVar, impVar = impVar)
}

#'@export
checkUniqueness2.data.frame <- function(GADSdat, varName, idVar, impVar) {
  dat <- data.table::as.data.table(GADSdat)
  if(nrow(dat) == length(unique(dat[[idVar]]))) stop("'idVar' is unique per row in 'GADSdat' and checking for uniqueness is obsolete.")

  #browser()
  form <- as.formula(paste0(idVar, " ~ ", impVar))
  subdat <- dat[, c(idVar, varName, impVar), with = FALSE]
  wide <- data.table::dcast(subdat, formula = form, value.var = varName)
  all(wide[, 2] == wide[, 3])
}


