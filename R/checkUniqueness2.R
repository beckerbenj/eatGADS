
#############################################################################
#' Check uniqueness of a variable.
#'
#' Function to check if a variable is unique for all cases of an identifier variable. This is a fast and more efficient version of
#' \code{\link{checkUniqueness}} which always returns a logical of length one.
#'
#' For example if missing values are multiple imputed and data is stored in a long format, checking the uniqueness of a variable
#' within an identifier can be tricky. This function automates this task via reshaping the data into wide format and testing equality
#' among the reshaped variables. . Similar functionality (via matrices) is covered by \code{lme4::isNested},
#' which is more general and performs similarly.
#'
#'@param GADSdat \code{GADSdat} object imported via \code{eatGADS}.
#'@param varName Single string containing the variable name for which the check should be performed.
#'@param idVar Single string containing the name of the identifier variable.
#'@param impVar Single string containing the name of the imputation number.
#'
#'@return Returns a logical of length one.
#'
#'@examples
#'## create an example GADSdat
#'l <- 1000
#'long_df <- data.table::data.table(id = sort(rep(1:l, 15)),
#'                                v1 = sort(rep(1:l, 15)),
#'                                  imp = rep(1:15, l))
#'gads <- import_DF(long_df)
#'## check uniqueness
#'checkUniqueness2(gads, varName = "v1", idVar = "id", impVar = "imp")
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
  if(nrow(dat) == length(unique(dat[[idVar]]))) {
    message("'idVar' is unique per row in 'GADSdat' and checking for uniqueness is obsolete.")
    return(TRUE)
  }

  #browser()
  form <- stats::as.formula(paste0(idVar, " ~ ", impVar))
  subdat <- dat[, c(idVar, varName, impVar), with = FALSE]
  wide <- data.table::dcast(subdat, formula = form, value.var = varName)

  ## compare all of them
  imp_num <- length(unique(dat[[impVar]]))
  if(imp_num < 2)  stop("'impVar' must be an imputation variable with at least two different values.")
  log_list <- sapply(3:(imp_num+1), function(x) {
    all(wide[[2]] == wide[[x]])
  })
  all(log_list)
}


