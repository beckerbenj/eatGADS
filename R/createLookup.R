#### Write xlsx for recoding
#############################################################################
#' Extract values for recoding.
#'
#' Extract unique values from one or multiple variables of a \code{GADSdat} object for recoding (e.g. via an Excel spreadsheet).
#'
#' If recoding of one or multiple variables is more complex, a lookup table can be created for later application via
#' \code{\link{applyLookup}} or \code{\link{applyLookup_expandVar}}. The function allows the extraction of the values
#' of multiple variables and sorting of these unique values via \code{variable} and/or \code{values}.
#' If \code{addCols} are specified the lookup table has to be formatted via \code{\link{collapseColumns}},
#' before it can be applied to recode data.
#'
#'@param GADSdat A \code{GADSdat} object.
#'@param recodeVars Character vector of variable names which should be recoded.
#'@param sort_by By which column (\code{variable} and/or \code{value}) should the long format \code{data.frame} be sorted? If \code{NULL}, no sorting is performed.
#'@param addCols Character vector of additional column names for recoding purposes.
#'
#'@return Returns a data frame in long format with the following variables:
#'\item{variable}{Variables as specified in \code{recodeVars}}
#'\item{value}{Unique values of the variables specified in \code{recodeVars}}
#'\item{value_new}{This is the default for \code{addCols}. If different additional column names are supplied, this column is missing.}
#'
#'@examples
#' # create example GADS
#' dat <- data.frame(ID = 1:4, var1 = c(NA, "Eng", "Aus", "Aus2"),
#'                   var2 = c(NA, "French", "Ger", "Ita"),
#'                   stringsAsFactors = FALSE)
#' gads <- import_DF(dat)
#'
#' # create Lookup table for recoding
#' lookup <- createLookup(gads, recodeVars = c("var1", "var2"), sort_by = c("value", "variable"))
#'
#' # create Lookup table for recoding by multiple recoders
#' lookup2 <- createLookup(gads, recodeVars = c("var1", "var2"), sort_by = c("value", "variable"),
#'                         addCols = c("value_recoder1", "value_recoder2"))
#'
#'@export
createLookup <- function(GADSdat, recodeVars, sort_by = NULL, addCols = c("value_new")) {
  UseMethod("createLookup")
}

#'@export
createLookup.GADSdat <- function(GADSdat, recodeVars, sort_by = NULL, addCols = c("value_new")) {
  check_GADSdat(GADSdat)
  if(!is.character(recodeVars) && length(recodeVars) > 0) stop("recodeVars needs to be a character vector of at least length 1.")
  if(!is.character(addCols) && length(addCols) > 0) stop("addCols needs to be a character vector of at least length 1.")
  if(!all(recodeVars %in% namesGADS(GADSdat))) stop("Some of the variables are not variables in the GADSdat.")
  vars_w <- data.table::as.data.table(GADSdat$dat[, recodeVars, drop = FALSE])
  dt_l <- suppressWarnings(unique(data.table::melt(vars_w, measure.vars = recodeVars, variable.factor = FALSE, value.factor = FALSE)))

  if(!all(sort_by %in% names(dt_l))) stop("data.frame can only be sorted by 'variable' or 'value' or both.")
  if(!is.null(sort_by)) {
    data.table::setorderv(dt_l, cols = sort_by, order=1L, na.last=FALSE)
  }

  vars_l <- data.frame(dt_l, stringsAsFactors = FALSE)
  for(i in addCols) vars_l[, i] <- NA

  vars_l
}
