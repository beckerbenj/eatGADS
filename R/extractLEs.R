
#############################################################################
#' Extract linking errors.
#'
#' Extract linking errors from a \code{trend_GADSdat} object.
#'
#' \code{\link{extractData}} has been split up into \code{\link{extractData}} and \code{\link{extractLEs}}. \code{\link{extractDataOld}}
#' provides (for the time being) backwards compatability so linking errors can be merged automatically.
#'
#'@param GADSdat A \code{GADSdat} object.
#'@param convertMiss Should values coded as missing values be recoded to \code{NA}?
#'@param convertLabels If \code{"numeric"}, values remain as numerics. If \code{"factor"} or \code{"character"}, values are recoded to their labels. Corresponding variable type is applied.
#'@param dropPartialLabels Should value labels for partially labeled variables be dropped? If \code{TRUE}, the partial labels will be dropped. If \code{FALSE}, the variable will be converted to the class specified in \code{convertLabels}.
#'@param convertVariables Character vector of variables names, which labels should be applied to. If not specified (default), value labels are applied to all variables for which labels are available. Variable names not in the actual GADS are silently dropped.
#'
#'@return Returns a data frame.
#'
#'
#'@export
extractLEs <- function(GADSdat, convertMiss = TRUE, convertLabels = "character", dropPartialLabels = TRUE, convertVariables = NULL) {
  UseMethod("extractLEs")
}

#'@export
extractLEs.GADSdat <- function(GADSdat, convertMiss = TRUE, convertLabels = "character", dropPartialLabels = TRUE, convertVariables = NULL) {
  stop("extractLEs() is only meaningful for 'trend_GADSdat' objects.")
}

#'@export
extractLEs.trend_GADSdat <- function(GADSdat, convertMiss = TRUE, convertLabels = "character", dropPartialLabels = TRUE, convertVariables = NULL) {
  if(!"LEs" %in% names(GADSdat$datList) || is.null(GADSdat$datList$LEs)) stop("No linking errors found in 'GADSdat'. Make sure to specify 'lePath' in getTrendsGADS().")
  check_trend_GADSdat(GADSdat)

  gads <- extractGADSdat(all_GADSdat = GADSdat, name = "LEs")
  dat <- extractData(gads, convertMiss = convertMiss, convertLabels = convertLabels,
                     dropPartialLabels = dropPartialLabels, convertVariables = convertVariables)
  dat
}
