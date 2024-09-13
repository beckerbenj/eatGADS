#### extractData
#############################################################################
#' Extract Data while merging linking errors.
#'
#' Support for linking error data bases has been removed from \code{eatGADS}.
#' \code{extractDataOld} provides (for the time being) backwards compatibility, so linking errors can still be merged automatically.
#'
#' See \code{\link{extractData}} for the current functionality.
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
extractDataOld <- function(GADSdat, convertMiss = TRUE, convertLabels = "character", dropPartialLabels = TRUE, convertVariables = NULL) {
  UseMethod("extractDataOld")
}

#'@export
extractDataOld.GADSdat <- function(GADSdat, convertMiss = TRUE, convertLabels = "character", dropPartialLabels = TRUE, convertVariables = NULL) {
  stop("extractDataOld() is only implemented for backwards compatability of 'trend_GADSdat' objects. Please use extractData2() or extractData() for 'GADSdat' objects.")
}

#'@export
extractDataOld.trend_GADSdat <- function(GADSdat, convertMiss = TRUE, convertLabels = "character", dropPartialLabels = TRUE, convertVariables = NULL) {
  names_no_LEs <- names(GADSdat$datList)[names(GADSdat$datList) != "LEs"]
  if(length(names_no_LEs) > 2) {
    stop("extractDataOld() is only implemented for backwards compatability of 'trend_GADSdat' with data from two data bases. For 'trend_GADSdat' objects with data from more than two data bases use extractData2() or extractData() instead.")
  }
  check_trend_GADSdat(GADSdat)

  GADSdat_noLEs <- GADSdat
  GADSdat_noLEs$datList <- GADSdat_noLEs$datList[names(GADSdat_noLEs$datList) != "LEs"]
  class(GADSdat) <- class(GADSdat)

  all_dat <-   transform_call_extractData2(GADSdat = GADSdat_noLEs, convertMiss = convertMiss,
                                           convertLabels = convertLabels, dropPartialLabels = dropPartialLabels,
                                           convertVariables = convertVariables)

  ## if available, merge also linking errors; merge picks by automatically, keep variable order as in original data frames
  if(!is.null(GADSdat$datList[["LEs"]])) {
    gads_le <- extractGADSdat(all_GADSdat = GADSdat, name = "LEs")
    le <- extractData(gads_le, convertMiss = convertMiss, convertLabels = "character")

    # performance relevant: merge (data.table seems to be fastest)
    all_dat <- data.table::setDT(all_dat)
    le <- data.table::setDT(le)
    all_dat_withLEs <- merge(all_dat, le)
    all_dat_withLEs <- as.data.frame(all_dat_withLEs)

    all_dat <- all_dat_withLEs[, c(names(all_dat), setdiff(names(le), names(all_dat)))]
  }

  all_dat <- all_dat[, c(names(all_dat)[names(all_dat) != "year"], "year")]
  # remove attributes (varLabels) (extractData has been changed)
  all_dat <- all_dat[seq(nrow(all_dat)), ]

  all_dat
}
