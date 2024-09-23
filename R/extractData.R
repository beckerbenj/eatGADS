#### extractData
#############################################################################
#' Extract Data
#'
#' Extract \code{data.frame} from a \code{GADSdat} object for analyses in \code{R}. Value labels can be
#'  selectively applied via defining \code{convertLabels} and \code{covertVariables}.
#'  For extracting meta data see \code{\link{extractMeta}}.
#'
#' A \code{GADSdat} object includes actual data (\code{GADSdat$dat}) and the corresponding meta data information
#' (\code{GADSdat$labels}). \code{extractData} extracts the data and applies relevant meta data on value level (missing conversion, value labels),
#' so the data can be used for analyses in \code{R}. Variable labels are retained as \code{label} attributes on column level.
#'
#' If \code{factor} are extracted via \code{convertLabels == "factor"}, an attempt is made to preserve the underlying integers.
#' If this is not possible, a warning is issued.
#' As \code{SPSS} has almost no limitations regarding the underlying values of labeled
#' integers and \code{R}'s \code{factor} format is very strict (no \code{0}, only integers increasing by \code{+ 1}),
#' this procedure can lead to frequent problems.
#'
#'@param GADSdat A \code{GADSdat} object.
#'@param convertMiss Should values tagged as missing values be recoded to \code{NA}?
#'@param convertLabels If \code{"numeric"}, values remain as numerics. If \code{"factor"} or
#' \code{"character"}, values are recoded to their labels. Corresponding variable type is applied.
#'@param convertVariables Character vector of variables names, which labels should be applied to.
#' All other variables remain as numeric variables in the data.
#'If not specified [default], value labels are applied to all variables for which labels are available.
#' Variable names not in the actual \code{GADS} are silently dropped.
#'@param dropPartialLabels Should value labels for partially labeled variables be dropped?
#'If \code{TRUE}, the partial labels will be dropped. If \code{FALSE}, the variable will be converted
#'to the class specified in \code{convertLabels}.
#'
#'@return Returns a data frame.
#'
#'@examples
#'# Extract Data for Analysis
#'dat <- extractData(pisa)
#'
#'# convert labeled variables to factors
#'dat <- extractData(pisa, convertLabels = "factor")
#'
#'# convert only some variables to factor, all others remain numeric
#'dat <- extractData(pisa, convertLabels = "factor", convertVariables = c("schtype", "ganztag"))
#'
#'# convert only some variables to character, all others remain numeric
#'dat <- extractData(pisa, convertLabels = "factor", convertVariables = c("schtype", "ganztag"))
#'# schtype is now character
#'table(dat$schtype)
#'# schtype remains numeric
#'table(dat$gender)
#'
#'@export
extractData <- function(GADSdat,
                        convertMiss = TRUE,
                        convertLabels = c("character", "factor", "numeric"),
                        convertVariables = NULL,
                        dropPartialLabels = TRUE) {
  UseMethod("extractData")
}

#'@export
extractData.GADSdat <- function(GADSdat,
                                convertMiss = TRUE,
                                convertLabels = c("character", "factor", "numeric"),
                                convertVariables = NULL,
                                dropPartialLabels = TRUE) {
  check_GADSdat(GADSdat)
  convertLabels <- match.arg(convertLabels)

  transform_call_extractData2(GADSdat = GADSdat, convertMiss = convertMiss,
                              convertLabels = convertLabels, dropPartialLabels = dropPartialLabels,
                              convertVariables = convertVariables)
}

#'@export
extractData.trend_GADSdat <- function(GADSdat,
                                      convertMiss = TRUE,
                                      convertLabels = c("character", "factor", "numeric"),
                                      convertVariables = NULL,
                                      dropPartialLabels = TRUE) {
  check_trend_GADSdat(GADSdat)
  if("LEs" %in% names(GADSdat$datList)) {
    stop("Linking errors are no longer supported by extractData. Use extractDataOld() instead.")
  }
  convertLabels <- match.arg(convertLabels)

  transform_call_extractData2(GADSdat = GADSdat, convertMiss = convertMiss,
                              convertLabels = convertLabels, dropPartialLabels = dropPartialLabels,
                              convertVariables = convertVariables)
}

transform_call_extractData2 <- function(GADSdat, convertMiss, convertLabels, dropPartialLabels, convertVariables) {
  # defautlt value of convertVariables is NULL and should transform all variables
  # (see extractData() documentation)
  if(is.null(convertVariables)) {
    convertVariables <- unique(unlist(namesGADS(GADSdat)))
  }

  if(identical(convertLabels, "character")) {
    GADSdat_out <- extractData2(GADSdat = GADSdat, convertMiss = convertMiss,
                                labels2character = convertVariables, dropPartialLabels = dropPartialLabels)
  } else if(identical(convertLabels, "factor")) {
    GADSdat_out <- extractData2(GADSdat = GADSdat, convertMiss = convertMiss,
                                labels2factor = convertVariables, dropPartialLabels = dropPartialLabels)
  } else if(identical(convertLabels, "numeric")) {
    GADSdat_out <- extractData2(GADSdat = GADSdat, convertMiss = convertMiss,
                                labels2factor = NULL, labels2character = NULL, labels2ordered = NULL,
                                dropPartialLabels = dropPartialLabels)
  }
  GADSdat_out
}



