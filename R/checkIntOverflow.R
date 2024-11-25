#' Check a \code{GADSdat} for large integers.
#'
#' Check a \code{GADSdat} object for any occurrences of integer values, esp.
#' in the metadata, that are too large for R to handle.
#' in its meta data which could cause problems esp. when exporting into \code{.dta} format.
#'
#' According to its documentation, R can only handle \code{\link[base:integer]{integers}}
#' of up to \emph{roughly} \eqn{\pm 2 \times 10^9}. Right now, this restriction appears
#' to only be of interfering relevance when exporting a data set to \code{.dta} and only
#' when the very large value is also labeled (or tagged as missing). This is due to
#' Stata only accepting integers as being labeled - otherwise the value will stay a
#' generic numeric.
#'
#'@param GADSdat A \code{GADSdat} object.
#'
#'@return Returns a \code{data.frame}, listing the \code{varName}s,
#' fractional \code{value}s, their \code{missings} tag according to the metadata,
#' and whether they actually occur in the data.
#'
#'@examples
#' # Introduce a fractional value into meta data
#' pisa2 <- recodeGADS(GADSdat = pisa,
#'                     varName = "schtype",
#'                     oldValues = 2,
#'                     newValues = .5)
#' checkLabeledFractionals(pisa2)
#'
#'@export
checkIntOverflow <- function(GADSdat) {
  check_GADSdat(GADSdat)
}
