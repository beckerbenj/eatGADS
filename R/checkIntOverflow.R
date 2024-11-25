#' Check a \code{GADSdat} for large integers.
#'
#' Check a \code{GADSdat} object for any occurrences of integer values, esp.
#' in the metadata, that are too large for R to handle.
#' in its meta data which could cause problems esp. when exporting into \code{.dta} format.
#'
#' According to its documentation, R can only handle \code{\link[base:integer]{integers}}
#' of up to \emph{roughly} \eqn{\pm 2 \times 10^9} (2,147,483,647 to be exact).
#' Right now, this restriction appears to only be of interfering relevance when
#' exporting a data set to \code{.dta} and only when the very large value is also
#' labeled (or tagged as missing). This is due to Stata only accepting integers
#' as being labeled - otherwise the value will stay a generic numeric.
#'
#'@param GADSdat A \code{GADSdat} object.
#'
#'@return Returns a \code{data.frame}, listing the \code{varName}s,
#' large \code{value}s (\code{as.numeric}), their \code{missings} tag
#' according to the metadata, and whether they actually occur in the data.
#' The \code{rownum}s of the affected rows in \code{GADSdat$labels} is provided
#' in a separate column as a fail safe.
#'
#'@examples
#' # Introduce a fractional value into meta data
#' pisa2 <- changeMissings(GADSdat = pisa,
#'                         varName = "schtype",
#'                         value = 9999999999,
#'                         missings = "miss")
#' checkIntOverflow(pisa2)
#'
#'@export
checkIntOverflow <- function(GADSdat) {
  check_GADSdat(GADSdat)

  labels <- GADSdat$labels

  # initialize the return list
  out <- data.frame(varName = "<none found>",
                    value = NA_real_,
                    missings = NA_character_,
                    empty = NA,
                    rownum = NA_integer_)

  # option 1
  # huge_number_row <- which((labels$labeled == "yes") &
  #                            (labels$value > 2147483647 | labels$value < -2147483647))

  # option 2
  huge_number_row <- suppressWarnings(which((labels$labeled == "yes") &
                                              !is.na(as.numeric(labels$value)) &
                                              is.na(as.integer(labels$value))))

  # exit if none of the labeled values is fractional
  if (length(huge_number_row) == 0) return(out)

  # fill list
  out[1:length(huge_number_row), 1:3] <- labels[huge_number_row, c("varName",
                                                                   "value",
                                                                   "missings")]
  out$rownum <- huge_number_row

  # check if values exist in data
  varlist <- unique(out$varName)
  emptyvals <- checkEmptyValLabels(GADSdat = GADSdat,
                                   vars = varlist)
  out$empty <- unlist(lapply(varlist, function(var) {
    vallist <- out[out$varName == var, "value"]
    is_empty <- match(x = vallist,
                      table = emptyvals[[var]]$value,
                      nomatch = 0) > 0
    return(is_empty)
  }))

  rownames(out) <- NULL
  return(out)
}
