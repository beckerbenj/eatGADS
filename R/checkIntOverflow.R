#' Check a \code{GADSdat} for large integers.
#'
#' Check a \code{GADSdat} object for any occurrences of integer values, esp.
#' in the metadata, that are too large for R to handle.
#'
#' According to its documentation, R can only handle \code{\link[base:integer]{integers}}
#' of up to (roughly) \eqn{\pm 2 \times 10^9} (2,147,483,647 to be exact;
#' c.f. \code{\link[base:.Machine]{.Machine}$integer.max}).
#' This restriction appears relevant only when exporting a \code{GADSdat} to \code{.dta}
#' and only when the very large value is also labeled (or tagged as missing).
#' This is because Stata only accepts labeled \emph{integer} values, not labeled \emph{double}
#' values. Unlabeled values will stay a generic numeric.
#'
#'@param GADSdat A \code{GADSdat} object.
#'
#'@return Returns a \code{data.frame}, listing the affected \code{varName}s,
#' the large \code{value}s, their respective \code{missings} tag
#' according to the metadata, and whether they actually occur in the data (\code{empty}).
#' The \code{rownum}s of the affected rows in \code{GADSdat$labels} are also
#' provided in a separate column as a fail safe.
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

  out <- data.frame(varName = NA_character_,
                    value = NA_real_,
                    missings = NA_character_,
                    empty = NA,
                    rownum = NA_integer_)[0,]

  # option 1
  # huge_number_rows <- which((labels$labeled == "yes") &
  #                            (labels$value > 2147483647 | labels$value < -2147483647))

  # option 2
  huge_number_rows <- suppressWarnings(which((labels$labeled == "yes") &
                                               !is.na(as.numeric(labels$value)) &
                                               is.na(as.integer(labels$value))))

  if (length(huge_number_rows) == 0) {
    return(out)
  }

  out[1:length(huge_number_rows), 1:3] <- labels[huge_number_rows, c("varName",
                                                                     "value",
                                                                     "missings")]
  out$rownum <- huge_number_rows

  varlist <- unique(out$varName)
  empty_values <- checkEmptyValLabels(GADSdat = GADSdat,
                                      vars = varlist)
  out$empty <- unlist(lapply(varlist, function(varname) {
    vallist <- out[out$varName == varname, "value"]
    is_empty <- match(x = vallist,
                      table = empty_values[[varname]]$value,
                      nomatch = 0) > 0
    return(is_empty)
  }))

  rownames(out) <- NULL
  return(out)
}
