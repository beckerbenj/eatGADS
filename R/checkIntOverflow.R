#' Check a \code{GADSdat} for large labeled whole-number values.
#'
#' Check a \code{GADSdat} object for any occurrences of labeled whole-number values
#'  that would be too large for R to handle if they were coerced \code{as.integer()}.
#'
#' According to its documentation, R can only handle \code{\link[base:integer]{integer}}
#' values of up to (roughly) \eqn{\pm 2 \times 10^9} (2,147,483,647 to be exact;
#' c.f. \code{\link[base:.Machine]{.Machine}$integer.max}).
#' This restriction is only relevant when exporting a \code{GADSdat} to \code{.dta}
#' and only when any value exceeding the limit is also labeled (or tagged as missing).
#' This is because Stata only accepts labeled \emph{integer} (not labeled \emph{floating-point})
#' values. \link[haven:write_dta]{\code{haven}'s \code{write_dta}} function will therefore
#' coerce any labeled values \code{as.integer()}. Unlabeled values, however, will stay
#' generic \code{numeric} values that have a higher limit.
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

  huge_labeled_number_rows <- which(abs(labels$value) > .Machine$integer.max &
                                      labels$value %% 1 == 0)

  if (length(huge_labeled_number_rows) == 0) {
    return(out)
  }

  out[1:length(huge_labeled_number_rows), 1:3] <- labels[huge_labeled_number_rows, c("varName",
                                                                                     "value",
                                                                                     "missings")]
  out$rownum <- huge_labeled_number_rows

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
