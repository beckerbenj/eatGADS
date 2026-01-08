#' Check a \code{GADSdat} for large labeled whole-number values.
#'
#' Check a \code{GADSdat} object for any occurrences of labeled whole-number values
#'  that would be too large for R to handle if they were coerced \code{as.integer()}.
#'
#' According to its documentation, R can only handle \code{\link[base:integer]{integer}}
#' values of up to (roughly) \eqn{\pm 2 \times 10^9} (2,147,483,647 to be exact;
#' c.f. \code{\link[base:.Machine]{.Machine}$integer.max}).
#' This restriction is relevant when exporting a \code{GADSdat} to \code{.dta}
#' and only when any value exceeding the limit is also labeled (or tagged as missing).
#' This is because Stata only accepts labeled \emph{integer} (not labeled \emph{floating-point};
#' c.f. \code{\link{checkLabeledFractionals}()} in this package)
#' values. \code{haven}'s \code{\link[haven:write_dta]{write_dta}} function will therefore
#' try to coerce any labeled values \code{as.integer()}. Unlabeled values, however, will
#' stay generic \code{numeric} values that have a higher limit.
#'
#'@param GADSdat A \code{GADSdat} object.
#'
#'@return Returns a \code{data.frame}, listing the affected \code{varName}s,
#' the large whole-number \code{value}s, their respective \code{missings} tags,
#' and whether they actually occur in the data (\code{empty}).
#' The \code{rownum}s of the affected rows in \code{GADSdat$labels} are also
#' provided in a separate column as a fail safe.
#'
#'@examples
#' # Introduce a fractional value into meta data
#' pisa2 <- changeMissings(GADSdat = pisa,
#'                         varName = "schtype",
#'                         value = 9999999999,
#'                         missings = "miss")
#' eatGADS:::checkIntOverflow(pisa2)

checkIntOverflow <- function(GADSdat) {
  check_GADSdat(GADSdat)
  labels <- GADSdat$labels

  out <- data.frame(varName = character(),
                    value = numeric(),
                    missings = character(),
                    empty = logical(),
                    rownum = integer())

  huge_labeled_number_rows <- which(abs(eatTools::asNumericIfPossible(labels$value)) > .Machine$integer.max &
                                      eatTools::asNumericIfPossible(labels$value) %% 1 == 0)

  if (length(huge_labeled_number_rows) == 0) {
    return(out)
  }

  out[1:length(huge_labeled_number_rows), c("varName",
                                            "value",
                                            "missings")] <-
    labels[huge_labeled_number_rows, c("varName",
                                       "value",
                                       "missings")]
  out$rownum <- huge_labeled_number_rows

  varlist <- unique(out$varName)
  empty_values <- checkEmptyValLabels(GADSdat = GADSdat,
                                      vars = varlist)
  for(varname in varlist) {
    vallist <- out[out$varName == varname, "value"]
    out[out$varName == varname, "empty"] <- vallist %in% empty_values[[varname]]$value
  }

  rownames(out) <- NULL
  return(out)
}
