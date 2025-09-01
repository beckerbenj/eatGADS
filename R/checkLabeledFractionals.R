#' Check a \code{GADSdat} for labeled fractional values.
#'
#' Check a \code{GADSdat} object for any occurrences of fractional values in its metadata,
#' including both "truly" labeled values and values tagged as \code{missings}.
#'
#' This function is mainly useful to ensure a data set can be saved as a \code{.dta} file.
#' Unlike, for example, SPSS, Stata only allows for integer values
#' (and so-called extended missing values) to be labeled
#' (\href{https://www.stata.com/manuals/u12.pdf#u12.6.3Valuelabels}{Stata manual: 12.6.3}).
#' Trying to export (meta) data with labeled fractional values would therefore cause problems
#' and run into an error from \code{haven}'s \code{\link[haven:write_dta]{write_dta}} function.
#'
#'@param GADSdat A \code{GADSdat} object.
#'
#'@return Returns a \code{data.frame}, listing the affected \code{varName}s,
#' the labeled fractional \code{value}s, their respective \code{missings} tags,
#' and whether they actually occur in the data (\code{empty}).
#'
#'@family dataset compliance checks
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
checkLabeledFractionals <- function(GADSdat) {
  check_GADSdat(GADSdat)
  labels <- GADSdat$labels

  out <- data.frame(varName = character(),
                    value = numeric(),
                    missings = character(),
                    empty = logical())

  labeled_fractional_rows <- which(labels$value %% 1 != 0)

  if (length(labeled_fractional_rows) == 0) {
    return(out)
  }

  out[1:length(labeled_fractional_rows), c("varName",
                                           "value",
                                           "missings")] <-
    labels[labeled_fractional_rows, c("varName",
                                      "value",
                                      "missings")]

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
