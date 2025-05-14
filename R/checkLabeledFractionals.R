#' Check meta data for fractional values.
#'
#' Check a \code{GADSdat} object for any occurrences of fractional values
#' in its meta data which could cause problems esp. when exporting into \code{.dta} format.
#'
#' This functions checks values that are tagged as labeled in the corresponding
#' metadata column (\code{GADSdat$labels$labeled}). It therefore covers both
#' "truely" labeled values (that have been assigned a \code{valLabel}) and
#' values tagged as \code{missings} (with or without a \code{valLabel}).
#'
#'@param GADSdat A \code{GADSdat} object.
#'
#'@return Returns a \code{data.frame}, listing the affected \code{varName}s,
#' the labeled fractional \code{value}s, and their respective \code{missings} tag
#' according to the metadata, as well as whether they actually occur in the data (\code{empty}).
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

  out <- data.frame(varName = "<none found>",
                    value = NA_real_,
                    missings = NA_character_,
                    empty = NA)

  labeled_fractional_rows <- which((labels$labeled == "yes") & (labels$value %% 1 != 0))

  if (length(labeled_fractional_rows) == 0) {
    return(out)
  }

  out[1:length(labeled_fractional_rows), 1:3] <- labels[labeled_fractional_rows, c("varName",
                                                                                   "value",
                                                                                   "missings")]

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
