#' Check meta data for fractional values.
#'
#' Check the a \code{GADSdat} object for any occurrences of fractional values
#' in its meta data which could cause problems esp. when exporting into \code{.dta} format.
#'
#' This functions checks values that are tagged as being labeled in the corresponding
#' metadata column (\code{GADSdat$labels$labeled}). It therefore covers both
#' "truely" labeled values (that have been assigned a \code{valLabel}) and
#' values tagged as \code{missings} (with or without a \code{valLabel}).
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
checkLabeledFractionals <- function(GADSdat) {
  check_GADSdat(GADSdat)
  labels <- GADSdat$labels

  # initialize the return list
  out <- data.frame(varName = "<none found>",
                    value = NA_real_,
                    missings = NA_character_,
                    empty = NA)

  labeled_fractional_row <- which((labels$labeled == "yes") & (labels$value %% 1 != 0))

  # exit if none of the labeled values is fractional
  if (length(labeled_fractional_row) == 0) return(out)

  # fill list
  out[1:length(labeled_fractional_row), 1:3] <- labels[labeled_fractional_row, c("varName",
                                                                                 "value",
                                                                                 "missings")]
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
