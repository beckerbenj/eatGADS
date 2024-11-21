#' Check meta data for fractional values.
#'
#' Check the a \code{GADSdat} object for any occurrences of fractional values
#' in its meta data which could cause problems esp. when exporting into \code{.dta} format.
#'
#' This functions checks values that are tagged as being labeled in the corresponding
#' meta data column (\code{GADSdat$labels$labeled}). It therefore covers both
#' "truely" labeled values (that have been assigned a \code{valLabel}) and
#' values tagged as \code{missings} (with or without a \code{valLabel}).
#'
#'@param GADSdat A \code{GADSdat} object.
#'
#'@return Returns a list of two \code{data.frames}, respectively listing the
#' \code{varName}s and fractional \code{value}s tagged as \code{valid} or \code{miss}.
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

  # initialize the return list
  out <- lapply(1:2, function(x) x = data.frame(varName = "<none found>",
                                                value = NA_real_,
                                                empty = NA))
  names(out) <- c("valid", "miss")

  # perform check
  is_labeled_fractional <- (GADSdat$labels$labeled == "yes") & (GADSdat$labels$value %% 1 != 0)

  # exit if none of the labeled values is fractional
  if (!any(is_labeled_fractional)) return(out)

  # look for empty labeled fractionals and fill return list
  for (misstag in c("valid", "miss")) {
    subset <- GADSdat$labels[is_labeled_fractional & GADSdat$labels$missings == misstag,
                             c("varName", "value")]
    # reset rownames carried over from metadata
    rownames(subset) <- NULL
    if (nrow(subset) == 0) next
    varlist <- unique(subset$varName)
    emptyvals <- checkEmptyValLabels(GADSdat = GADSdat,
                                     vars = varlist)
    subset$empty <- unlist(lapply(varlist, function(var) {
      vallist <- subset[subset$varName == var, "value"]
      is_empty <- match(x = vallist,
                        table = emptyvals[[var]]$value,
                        nomatch = 0) > 0
      return(is_empty)
    }))
    out[[misstag]][1:nrow(subset),] <- subset
  }

  return(out)
}
