#### Remove value label
#############################################################################
#' Remove unused value labels and missing tags.
#'
#' Remove unused value labels and missing tags of a variable as part of a \code{GADSdat} object.
#'
#'@param GADSdat \code{GADSdat} object imported via \code{eatGADS}.
#'@param vars Character string of variable names.
#'@param whichValLabels Should unused missing value tags and labels (\code{"miss"}), unused value labels for non-missing values (\code{"valid"}), or both (\code{"all"}) be removed?
#'
#'@return Returns the \code{GADSdat} object with changed meta data.
#'
#'@examples
#'gads <- import_DF(data.frame(v1 = 1))
#'gads <- changeMissings(gads, varName = "v1", value = c(-99, -98), missings = c("miss", "miss"))
#'gads <- changeValLabels(gads, varName = "v1", value = c(-99), valLabel = c("not reached"))
#'
#'gads2 <- removeEmptyValLabels(gads, vars = "v1")
#'@export
removeEmptyValLabels <- function(GADSdat, vars, whichValLabels = c("miss", "valid", "all")) {
  UseMethod("removeEmptyValLabels")
}
#'@export
removeEmptyValLabels.GADSdat <- function(GADSdat, vars, whichValLabels = c("miss", "valid", "all")) {
  check_GADSdat(GADSdat)
  check_vars_in_GADSdat(GADSdat, vars = vars, argName = "vars")
  whichValLabels <- match.arg(whichValLabels)

  emptyValLabels <- checkEmptyValLabels(GADSdat, vars = vars, output = "list")

  for(nam in names(emptyValLabels)){
    #browser()
    emptyValLabels_nam <- emptyValLabels[[nam]]

    if(identical(whichValLabels, "miss")) {
      emptyValLabels_nam <- emptyValLabels_nam[which(emptyValLabels_nam$missings == "miss"), ]
    }
    if(identical(whichValLabels, "valid")) {
      emptyValLabels_nam <- emptyValLabels_nam[which(emptyValLabels_nam$missings == "valid"), ]
    }

    if(is.null(emptyValLabels_nam) || nrow(emptyValLabels_nam) == 0) next
    GADSdat <- removeValLabels(GADSdat, varName = nam, value = emptyValLabels_nam$value)
  }

  GADSdat
}



