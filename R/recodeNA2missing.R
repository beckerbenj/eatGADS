
#' Recode \code{NAs} to Missing.
#'
#' Recode \code{NAs} in multiple variables in a \code{GADSdat} to a numeric value with a value label and a missing tag.
#'
#' The value label and missing tag are only added to variables which contain \code{NAs} and which have been recoded.
#' If a variable has an existing value label for \code{value}, the existing value label is overwritten and a missing tag is added.
#' A corresponding warning is issued.
#'
#'
#'@param GADSdat A \code{GADSdat} object.
#'@param recodeVars Character vector of variable names which should be recoded.
#'@param value Which value should \code{NAs} be recoded to?
#'@param valLabel Which value label should \code{value} be assigned?
#'
#'@return Returns the recoded \code{GADSdat}.
#'
#'@examples
#' # create example GADS
#' dat <- data.frame(ID = 1:4, age = c(NA, 18, 21, 23),
#'                   siblings = c(0, 2, NA, NA))
#' gads <- import_DF(dat)
#'
#' # recode NAs
#' gads2 <- recodeNA2missing(gads)
#'@export
recodeNA2missing <- function(GADSdat, recodeVars = namesGADS(GADSdat), value = -99, valLabel = "missing") {
  UseMethod("recodeNA2missing")
}

#'@export
recodeNA2missing.GADSdat <- function(GADSdat, recodeVars = namesGADS(GADSdat), value = -99, valLabel = "missing") {
  check_GADSdat(GADSdat)
  if(!is.character(recodeVars) || length(recodeVars) < 1) {
    stop("'recodeVars' needs to be character vector of at least length 1.")
  }
  check_vars_in_GADSdat(GADSdat, vars = recodeVars, argName = "recodeVars")
  check_numericArgument(value, argName = "value")
  check_characterArgument(valLabel, argName = "valLabel")

  for(recodeVar in recodeVars) {
    if(any(is.na(GADSdat$dat[[recodeVar]]))) { # only variables with actual NAs should receive new meta data
      ## check for valueLabel conflicts
      labeled_values <- extractMeta(GADSdat, recodeVar)
      if(!all(is.na(labeled_values$value)) && value %in% labeled_values$value) {
        warning("'value' is already labeled for the following variable in 'recodeVars': ", recodeVar)
      }
      GADSdat <- recodeGADS(GADSdat, varName = recodeVar, oldValues = NA, newValues = value)
      GADSdat <- changeValLabels(GADSdat, varName = recodeVar, value = value, valLabel = valLabel)
      GADSdat <- changeMissings(GADSdat, varName = recodeVar, value = value, missings = "miss")
    }
  }
  GADSdat
}
