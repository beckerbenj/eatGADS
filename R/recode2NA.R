
#############################################################################
#' Recode a value to \code{NA}.
#'
#' Recode a value in multiple variables in a \code{GADSdat} to \code{NA}.
#'
#' If there are value labels given to the specified value, these are removed. Number of recodes per variable are reported.
#'
#' If a data set is imported from \code{.sav} character variables frequently contain empty strings. Especially if parts of the
#' data are written to \code{.xlsx} this can cause problems (e.g. as look up tables from \code{\link{createLookup}}),
#' as most function which write to \code{.xlsx} convert empty strings to \code{NAs}. \code{recodeString2NA} can be
#' used to recode all empty strings to \code{NA} beforehand.
#'
#'@param GADSdat A \code{GADSdat} object.
#'@param recodeVars Character vector of variable names which should be recoded.
#'@param value Which value should be recoded to \code{NA}?
#'
#'@return Returns the recoded \code{GADSdat}.
#'
#'@examples
#' # create example GADS
#' dat <- data.frame(ID = 1:4, var1 = c("", "Eng", "Aus", "Aus2"),
#'                   var2 = c("", "French", "Ger", "Ita"),
#'                   stringsAsFactors = FALSE)
#' gads <- import_DF(dat)
#'
#' # recode empty strings
#' gads2 <- recode2NA(gads)
#'
#' # recode numeric value
#' gads3 <- recode2NA(gads, recodeVars = "ID", value = 1)
#'
#'
#'@export
recode2NA <- function(GADSdat, recodeVars = namesGADS(GADSdat), value = "") {
  UseMethod("recode2NA")
}

#'@export
recode2NA.GADSdat <- function(GADSdat, recodeVars = namesGADS(GADSdat), value = "") {
  check_GADSdat(GADSdat)
  if(!is.character(recodeVars) || length(recodeVars) < 1) stop("'recodeVars' needs to be character vector of at least length 1.")
  if(!all(recodeVars %in% namesGADS(GADSdat))) stop("All variables names in 'recodeVars' need to be variables in the GADSdat.")
  if(!is.vector(value) || length(value) != 1) stop("'value' needs to be a vector of exactly length 1.")

  if(length(which(GADSdat$labels[GADSdat$labels$varName %in% recodeVars, "value"] == value)) > 0) {
   # stop("'string' is labeled in at least one of the recodeVars.")
  }

  for(recodeVar in recodeVars) {
    log_vec <- which(GADSdat[["dat"]][, recodeVar] == value)
    GADSdat[["dat"]][log_vec, recodeVar] <- NA
    message("Recodes in variable ", recodeVar, ": ", length(log_vec))
  }
  GADSdat
}
