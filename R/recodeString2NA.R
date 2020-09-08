
#### Empty strings to NA
#############################################################################
#' Recode a string to \code{NA}.
#'
#' Recode a string in multiple variables in a \code{GADSdat} to \code{NA}.
#'
#' A check is performed, whether there are no value labels given to the specified string. Number of recodes per variable are reported.
#'
#' If a data set is imported from \code{.sav} character variables frequently contain empty strings. Especially if parts of the
#' data are written to \code{.xlsx} this can cause problems (e.g. as look up tables from \code{\link{createLookup}}),
#' as most function which write to \code{.xlsx} convert empty strings to \code{NAs}. \code{recodeString2NA} can be
#' used to recode all empty strings to \code{NA} beforehand.
#'
#'@param GADSdat A \code{GADSdat} object.
#'@param recodeVars Character vector of variable names which should be recoded.
#'@param string Which string should be recoded to \code{NA}?
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
#' gads2 <- recodeString2NA(gads)
#'
#'@export
recodeString2NA <- function(GADSdat, recodeVars = namesGADS(GADSdat), string = "") {
  UseMethod("recodeString2NA")
}

#'@export
recodeString2NA.GADSdat <- function(GADSdat, recodeVars = namesGADS(GADSdat), string = "") {
  if(!is.character(recodeVars) || length(recodeVars) < 1) stop("'recodeVars' needs to be character vector of at least length 1.")
  if(!all(recodeVars %in% namesGADS(GADSdat))) stop("All variables names in 'recodeVars' need to be variables in the GADSdat.")
  if(!is.character(string) || length(string) != 1) stop("'string' needs to be a character vector of exactly length 1.")

  if(length(which(GADSdat$labels[GADSdat$labels$varName %in% recodeVars, "value"] == string)) > 0) {
    stop("'string' is labeled in at least one of the recodeVars.")
  }

  for(recodeVar in recodeVars) {
    log_vec <- which(GADSdat[["dat"]][, recodeVar] == string)
    GADSdat[["dat"]][log_vec, recodeVar] <- NA
    message("Recodes in variable ", recodeVar, ": ", length(log_vec))
  }
  GADSdat
}


