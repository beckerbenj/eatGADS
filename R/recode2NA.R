
#############################################################################
#' Recode values to \code{NA}.
#'
#' Recode multiple values in multiple variables in a \code{GADSdat} to \code{NA}.
#'
#' If there are value labels given to the specified value, a warning is issued. Number of recodes per variable are reported.
#'
#' If a data set is imported from \code{.sav}, character variables frequently contain empty strings. Especially if parts of the
#' data are written to \code{.xlsx}, this can cause problems (e.g. as look up tables from \code{\link{createLookup}}),
#' as most function which write to \code{.xlsx} convert empty strings to \code{NAs}. \code{recodeString2NA} can be
#' used to recode all empty strings to \code{NA} beforehand.
#'
#'@param GADSdat A \code{GADSdat} object.
#'@param recodeVars Character vector of variable names which should be recoded.
#'@param value Which values should be recoded to \code{NA}?
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
#' gads3 <- recode2NA(gads, recodeVars = "ID", value = 1:3)
#'
#'
#'@export
recode2NA <- function(GADSdat, recodeVars = namesGADS(GADSdat), value = "") {
  UseMethod("recode2NA")
}

#'@export
recode2NA.GADSdat <- function(GADSdat, recodeVars = namesGADS(GADSdat), value = "") {
  check_GADSdat(GADSdat)
  if(!is.character(recodeVars) || length(recodeVars) < 1) {
    stop("'recodeVars' needs to be character vector of at least length 1.")
  }
  check_vars_in_GADSdat(GADSdat, vars = recodeVars, argName = "recodeVars")
  if(!is.vector(value) || length(value) == 0) {
    stop("'value' needs to be a vector of at least length 1.")
  }

  labeled_values <- GADSdat$labels[GADSdat$labels$varName %in% recodeVars, c("varName", "value")]
  labeled_values_recode <- unique(labeled_values[which(labeled_values$value %in% value), "varName"])
  if(length(labeled_values_recode) > 0) {
   warning("Some 'value' is labeled in the following variables in 'recodeVars': ",
        paste(labeled_values_recode, collapse = ", "))
  }

  for(recodeVar in recodeVars) {
    log_vec <- which(GADSdat[["dat"]][, recodeVar] %in% value)
    GADSdat[["dat"]][log_vec, recodeVar] <- NA
    message("Recodes in variable ", recodeVar, ": ", length(log_vec))
  }
  GADSdat
}
