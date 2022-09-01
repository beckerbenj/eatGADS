#### Remove variables from a GADSdat
#############################################################################
#' Extract or remove variables from a \code{GADSdat}.
#'
#' Extract or remove variables and their meta data from a \code{GADSdat} object.
#'
#' Both functions simply perform the variable removal or extraction from the underlying \code{data.frame}
#' in the \code{GADSdat} object followed by calling \code{\link{updateMeta}}.
#'
#'@param GADSdat \code{GADSdat} object.
#'@param vars A character vector containing the variables names in the \code{GADSdat}.
#'
#'@return Returns a \code{GADSdat} object.
#'
#'@examples
#'## create an example GADSdat
#'example_df <- data.frame(ID = 1:4,
#'                         age = c(12, 14, 16, 13),
#'                         citizenship1 = c("German", "English", "Polish", "Chinese"),
#'                         citizenship2 = c(NA, "German", "Chinese", "Polish"),
#'                         stringsAsFactors = TRUE)
#'gads <- import_DF(example_df)
#'
#'## remove variables from GADSdat
#'gads2 <- removeVars(gads, vars = c("citizenship2", "age"))
#'
#'## extract GADSdat with specific variables
#'gads3 <- extractVars(gads, vars = c("ID", "citizenship1"))

#'@export
extractVars <- function(GADSdat, vars) {
  UseMethod("extractVars")
}
#'@export
extractVars.GADSdat <- function(GADSdat, vars) {
  check_GADSdat(GADSdat)
  check_vars_in_GADSdat(GADSdat, vars = vars)

  new_dat <- GADSdat$dat[, names(GADSdat$dat) %in% vars, drop = FALSE]
  updateMeta(GADSdat, newDat = new_dat)
}

#' @export
#' @rdname extractVars
removeVars <- function(GADSdat, vars) {
UseMethod("removeVars")
}
#'@export
removeVars.GADSdat <- function(GADSdat, vars) {
  check_GADSdat(GADSdat)
  check_vars_in_GADSdat(GADSdat, vars = vars)

  new_dat <- GADSdat$dat[, !names(GADSdat$dat) %in% vars, drop = FALSE]
  updateMeta(GADSdat, newDat = new_dat)
}




