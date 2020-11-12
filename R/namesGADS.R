#### Get Names from GADS
#############################################################################
#' Variables names of a GADS.
#'
#' Variables names of a \code{GADSdat} object, a \code{all_GADSdat} object or a \code{eatGADS} data base.
#'
#' If the function is applied to a \code{GADSdat} object, a character vector with all variable names is returned. If the function is
#' applied to a \code{all_GADSdat} object or to the path of a \code{eatGADS} data base, a named list is returned. Each list entry
#' represents a data table in the object.
#'
#'@param GADS A \code{GADSdat} object, a \code{all_GADSdat} or the path to an existing \code{eatGADS} data base.
#'
#'@return Returns a character vector or a named list of character vectors.
#'
#'@examples
#'# Extract variable names from data base
#'db_path <- system.file("extdata", "pisa.db", package = "eatGADS")
#'namesGADS(db_path)
#'
#'# Extract variable names  from loaded/imported GADS
#'namesGADS(pisa)
#'
#'@export
namesGADS <- function(GADS) {
  UseMethod("namesGADS")
}

#'@export
namesGADS.character <- function(GADS) {
  eatDB::dbNames(filePath = GADS, includeMeta = FALSE)
}
#'@export
namesGADS.GADSdat <- function(GADS) {
  check_GADSdat(GADS)
  names(GADS$dat)
}

#'@export
namesGADS.all_GADSdat <- function(GADS) {
  check_all_GADSdat(GADS)
  lapply(GADS$dat, names)
}
