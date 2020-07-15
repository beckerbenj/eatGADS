#### Get Names from GADS
#############################################################################
#' Variables names of a GADS data base.
#'
#' Returns a list of all variable names included in the GADS data base.
#'
#' Extracts names of all variables included in the relational data base, structured as a list with the individual data tables as list elements.
#'
#'@param GADS Path of an existing \code{eatGADS} data base.
#'
#'@return Returns a named list of variable names per data table.
#'
#'@examples
#'\dontrun{
#'varNames <- namesGADS("t:/_R_Tutorials/R_Workshops/04_eatPakete/minigads_2010.db")
#'
#'# all variable names sorted by data table
#'varNames
#'
#'# variables in a specific data table
#'varNames$allDat
#'
#'}
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
  names(GADS$dat)
}
