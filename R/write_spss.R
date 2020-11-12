
#### Writing sav files
#############################################################################
#' Write a \code{GADSdat} object to \code{sav}
#'
#' Write a \code{GADSdat} object, which contains meta information as value and variable labels to an SPSS file (\code{sav}).
#' See 'details' for some important limitations.
#'
#' The provided functionality relies on \code{havens} \code{\link[haven:read_spss]{write_sav}} function.
#' Currently known limitations are:
#' (a) Missing codes for all character variables are dropped, (b) value labels for long character variables (> \code{A10}) are
#' dropped, (c) under specific conditions very long character variables (> \code{A254}) are incorrectly displayed as multiple
#' character variables in \code{SPSS}. Furthermore, \code{write_spss} currently does not support exporting date or time variables.
#'
#'@param GADSdat A \code{GADSdat} object.
#'@param filePath Path of \code{sav} file to write.
#'
#'@return Writes \code{sav} file to disc, returns \code{NULL}.
#'
#'@examples
#'tmp <- tempfile(fileext = ".sav")
#'write_spss(pisa, tmp)
#'
#'@export
write_spss <- function(GADSdat, filePath) {
  UseMethod("write_spss")
}

#'@export
write_spss.GADSdat <- function(GADSdat, filePath) {
  df <- export_tibble(GADSdat = GADSdat)
  haven::write_sav(df, path = filePath)
  return()
}


