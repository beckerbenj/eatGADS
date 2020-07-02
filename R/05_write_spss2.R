
#### Writing sav files via SPSS syntax
#############################################################################
#' Write a \code{GADSdat} object to \code{txt} and \code{SPSS} syntax
#'
#' Write a \code{GADSdat} object to a text file (\code{txt}) and an accompanying \code{SPSS} syntax file containing all meta information (e.g. value and variable labels).
#'
#' This function is based on \code{eatPrep's} \code{\link[eatPrep]{writeSpss}} function. Currently under developement.
#'
#'@param GADSdat A \code{GADSdat} object.
#'@param filePath Path of \code{sav} file to write.
#'
#'@return Writes \code{sav} file to disc, returns \code{NULL}.
#'
#'@examples
#'# tbd
#'
#'@export
write_spss2 <- function(GADSdat, filePath) {
  UseMethod("write_spss")
}

#'@export
write_spss2.GADSdat <- function(GADSdat, filePath) {

  stop("Not implemented yet.")
  ## write txt

  ## write SPSS syntax

  return()
}
