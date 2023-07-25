
#### Writing files
#############################################################################
#' Write a \code{GADSdat} object to a file
#'
#' Write a \code{GADSdat} object, which contains meta information as value and variable labels to an \code{SPSS} file (\code{sav})
#' or \code{Stata} file (\code{dta}).
#' See 'details' for some important limitations.
#'
#' The provided functionality relies on \code{havens} \code{\link[haven:read_spss]{write_sav}} and
#' \code{\link[haven:read_dta]{write_dta}} functions.
#'
#' Currently known limitations for \code{write_spss} are:
#' \itemize{
#' \item{a) } {value labels for long character variables (> \code{A10}) are dropped,}
#' \item{b) } {under specific conditions very long character variables (> \code{A254}) are incorrectly
#' displayed as multiple character variables in \code{SPSS},}
#' \item{c) } {exporting date or time variables is currently not supported,}
#' \item{d) } {missing tags are slightly incompatible between \code{SPSS} and \code{eatGADS}
#' as \code{eatGADS} supports unlimited discrete missing tags (but no range of missing tags) and
#' \code{SPSS} only supports up to three discrete missing tags or ranges of missing tags. For this purpose, if a variable
#' is assigned more than three discrete missing tags, \code{write_spss()} (more precisely \code{\link{export_tibble}})
#' performs a silent conversion of the discrete missing tags into a missing range.
#' If this conversion affects other value labels or values in the data not tagged as missing, an error is issued.}
#'}
#'
#' Currently known limitations for \code{write_stata} are:
#' \itemize{
#' \item{a) }{Variable format is dropped,}
#' \item{b) }{missing codes are dropped.}
#' }
#'
#'@param GADSdat A \code{GADSdat} object.
#'@param filePath Path of \code{sav} file to write.
#'
#'@return Writes file to disc, returns \code{NULL}.
#'
#'@examples
#'
#'# write to spss
#'tmp <- tempfile(fileext = ".sav")
#'write_spss(pisa, tmp)
#'
#'# write to stata
#'tmp <- tempfile(fileext = ".dta")
#'write_stata(pisa, tmp)
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

#' @rdname write_spss
#'@export
write_stata <- function(GADSdat, filePath) {
  UseMethod("write_stata")
}

#'@export
write_stata.GADSdat <- function(GADSdat, filePath) {
  df <- export_tibble(GADSdat = GADSdat)
  warning("Missing codes and variable formats are dropped when writing to '.dta'.")
  haven::write_dta(df, path = filePath)
  return()
}

