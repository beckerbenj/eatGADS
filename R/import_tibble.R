#### Import SPSS data
#############################################################################
#' Import tibble
#'
#' Function to import a tibble while extracting meta information, e.g. variable and value labels.
#'
#' Tibbles may store variable and value labels as well as missing tags via the \code{labelled} class. \code{import_tibble}
#' restructures this meta information separately in a long format \code{data.frame}. Value labels and missing tags are used
#' to identify missing tags (see \code{\link{checkMissings}}). Time and date variables are converted to character.
#'
#'@param tibble A \code{tibble} object.
#'@param checkVarNames Should variable names be checked for violations of \code{SQLite} and \code{R} naming rules?
#'@param labeledStrings Should strings as labeled values be allowed? If \code{"drop"} (default), all labeled strings are dropped and \code{NAs} occur in the meta data. If \code{"transform"}, all underlying values are transformed to numeric. If \code{"keep"}, value labels stay untouched. However, the latter possibly corrupts all labeled values.
#'
#'@return Returns a list with the actual data \code{dat} and a data frame with all meta information in long format \code{labels}.
#'
#'@examples
#'# Use spss data from within package
#'spss_path <- system.file("extdata", "pisa.zsav", package = "eatGADS")
#'pisa_gads <- import_spss(spss_path)
#'
#'@export
import_tibble <- function(tibble, checkVarNames = TRUE, labeledStrings = c("drop", "keep", "transform")) {
  labeledStrings <- match.arg(labeledStrings)

  out <- prepare_labels(rawDat = tibble, checkVarNames = checkVarNames, labeledStrings = labeledStrings)
  out
}

