#### Change spss format
#############################################################################
#' Change SPSS format.
#'
#' Change the SPSS format of a variable as part of a \code{GADSdat} or \code{all_GADSdat} object.
#'
#' Applied to a \code{GADSdat} or \code{all_GADSdat} object, this function is a wrapper of \code{\link{getChangeMeta}} and \code{\link{applyChangeMeta}}.
#'
#'@param GADSdat \code{GADSdat} object imported via \code{eatGADS}.
#'@param varName Character string of variable names.
#'@param format A single string containing the new SPSS format, for example 'A25' or 'F10'.
#'
#'@return Returns the \code{GADSdat} object with changed meta data..
#'
#'@examples
#' pisa2 <- changeSPSSformat(pisa, varName = "idstud",
#'                         format = "F10.0")
#'
#'
#'@export
changeSPSSformat <- function(GADSdat, varName, format) {
  UseMethod("changeSPSSformat")
}
#'@export
changeSPSSformat.GADSdat <- function(GADSdat, varName, format) {
  check_GADSdat(GADSdat)
  if(!all(varName %in% namesGADS(GADSdat))) stop("varName are not all variables in the GADSdat.")
  if(!is.character(format) || length(format) != 1) stop("format has to be a single character value.")
  #other format checks performed in applyChangeMeta

  changeTable <- getChangeMeta(GADSdat, level = "variable")
  for(i in seq_along(varName)) {
    changeTable[changeTable$varName == varName[i], "format_new"] <- format
  }
  GADSdat_out <- applyChangeMeta(GADSdat, changeTable = changeTable)
  check_var_type(GADSdat_out)
  GADSdat_out
}

#'@export
changeVarLabels.all_GADSdat <- function(GADSdat, varName, varLabel) {
  stop("This method has not been implemented yet")
}


check_format_vector <- function(format) {
  format <- format[!is.na(format)]
  if(length(format) == 0) return()

  if(!any(grepl("^A|^F", format))) stop("format has to start with A (string) or F (numeric).")
  format_for_check <- gsub("\\.", "", format)
  if(any(nchar(format_for_check) > 4)) stop("format has to have maximum 3 numbers (width) after its type.")
  format_numbers <- substr(format_for_check, 2, nchar(format))
  if(any(!grepl("^[0-9]*$", format_numbers))) stop("format can only have numbers (width) after its type.")
  return()
}
