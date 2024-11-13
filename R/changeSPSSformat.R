
#' Change SPSS format.
#'
#' Change the SPSS format of one or multiple variables as part of a \code{GADSdat} object.
#'
#' Applied to a \code{GADSdat} or \code{all_GADSdat} object, this function is a wrapper
#' of \code{\link{getChangeMeta}} and \code{\link{applyChangeMeta}}.
#'
#' SPSS format is supplied following SPSS logic. \code{'A'} represents character variables,
#' \code{'F'} represents numeric variables. The number following this letter represents the maximum width.
#' Optionally, another number can be added after a dot, representing the number of decimals
#' in case of a numeric variable. For instance, \code{'F8.2'} is used for a numeric variable with
#' a maximum width of 8 with 2 decimal numbers.
#'
#'@param GADSdat \code{GADSdat} object imported via \code{eatGADS}.
#'@param varName Character vector of variable names.
#'@param format A single string containing the new SPSS format, for example 'A25' or 'F10'.
#'
#'@return Returns the \code{GADSdat} object with changed meta data..
#'
#'@examples
#' # change SPSS format for a single variable (numeric variable with no decimals)
#' pisa2 <- changeSPSSformat(pisa, varName = "idstud",
#'                           format = "F10.0")
#'
#' # change SPSS format for multiple variables (numeric variable with no decimals)
#' pisa2 <- changeSPSSformat(pisa, varName = c("idstud", "idschool"),
#'                           format = "F10.0")
#'
#'@export
changeSPSSformat <- function(GADSdat, varName, format) {
  UseMethod("changeSPSSformat")
}
#'@export
changeSPSSformat.GADSdat <- function(GADSdat, varName, format) {
  check_GADSdat(GADSdat)
  check_vars_in_GADSdat(GADSdat, vars = varName, argName = "varName")
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

# used in applyChangeMeta
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
