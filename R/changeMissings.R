####
#############################################################################
#' Change missing code.
#'
#' Change or add missing codes of a variable as part of a \code{GADSdat} or \code{all_GADSdat} object.
#'
#' Applied to a \code{GADSdat} or \code{all_GADSdat} object, this function is a wrapper of \code{\link{getChangeMeta}} and
#' \code{\link{applyChangeMeta}}.
#'
#'@param GADSdat \code{GADSdat} object imported via \code{eatGADS}.
#'@param varName Character string of a variable name.
#'@param value Numeric values.
#'@param missings Character string of the new missing codes, either \code{"miss"} or \code{"valid"}.
#'
#'@return Returns the \code{GADSdat} object with changed meta data.
#'
#'@examples
#'# Set a specific value to missing
#' pisa2 <- changeMissings(pisa, varName = "computer_age",
#'                         value = 5, missings = "miss")
#'
#'# Set multiple values to missing
#' pisa3 <- changeMissings(pisa, varName = "computer_age",
#'                         value = 1:4,
#'                         missings = c("miss", "miss", "miss", "miss"))
#'
#'# Set a specific value to not missing
#' pisa4 <- changeMissings(pisa2, varName = "computer_age",
#'                         value = 5, missings = "valid")
#'
#'@export
changeMissings <- function(GADSdat, varName, value, missings) {
  UseMethod("changeMissings")
}
#'@export
changeMissings.GADSdat <- function(GADSdat, varName, value, missings) {
  checkMissingsInput(varName = varName, value = value, missings = missings, labels = GADSdat$labels)
  changeTable <- getChangeMeta(GADSdat, level = "value")

  existing_values <- value[value %in% changeTable[changeTable$varName == varName, "value"]]
  existing_missings <- missings[value %in% changeTable[changeTable$varName == varName, "value"]]
  new_values <- value[!value %in% changeTable[changeTable$varName == varName, "value"]]
  new_missings <- missings[!value %in% changeTable[changeTable$varName == varName, "value"]]

  for(i in seq_along(existing_values)) {
    changeTable[changeTable$varName == varName & changeTable$value == value[i], "value_new"] <-
      changeTable[changeTable$varName == varName & changeTable$value == value[i], "value"]
    changeTable[changeTable$varName == varName & changeTable$value == value[i], "valLabel_new"] <-
      changeTable[changeTable$varName == varName & changeTable$value == value[i], "valLabel"]
    changeTable[changeTable$varName == varName & changeTable$value == value[i], "missings_new"] <- missings[i]
  }
  for(i in seq_along(new_values)) {
    change_row <- changeTable[changeTable$varName == varName, ][1, ]

    # if no other value labels exist in the first place, omit original row
    if(i == 1 && nrow(changeTable[changeTable$varName == varName, ]) == 1) {
      changeTable <- changeTable[changeTable$varName != varName, ]
    }

    change_row[, "value"] <- NA
    change_row[, "value_new"] <- new_values[i]
    change_row[, "missings_new"] <- new_missings[i]
    changeTable <- rbind(changeTable, change_row)
  }

  applyChangeMeta(GADSdat, changeTable = changeTable)
}

#'@export
changeMissings.all_GADSdat <- function(GADSdat, varName, value, missings) {
  stop("This method has not been implemented yet")
}

checkMissingsInput <- function(varName, value, missings, labels) {
  if(!is.character(varName) || !length(varName) == 1) stop("'varName' is not a character vector of length 1.")
  if(!varName %in% labels$varName) stop("'varName' is not a variable name in the GADSdat.")
  if(length(value) != length(missings)) stop("'value' and 'missings' are not of identical length.", call. = FALSE)
  if(!all(missings %in% c("miss", "valid"))) stop("All values in 'missings' need to be 'miss' or 'valid'.")
  return()
}

