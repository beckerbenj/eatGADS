#### Change value label
#############################################################################
#' Change value labels.
#'
#' Change or add value labels of a variable as part of a \code{GADSdat} or \code{all_GADSdat} object.
#'
#' Applied to a \code{GADSdat} or \code{all_GADSdat} object, this function is a wrapper of \code{\link{getChangeMeta}} and
#' \code{\link{applyChangeMeta}}.
#'
#'@param GADSdat \code{GADSdat} object imported via \code{eatGADS}.
#'@param varName Character string of a variable name.
#'@param value Numeric values.
#'@param valLabel Character string of the new value labels.
#'
#'@return Returns the \code{GADSdat} object with changed meta data.
#'
#'@examples
#'# Change existing value labels
#' pisa2 <- changeValLabels(pisa, varName = "repeated",
#'                         value = c(1, 2),
#'                         valLabel = c("no grade repetition", "grade repitition"))
#'
#'@export
changeValLabels <- function(GADSdat, varName, value, valLabel) {
  UseMethod("changeValLabels")
}
#'@export
changeValLabels.GADSdat <- function(GADSdat, varName, value, valLabel) {
  checkValLabelInput(varName = varName, value = value, valLabel = valLabel, labels = GADSdat$labels)
  changeTable <- getChangeMeta(GADSdat, level = "value")

  #if(identical(varName, "text1")) browser()

  existing_values <- value[value %in% changeTable[changeTable$varName == varName, "value"]]
  existing_valLabels <- valLabel[value %in% changeTable[changeTable$varName == varName, "value"]]
  new_values <- value[!value %in% changeTable[changeTable$varName == varName, "value"]]
  new_valLabels <- valLabel[!value %in% changeTable[changeTable$varName == varName, "value"]]

  for(i in seq_along(existing_values)) {
    changeTable[changeTable$varName == varName & changeTable$value == value[i], "valLabel_new"] <- existing_valLabels[i]
  }
  for(i in seq_along(new_values)) {
    change_row <- changeTable[changeTable$varName == varName, ][1, ]

    # if no other value labels exist in the first place, omit original row
    if(i == 1 && nrow(changeTable[changeTable$varName == varName, ]) == 1 && is.na(change_row$value)) {
      changeTable <- changeTable[changeTable$varName != varName, ]
    }

    change_row[, "value"] <- NA
    change_row[, "value_new"] <- new_values[i]
    change_row[, "valLabel_new"] <- new_valLabels[i]
    changeTable <- rbind(changeTable, change_row)
  }
  applyChangeMeta(GADSdat, changeTable = changeTable)
}

#'@export
changeValLabels.all_GADSdat <- function(GADSdat, varName, value, valLabel) {
  stop("This method has not been implemented yet")
}

checkValLabelInput <- function(varName, value, valLabel, labels) {
  if(!is.character(varName) || !length(varName) == 1) stop("varName is not a character vector of length 1.")
  if(!varName %in% labels$varName) stop("varName is not a variable name in the GADSdat.")
  if(length(value) != length(valLabel)) stop("value and valLabel are not of identical length.", call. = FALSE)
  return()
}


