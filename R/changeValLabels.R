#### Change value label
#############################################################################
#' Change value labels.
#'
#' Change or add value labels of one or multiple variables as part of a \code{GADSdat} object.
#'
#' Applied to a \code{GADSdat} or \code{all_GADSdat} object, this function is a wrapper
#' of \code{\link{getChangeMeta}} and \code{\link{applyChangeMeta}}.
#' The function supports changing multiple value labels (\code{valLabel}) as well as value labels of
#' multiple variables (\code{varName}) at once.
#'
#'@param GADSdat \code{GADSdat} object imported via \code{eatGADS}.
#'@param varName Character vector containing variable names.
#'@param value Numeric values which are being labeled.
#'@param valLabel Character vector of the new value labels.
#'Labels are applied in the same ordering as \code{value}.
#'
#'@return Returns the \code{GADSdat} object with changed meta data.
#'
#'@examples
#'# Change existing value labels
#' pisa2 <- changeValLabels(pisa, varName = "repeated",
#'                         value = c(1, 2),
#'                         valLabel = c("no grade repetition", "grade repitition"))
#'
#'# Add value label to unlabeled value
#' mtcars_g <- import_DF(mtcars)
#' mtcars_g2 <- changeValLabels(mtcars_g, varName = "cyl",
#'                              value = c(4, 6, 8),
#'                              valLabel = c("four", "six", "eight"))
#'
#'# Add value labels to multiple variables at once
#' mtcars_g3 <- changeValLabels(mtcars_g, varName = c("mpg", "cyl", "disp"),
#'                              value = c(-99, -98),
#'                              valLabel = c("missing", "not applicable"))
#'
#'
#'@export
changeValLabels <- function(GADSdat, varName, value, valLabel) {
  UseMethod("changeValLabels")
}
#'@export
changeValLabels.GADSdat <- function(GADSdat, varName, value, valLabel) {
  check_GADSdat(GADSdat)
  check_vars_in_GADSdat(GADSdat, vars = varName, argName = "varName")
  if(length(value) != length(valLabel)) {
    stop("value and valLabel are not of identical length.", call. = FALSE)
  }

  changeTable <- getChangeMeta(GADSdat, level = "value")

  for(single_varName in varName) {
    is_existing <- value %in% changeTable[changeTable$varName == single_varName, "value"]

    existing_values <- value[is_existing]
    existing_valLabels <- valLabel[is_existing]
    new_values <- value[!is_existing]
    new_valLabels <- valLabel[!is_existing]

    # edit change table
    for(i in seq_along(existing_values)) {
      changeTable[changeTable$varName == single_varName & changeTable$value == existing_values[i],
                  "valLabel_new"] <- existing_valLabels[i]
    }
    for(i in seq_along(new_values)) {
      change_row <- changeTable[changeTable$varName == single_varName, ][1, ]

      # if no other value labels exist in the first place, omit original row
      if(i == 1 && nrow(changeTable[changeTable$varName == single_varName, ]) == 1 &&
         is.na(change_row$value)) {
        changeTable <- changeTable[changeTable$varName != single_varName, ]
      }

      change_row[, "value"] <- NA
      change_row[, "value_new"] <- new_values[i]
      change_row[, "valLabel_new"] <- new_valLabels[i]
      changeTable <- rbind(changeTable, change_row)
    }
  }

  applyChangeMeta(GADSdat, changeTable = changeTable)
}

#'@export
changeValLabels.all_GADSdat <- function(GADSdat, varName, value, valLabel) {
  stop("This method has not been implemented yet")
}



