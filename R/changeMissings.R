####
#############################################################################
#' Change missing code.
#'
#' Change or add missing codes of one or multiple variables as part of a \code{GADSdat} object.
#'
#' Applied to a \code{GADSdat} or \code{all_GADSdat} object, this function is a wrapper of
#'  \code{\link{getChangeMeta}} and \code{\link{applyChangeMeta}}.
#' The function supports changing multiple missing tags (\code{missings}) as well as missing tags of
#' multiple variables (\code{varName}) at once.
#'
#'@param GADSdat \code{GADSdat} object imported via \code{eatGADS}.
#'@param varName Character vector containing variable names.
#'@param value Numeric values.
#'@param missings Character vector of the new missing codes, either \code{"miss"} or \code{"valid"}.
#'Missings tags are applied in the same ordering as \code{value}.
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
#'# Add missing tags to multiple variables
#' pisa5 <- changeMissings(pisa, varName = c("g8g9", "computer_age"),
#'                         value = c(-99, -98), missings = c("miss", "miss"))
#'
#'@export
changeMissings <- function(GADSdat, varName, value, missings) {
  UseMethod("changeMissings")
}
#'@export
changeMissings.GADSdat <- function(GADSdat, varName, value, missings) {
  check_GADSdat(GADSdat)
  check_vars_in_GADSdat(GADSdat, vars = varName, argName = "varName")
  if(length(value) != length(missings)) {
    stop("'value' and 'missings' are not of identical length.", call. = FALSE)
  }
  if(!all(missings %in% c("miss", "valid"))) {
    stop("All values in 'missings' need to be 'miss' or 'valid'.")
  }

  changeTable_ori <- getChangeMeta(GADSdat, level = "value")
  changeTable <- changeTable_ori

  for(single_varName in varName) {
    is_existing <- value %in% changeTable[changeTable$varName == single_varName, "value"]

    existing_values <- value[is_existing]
    existing_missings <- missings[is_existing]
    new_values <- value[!is_existing]
    new_missings <- missings[!is_existing]

    for(i in seq_along(existing_values)) {
      filterValue <- changeTable$varName == single_varName & changeTable$value == existing_values[i]

      changeTable[filterValue, "value_new"] <- changeTable[filterValue, "value"]
      changeTable[filterValue, "valLabel_new"] <- changeTable[filterValue, "valLabel"]
      changeTable[filterValue, "missings_new"] <- existing_missings[i]
    }

    for(i in seq_along(new_values)) {
      change_row <- changeTable_ori[changeTable_ori$varName == single_varName, ][1, ]

      # if no other value labels or missing tags exist in the first place, omit original row
      if(i == 1 &&
         is.na(changeTable[changeTable$varName == single_varName, "value"][1]) &&
         nrow(changeTable[changeTable$varName == single_varName, ]) == 1) {
        changeTable <- changeTable[changeTable$varName != single_varName, ]
      }

      change_row[, "value"] <- NA
      change_row[, "value_new"] <- new_values[i]
      change_row[, "missings_new"] <- new_missings[i]
      changeTable <- rbind(changeTable, change_row)
    }
  }
  applyChangeMeta(GADSdat, changeTable = changeTable)
}

#'@export
changeMissings.all_GADSdat <- function(GADSdat, varName, value, missings) {
  stop("This method has not been implemented yet")
}


