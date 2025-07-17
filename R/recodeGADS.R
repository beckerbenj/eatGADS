####
#############################################################################
#' Recode variables.
#'
#' Recode one or multiple variables as part of a \code{GADSdat} or \code{all_GADSdat} object.
#'
#' Applied to a \code{GADSdat} or \code{all_GADSdat} object, this function is a wrapper of \code{\link{getChangeMeta}}
#' and \code{\link{applyChangeMeta}}. Beyond that, unlabeled variables and values are recoded as well.
#' \code{oldValues} and \code{newValues} are matched by ordering in the function call.
#'
#' If changes are performed on value levels, recoding into existing values can occur.
#' In these cases, \code{existingMeta} determines how the resulting meta data conflicts are handled,
#' either raising an error if any occur (\code{"stop"}),
#' keeping the original meta data for the value (\code{"value"}),
#' using the meta data in the \code{changeTable} and, if incomplete, from the recoded value (\code{"value_new"}),
#' or leaving the respective meta data untouched (\code{"ignore"}).
#'
#' Furthermore, one might recode multiple old values in the same new value. This is currently only possible with
#' \code{existingMeta = "drop"}, which drops all related meta data on value level, or
#' \code{existingMeta = "ignore"}, which leaves all related meta data on value level untouched.
#'
#' Missing values (\code{NA}) are supported in \code{oldValues} but not in \code{newValues}. For recoding values to
#' \code{NA} see \code{\link{recode2NA}} instead.
#' For recoding character variables, using lookup tables via \code{\link{createLookup}} is recommended. For changing
#' value labels see \code{\link{changeValLabels}}.
#'
#'@param GADSdat \code{GADSdat} object imported via \code{eatGADS}.
#'@param varName Character vector containing variable names.
#'@param oldValues Vector containing the old values.
#'@param newValues Vector containing the new values (in the respective order as \code{oldValues}).
#'@param existingMeta If values are recoded, which meta data should be used (see details)?
#'
#'@return Returns a \code{GADSdat}.
#'
#'@examples
#'# Example gads
#'example_df <- data.frame(ID = 1:5, color = c("blue", "blue", "green", "other", "other"),
#'                         animal = c("dog", "Dog", "cat", "hors", "horse"),
#'                         age = c(NA, 16, 15, 23, 50),
#'                         stringsAsFactors = FALSE)
#'example_df$animal <- as.factor(example_df$animal)
#'gads <- import_DF(example_df)
#'
#'# simple recode
#'gads2 <- recodeGADS(gads, varName = "animal",
#'                    oldValues = c(3, 4), newValues = c(7, 8))
#'@export
recodeGADS <- function(GADSdat, varName, oldValues, newValues,
                       existingMeta = c("stop", "value", "value_new", "drop", "ignore")) {
  UseMethod("recodeGADS")
}
#'@export
recodeGADS.GADSdat <- function(GADSdat, varName, oldValues, newValues,
                               existingMeta = c("stop", "value", "value_new", "drop", "ignore")) {
  check_vars_in_GADSdat(GADSdat, vars = varName, argName = "varName")
  if(length(oldValues) != length(newValues)) {
    stop("'oldValues' and 'newValues' are not of identical length.", call. = FALSE)
  }
  if(any(is.na(newValues))) {
    stop("Missing value(s) in 'newValues'. Recode to NA using recodeString2NA() if required.", call. = FALSE)
  }
  duplicated_oldValues <- unique(oldValues[duplicated(oldValues)])
  if(length(duplicated_oldValues) > 0) {
    stop("The are duplicate values in 'oldValues': ", paste(duplicated_oldValues, collapse = ", "), call. = FALSE)
  }

  changeTable <- getChangeMeta(GADSdat, level = "value")

  for(single_varName in varName) {
    changeTable <- write_recodes_to_changeTable(changeTable, varName = single_varName,
                                                oldValues = oldValues, newValues = newValues)

    # recode values without labels (not the best solution but better usability)
    other_recodes <- which(!oldValues %in% changeTable[changeTable$varName == single_varName, "value"] | is.na(oldValues))

    if(length(other_recodes) > 0) {
      data_recode_lookup <- data.frame(oldValues = oldValues[other_recodes], newValues = newValues[other_recodes])
      not_in_data_aswell <- oldValues[!oldValues %in% GADSdat$dat[, single_varName]]
      if(length(not_in_data_aswell) > 0) {
          warning("The following values in 'oldValues' are neither a labeled value in the meta data nor an actual value in ",
                  single_varName, ": ", not_in_data_aswell)
        }
      GADSdat$dat[, single_varName] <- eatTools::recodeLookup(GADSdat$dat[, single_varName], lookup = data_recode_lookup)
    }
  }

  applyChangeMeta(GADSdat, changeTable = changeTable, existingMeta = existingMeta)
}

#'@export
recodeGADS.all_GADSdat <- function(GADSdat, varName, oldValues, newValues, existingMeta = c("stop", "value", "value_new", "drop", "ignore")) {
  check_all_GADSdat(GADSdat)
  check_vars_in_GADSdat(GADSdat, vars = varName, argName = "varName")

  singleGADS_list <- lapply(names(GADSdat$datList), function(nam ) {
    singleGADS <- extractGADSdat(GADSdat, name = nam)
    specific_varNames <- varName[varName %in% names(singleGADS$dat)]
    if(length(specific_varNames) > 0) {
      singleGADS <- recodeGADS(singleGADS, varName = specific_varNames, oldValues = oldValues,
                              newValues = newValues, existingMeta = existingMeta)
    }
    singleGADS
  })
  names(singleGADS_list) <- names(GADSdat$datList)
  do.call(mergeLabels, singleGADS_list)
}

write_recodes_to_changeTable <- function(changeTable, varName, oldValues, newValues) {
  oldValues_without_na <- oldValues[!is.na(oldValues)]
  newValues_without_na <- newValues[!is.na(oldValues)]

  for(i in seq_along(oldValues_without_na)) {
    changeTable[changeTable$varName == varName &
                  !is.na(changeTable$value) & changeTable$value == oldValues_without_na[i], "value_new"] <- newValues_without_na[i]
  }
  changeTable
}
