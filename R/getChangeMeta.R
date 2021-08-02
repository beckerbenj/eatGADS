

#### Extract Change Meta data on Variable Level
#############################################################################
#' Extract table for Meta Data Changes.
#'
#' Function to obtain a data frame from a \code{GADSdat} object for for changes to meta data on variable or on value level.
#'
#' Changes on variable level include variable names (\code{varName}), variable labels (\code{varLabel}),
#' SPSS format ((\code{format})) and display width (\code{display_width}).
#' Changes on value level include values (\code{value}), value labels (\code{valLabel}) and
#' missing codes (\code{missings}).
#'
#'@param GADSdat \code{GADSdat} object imported via \code{eatGADS}.
#'@param level \code{'variable'} or \code{'value'}.
#'
#'@return Returns the meta data sheet for all variables including the corresponding change columns.
#'
#'@examples
#'# For changes on variable level
#'varChangeTable <- getChangeMeta(pisa, level = "variable")
#'
#'# For changes on value level
#'valChangeTable <- getChangeMeta(pisa, level = "value")
#'
#'@export
getChangeMeta <- function(GADSdat, level = "variable") {
  UseMethod("getChangeMeta")
}

#'@export
getChangeMeta.GADSdat <- function(GADSdat, level = "variable") {
  check_GADSdat(GADSdat)
  labels <- GADSdat[["labels"]]
  if(identical(level, "variable")) {
    oldCols <- c("varName", "varLabel", "format", "display_width")
    newCols <- paste0(oldCols, "_new")
    for(n in newCols) labels[, n] <- NA
    change_sheet <- unique(labels[, c(oldCols, newCols)])
    return(new_varChanges(change_sheet))
  }
  if(identical(level, "value")) {
    oldCols <- c("value", "valLabel", "missings")
    newCols <- paste0(oldCols, "_new")
    for(n in newCols) labels[, n] <- NA
    change_sheet <- labels[, c("varName", oldCols, newCols)]
    return(new_valChanges(change_sheet))
  }
  stop("Invalid level argument.")
}
#'@export
getChangeMeta.all_GADSdat <- function(GADSdat, level = "variable") {
  check_all_GADSdat(GADSdat)
  changeSheet_list <- lapply(names(GADSdat$datList), function(nam ) {
    single_GADSdat <- extractGADSdat(GADSdat, name = nam)
    getChangeMeta(single_GADSdat, level = level)
  })
  names(changeSheet_list) <- names(GADSdat$datList)
  changeSheet_list
}

new_varChanges <- function(df) {
  stopifnot(is.data.frame(df))
  structure(df, class = c("varChanges", "data.frame"))
}
check_varChanges <- function(changeTable) {
  if(!is.data.frame(changeTable)) stop("changeTable is not a data.frame.")
  colNames <- c("varName", "varLabel", "format", "display_width")
  colNames <- c(colNames, paste0(colNames, "_new"))
  if(any(!names(changeTable) %in% colNames)) stop("Irregular column names in changeTable.")
  # tbd: content checks for format and display width
  # SQLite compliance
  changeTable$varName_new <- sapply(changeTable$varName_new, function(x) {
    if(is.na(x)) return(NA)
    transf_names(x)
    })

  changeTable
}

new_valChanges <- function(df) {
  stopifnot(is.data.frame(df))
  structure(df, class = c("valChanges", "data.frame"))
}
check_valChanges <- function(changeTable) {
  if(!is.data.frame(changeTable)) stop("changeTable is not a data.frame.")
  # Columns
  oldCols <- c("value", "valLabel", "missings")
  newCols <- paste0(oldCols, "_new")
  colNames <- c("varName", oldCols, newCols)
  if(any(!names(changeTable) %in% colNames)) stop("Irregular column names in changeTable.")
  # Values in columns
  if(!all(changeTable[, "missings_new"] %in% c("miss", "valid") | is.na(changeTable[, "missings_new"]))) {
    stop("Irregular values in 'missings_new' column.")
  }

  # Numeric value columns
  if(is.character(changeTable[, "value_new"])) {
    changeTable[, "value_new"] <- suppressWarnings(eatTools::asNumericIfPossible(changeTable[, "value_new"],
                                                                                 force.string = FALSE))
    if(is.character(changeTable[, "value_new"])) stop("Column 'value_new' in 'changeTable' is character and can not be transformed to numeric.")
  }
  if(is.character(changeTable[, "value"])) {
    changeTable[, "value"] <- suppressWarnings(eatTools::asNumericIfPossible(changeTable[, "value"],
                                                                                 force.string = FALSE))
    if(is.character(changeTable[, "value"])) stop("Column 'value' in 'changeTable' is character and can not be transformed to numeric.")
  }

  wrong_new_miss <- which((changeTable$missings_new == "miss" | !is.na(changeTable$valLabel_new))
                          & is.na(changeTable$value) & is.na(changeTable$value_new))
  if(length(wrong_new_miss) > 0)  stop("Value 'NA' can not receive a value label.")
  changeTable
}
