
#### Check Names
#############################################################################
#' Check names for \code{SQLite} column name conventions.
#'
#' Checks names for \code{SQLite} column name conventions and
#' applies appropriate variable name changes to \code{GADSdat} or \code{all_GADSdat} objects.
#'
#' Invalid column names in a \code{SQLite} data base include
#' \itemize{
#' \item \code{SQLite} keywords (see \code{\link[eatDB]{sqlite_keywords}}),
#' \item column names with a \code{"."} in it and
#' \item duplicate variable names which arise from \code{SQLite} being case insensitive.
#' }
#'
#' The corresponding variable name changes are
#' \itemize{
#' \item appending the suffix \code{"Var"} to all \code{SQLite} keywords,
#' \item changing all \code{"."} in variable names to \code{"_"} and
#' \item appending \code{"_2"} to case insensitive duplicated variable names.
#' }
#'
#'Note that avoiding \code{"."} in variable names is beneficial for multiple reasons, such as
#'avoiding confusion with \code{S3} methods in \code{R} and issues when importing from \code{Stata}.
#'
#'@param GADSdat \code{GADSdat} or \code{all_GADSdat} object.
#'@param checkKeywords Logical. Should \code{SQLite} keywords be checked and modified?
#'@param checkDots Logical. Should occurrences of \code{"."} be checked and modified?
#'@param checkDuplicates Logical. Should case insensitive duplicate variable names be checked and modified?
#'
#'@return Returns the original object with updated variable names.
#'
#'@examples
#'# Change example data set (create an invalid variable name)
#' pisa2 <- changeVarNames(pisa, oldNames = "computer_age",
#'                         newNames = "computer.age")
#'
#' pisa3 <- checkVarNames(pisa2)
#'
#'@export
checkVarNames <- function(GADSdat, checkKeywords = TRUE, checkDots = TRUE, checkDuplicates = TRUE) {
  UseMethod("checkVarNames")
}
#'@export
checkVarNames.GADSdat <- function(GADSdat, checkKeywords = TRUE, checkDots = TRUE, checkDuplicates = TRUE) {
  check_GADSdat(GADSdat)
  GADSdat[["labels"]][, "varName"] <- sapply(GADSdat[["labels"]][, "varName"], checkVarNames)
  names(GADSdat[["dat"]]) <- sapply(names(GADSdat[["dat"]]), checkVarNames)
  GADSdat
}
#'@export
checkVarNames.all_GADSdat <- function(GADSdat, checkKeywords = TRUE, checkDots = TRUE, checkDuplicates = TRUE) {
  check_all_GADSdat(GADSdat)
  GADSdat[["allLabels"]][, "varName"] <- sapply(GADSdat[["allLabels"]][, "varName"], checkVarNames)
  GADSdat[["datList"]] <- lapply(GADSdat[["datList"]], function(df) {
    names(df) <- sapply(names(df), checkVarNames)
    df
  })
  GADSdat
}
#'@export
checkVarNames.data.frame <- function(GADSdat, checkKeywords = TRUE, checkDots = TRUE, checkDuplicates = TRUE) {
  names(GADSdat) <- checkVarNames(names(GADSdat))
  GADSdat
}
#'@export
checkVarNames.character <- function(GADSdat, checkKeywords = TRUE, checkDots = TRUE, checkDuplicates = TRUE) {
  NewName <- GADSdat
  check_logicalArgument(checkKeywords)
  check_logicalArgument(checkDots)
  if(any(is.na(GADSdat))) {
    stop("Column names can not be NA.")
  }

  #browser()
  ## SQLite Keywords
  if(checkKeywords) {
    keyword_matches <- tolower(GADSdat) %in%  tolower(eatDB::sqlite_keywords)
    NewName[keyword_matches] <- paste0(GADSdat[keyword_matches], "Var")
    NewName <- make.names(NewName)
  }
  ## Dots
  if(checkDots){
    NewName <- gsub("\\.", "_", NewName)
  }
  ## Check for duplicates due to SQLite ignoring case
  if(checkDuplicates) {
    if(anyDuplicated(tolower(NewName))) {
      NewName <- unduplicate(NewName)
    }
  }

  ## report all changes
  which_changed <- which(NewName != GADSdat)
  if(length(which_changed) > 0) {
    for(i in which_changed) {
      message(paste(GADSdat[i], "has been renamed to", NewName[i]))
    }
  }
  NewName
}

# sqlite3 not case sensitive!
unduplicate <- function(x) {
  out <- x
  allower <- tolower(x)
  out[duplicated(allower)] <- paste(out[duplicated(allower)], 2, sep = "_")
  if(anyDuplicated(tolower(out))) out <- unduplicate(out)

  out
}
