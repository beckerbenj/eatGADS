
#### Check Names
#############################################################################
#' Check names for \code{SQLite} column name conventions and length limits.
#'
#' Checks names for \code{SQLite} column name conventions and \code{SPSS}/\code{Stata}
#' variable name limits, and applies appropriate variable name changes to
#' \code{GADSdat} or \code{all_GADSdat} objects.
#'
#' Invalid column names in a \code{SQLite} data base include
#' \itemize{
#' \item \code{SQLite} keywords (see \code{\link[eatDB]{sqlite_keywords}}),
#' \item column names with a \code{"."} in it and
#' \item duplicate variable names which arise from \code{SQLite} being case insensitive.
#' }
#'
#' Additionally, \code{SPSS} and \code{Stata} restrict the length of variable names to
#' 64 bytes (\code{SPSS}) or 32 characters (\code{Stata}).
#'
#' The corresponding variable name changes are
#' \itemize{
#' \item appending the suffix \code{"Var"} to all \code{SQLite} keywords,
#' \item changing all \code{"."} in variable names to \code{"_"},
#' \item appending \code{"_2"} to case insensitive duplicated variable names, and
#' \item appending \code{"_tr"} to variable names exceeding the limits of the chosen program.
#' }
#'
#'Note that avoiding \code{"."} in variable names is beneficial for multiple reasons, such as
#'avoiding confusion with \code{S3} methods in \code{R} and issues when exporting to \code{Stata}.
#'
#'@param GADSdat \code{GADSdat} or \code{all_GADSdat} object.
#'@param checkKeywords Logical. Should \code{SQLite} keywords be checked and modified?
#'@param checkDots Logical. Should occurrences of \code{"."} be checked and modified?
#'@param checkDuplicates Logical. Should case insensitive duplicate variable names be checked and modified?
#'@param charLimits Optional. Either \code{NULL} to disable the length check, or the program
#' (\code{SPSS} or \code{Stata}) against whose limits the names should be checked.
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
checkVarNames <- function(GADSdat, checkKeywords = TRUE, checkDots = TRUE, checkDuplicates = TRUE,
                          charLimits = c("SPSS", "Stata")) {
  UseMethod("checkVarNames")
}
#'@export
checkVarNames.GADSdat <- function(GADSdat, checkKeywords = TRUE, checkDots = TRUE, checkDuplicates = TRUE,
                                  charLimits = c("SPSS", "Stata")) {
  check_GADSdat(GADSdat)
  if (missing(charLimits)) {
    # Don't interrupt existing user code written when 'charLimits' was not yet implemented
    charLimits <- NULL
  }
  GADSdat[["labels"]][, "varName"] <- sapply(GADSdat[["labels"]][, "varName"], checkVarNames,
                                             checkKeywords, checkDots, checkDuplicates, charLimits)
  names(GADSdat[["dat"]]) <- sapply(names(GADSdat[["dat"]]), checkVarNames,
                                    checkKeywords, checkDots, checkDuplicates, charLimits)
  GADSdat
}
#'@export
checkVarNames.all_GADSdat <- function(GADSdat, checkKeywords = TRUE, checkDots = TRUE, checkDuplicates = TRUE,
                                      charLimits = c("SPSS", "Stata")) {
  check_all_GADSdat(GADSdat)
  if (missing(charLimits)) {
    # Don't interrupt existing user code written when 'charLimits' was not yet implemented
    charLimits <- NULL
  }
  GADSdat[["allLabels"]][, "varName"] <- sapply(GADSdat[["allLabels"]][, "varName"], checkVarNames,
                                                checkKeywords, checkDots, checkDuplicates, charLimits)
  GADSdat[["datList"]] <- lapply(GADSdat[["datList"]], function(df) {
    names(df) <- sapply(names(df), checkVarNames,
                        checkKeywords, checkDots, checkDuplicates, charLimits)
    df
  })
  GADSdat
}
#'@export
checkVarNames.data.frame <- function(GADSdat, checkKeywords = TRUE, checkDots = TRUE, checkDuplicates = TRUE,
                                     charLimits = c("SPSS", "Stata")) {
  if (missing(charLimits)) {
    # Don't interrupt existing user code written when 'charLimits' was not yet implemented
    charLimits <- NULL
  }
  names(GADSdat) <- checkVarNames(names(GADSdat),
                                  checkKeywords, checkDots, checkDuplicates, charLimits)
  GADSdat
}
#'@export
checkVarNames.character <- function(GADSdat, checkKeywords = TRUE, checkDots = TRUE, checkDuplicates = TRUE,
                                    charLimits = c("SPSS", "Stata")) {
  NewName <- GADSdat
  check_logicalArgument(checkKeywords)
  check_logicalArgument(checkDots)
  if(any(is.na(GADSdat))) {
    stop("Column names can not be NA.")
  }
  if (missing(charLimits)) {
    # Don't interrupt existing user code written when 'charLimits' was not yet implemented
    charLimits <- NULL
  }

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
  ## Variable name-length limits
  if (!is.null(charLimits)) {
    charLimits <- match.arg(charLimits, several.ok = TRUE)
    name_lengths_char <- nchar(NewName, type = "char")
    name_lengths_byte <- nchar(NewName, type = "byte")
    long_names <- NULL
    if ("SPSS" %in% charLimits && any(name_lengths_byte > 64)) {
      long_names <- which(name_lengths_byte > 64)
      string_limit <- list(unit = "byte", x = 64)
    }
    if ("Stata" %in% charLimits && any(name_lengths_char > 32)) {
      long_names <- unique(c(long_names,
                             which(name_lengths_char > 32)))
      string_limit <- list(unit = "char", x = 32)
    }
    for (i in long_names) {
      NewName[i] <- truncate_string(string = NewName[i],
                                    n = string_limit$x,
                                    unit = string_limit$unit)
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

truncate_string <- function(string, n, unit) {
  check_numericArgument(n)
  check_characterArgument(unit)

  if (is.character(string)) {
    if (unit == "char") {
      out <- substr(string, start = 0, stop = n - 3)
      out <- paste0(out, "_tr")
      return(out)
    }

    # limit is byte length
    byte_char_diff <- nchar(string, type = "byte") - nchar(string, type = "chars")
    if (byte_char_diff == 0) {
      out <- truncate_string(string = string, n = n, unit = "char")
      return(out)
    }

    # n_bytes != n_chars
    splited_string <- strsplit(string, split = "")
    out <- truncate_string(string = splited_string, n = n, unit = unit)
    return(out)
  }

  if (is.list(string)) {
    if (length(string) != 1) {
      stop("Invalid input! 'string' is a list but its length is != 1")
    }
    string_vec <- unlist(string)
    if (!(is.character(string_vec) && all(nchar(string_vec, type = "chars") == 1))) {
      stop("Invalid input! If 'string' is a list, each of its elements needs to be a single character.")
    }
    string_table <- data.frame(string = string_vec,
                               bytes = NA_integer_,
                               cumulative = NA_integer_,
                               below_limit = FALSE)
    string_table$bytes <- nchar(string_table$string, type = "byte")
    string_table$cumulative <- cumsum(string_table$bytes)
    string_table$below_limit <- string_table$cumulative <= (n - 3)
    last_allowed_char <- max(which(string_table$below_limit))
    out <- paste0(string_table$string[1:last_allowed_char], collapse = "")
    out <- paste0(out, "_tr")
    return(out)
  }
}
