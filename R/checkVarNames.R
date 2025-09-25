
#### Check Names
#############################################################################
#' Check names for \code{SQLite} column name conventions and length limits.
#'
#' Checks names for \code{SQLite} column name conventions and \code{SPSS}/\code{Stata}
#' variable name limits, and applies appropriate variable name changes to
#' \code{GADSdat} or \code{all_GADSdat} objects.
#'
#' @details
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
#' \item changing all \code{"."} in variable names to \code{"_"}, and
#' \item appending \code{"_2"} to case insensitive duplicated variable names.
#' }
#'
#'Note that avoiding \code{"."} in variable names is beneficial for multiple reasons, such as
#'avoiding confusion with \code{S3} methods in \code{R} and issues when exporting to \code{Stata}.
#'
#' \subsection{Variable name length limits}{
#' The length of variable names is limited to 64 \emph{bytes} in \code{SPSS} and to 32
#'  \emph{characters} in \code{Stata}. If more than one program name is provided in
#'  \code{charLimits}, the most restrictive among the chosen limits will be applied. Variable names
#'  exceeding that limit will be truncated and marked with the suffix \code{"_tr"}.
#'}
#'
#'@param GADSdat \code{GADSdat} or \code{all_GADSdat} object.
#'@param checkKeywords Logical. Should \code{SQLite} keywords be checked and modified?
#'@param checkDots Logical. Should occurrences of \code{"."} be checked and modified?
#'@param checkDuplicates Logical. Should case insensitive duplicate variable names be checked and modified?
#'@param charLimits Optional character vector of one or more program names(s) for the
#' limit check (see details). Currently, these are implemented: \code{c("SPSS", "Stata")}
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
                          charLimits = NULL) {
  UseMethod("checkVarNames")
}
#'@export
checkVarNames.GADSdat <- function(GADSdat, checkKeywords = TRUE, checkDots = TRUE, checkDuplicates = TRUE,
                                  charLimits = NULL) {
  check_GADSdat(GADSdat)
  GADSdat[["labels"]][, "varName"] <- sapply(GADSdat[["labels"]][, "varName"], checkVarNames,
                                             checkKeywords, checkDots, checkDuplicates, charLimits)
  names(GADSdat[["dat"]]) <- sapply(names(GADSdat[["dat"]]), checkVarNames,
                                    checkKeywords, checkDots, checkDuplicates, charLimits)
  GADSdat
}
#'@export
checkVarNames.all_GADSdat <- function(GADSdat, checkKeywords = TRUE, checkDots = TRUE, checkDuplicates = TRUE,
                                      charLimits = NULL) {
  check_all_GADSdat(GADSdat)
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
                                     charLimits = NULL) {
  names(GADSdat) <- checkVarNames(names(GADSdat),
                                  checkKeywords, checkDots, checkDuplicates, charLimits)
  GADSdat
}
#'@export
checkVarNames.character <- function(GADSdat, checkKeywords = TRUE, checkDots = TRUE, checkDuplicates = TRUE,
                                    charLimits = NULL) {
  NewName <- GADSdat
  check_logicalArgument(checkKeywords)
  check_logicalArgument(checkDots)
  if(any(is.na(GADSdat))) {
    stop("Column names can not be NA.")
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
    charLimits <- match.arg(arg = charLimits,
                            choices = c("SPSS", "Stata"),
                            several.ok = TRUE)
    # get most restrictive limit incl. unit to be applied in nchar()
    limit_list <- getProgramLimit(program = charLimits,
                                  component = "varNames")

    name_lengths <- nchar(NewName, type = limit_list$unit)
    long_names <- which(name_lengths > limit_list$x)

    for (i in long_names) {
      NewName[i] <- truncate_string(string = NewName[i],
                                    n = limit_list$x,
                                    unit = limit_list$unit)
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
  check_characterArgument(string)
  check_numericArgument(n)
  check_characterArgument(unit)
  trunc_suffix <- "_tr"

  # limit is character length
  if (unit == "char") {
    out <- substr(string, start = 0, stop = n - nchar(trunc_suffix))
    out <- paste0(out, trunc_suffix)
    return(out)
  }

  # limit is byte length
  byte_char_diff <- nchar(string, type = "byte") - nchar(string, type = "chars")
  if (byte_char_diff == 0) {
    out <- truncate_string(string = string, n = n, unit = "char")
    return(out)
  }

  # n_bytes != n_chars
  last_char_below_limit <- find_byte_length(string = string,
                                            n = n - nchar(trunc_suffix))
  out <- substr(string, start = 0, stop = last_char_below_limit)
  out <- paste0(out, trunc_suffix)
  return(out)
}


find_byte_length <- function(string, n) {
  # return the position at which a string has to be cut to have a byte-length of <= n
  check_characterArgument(string)
  check_numericArgument(n)

  single_chars <- unlist(strsplit(string, split = ""))
  single_bytes <- nchar(single_chars, type = "byte")
  cumulated_bytes <- cumsum(single_bytes)
  char_below_limit <- cumulated_bytes <= n
  last_below_limit <- max(which(char_below_limit))
}
