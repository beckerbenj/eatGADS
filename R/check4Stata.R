
#### Check for Stata export
#############################################################################
#' Check a \code{GADSdat} for compatibility with \code{Stata}.
#'
#' This wrapper performs all relevant checks tp assess if a \code{GADSdat} complies with all of
#'  Stata's dataset requirements. Run this before exporting a dataset as \code{.dta}, using
#'  \link{write_stata}.
#'
#' Specifically, the following requirements are tested:
#' \enumerate{
#'  \item Variable names do not contain dots or special characters.
#'  \item Variable names are not longer than the specific limit (\link{checkVarNames}).
#'  \item There are no labeled fractional values like \code{99.9} (\link{checkLabeledFractionals}).
#'  \item All labeled values can be coerced \code{as.integer} (\link{checkIntOverflow}).
#'  \item Variable labels and value labels are not longer than the specific limits
#'   (\link{checkVarLabels}; \link{checkValLabels}).
#'  \item String variables do not have value labels. Currently, this is not allowed for
#'    \code{GADSdat} objects, anyway.
#'  \item String variables do not contain string values that are longer than the specific limit.
#'  \item The numbers of rows/observations and columns/variables do not exceed the specific limits.
#' }
#' Limits to different aspects of the dataset differ between the versions of the software. By
#'  default, compliance with the limits for \code{Stata 19/SE} is checked. Checks against the
#'  limits for \code{Stata 19/BE} or \code{Stata 19/MP} can be requested by specifying
#'  \code{version} with the corresponding string. For more details, see \link{program_limits}.
#'
#' @param GADSdat A \code{GADSdat} object.
#' @param version Optional single string to request checks for a different Stata version
#'  (see details)
#'
#' @returns A list of check results (enumerated corresponding to the list above), headed by an
#'  overall \code{verdict} if the \code{GADSdat} is compatible (\code{TRUE}) or if problems have
#'  been detected (\code{FALSE}).
#'
#' @family dataset compliance checks
#'
#' @examples
#' check4Stata(pisa)
#'
#' @export
check4Stata <- function(GADSdat, version = NULL) {
  check_GADSdat(GADSdat)
  if (is.null(version)) {
    version <- "Stata"
  } else {
    version <- match.arg(version, c("Stata 19/BE", "Stata 19/MP"))
  }

  out <- list()

  # varNames with dots or special characters
  other_gads <- suppressMessages(checkVarNames(GADSdat = GADSdat,
                                               checkKeywords = FALSE,
                                               checkDots = TRUE,
                                               charLimits = NULL))
  names_fixEncoding <- suppressMessages(fixEncoding(namesGADS(GADSdat)))
  out$r1_dots <- namesGADS(GADSdat)[!namesGADS(GADSdat) %in% namesGADS(other_gads)]
  out$r1_specchars <- namesGADS(GADSdat)[!namesGADS(GADSdat) %in% names_fixEncoding]
  #attr(out[[2]], "label") <- "1 - variable names with dots"
  #attr(out[[3]], "label") <- "1 - variable names with special characters"

  # varNames too long
  names_truncated <- suppressMessages(checkVarNames(GADSdat = namesGADS(GADSdat),
                                                    checkKeywords = FALSE,
                                                    checkDots = FALSE,
                                                    charLimits = "Stata"))
  out$r2_longNames <- namesGADS(GADSdat)[!namesGADS(GADSdat) %in% names_truncated]
  #attr(out[[4]], "label") <- "2 - long variable names"

  # labeled fractional values
  out$r3_labeledFractionals <- checkLabeledFractionals(GADSdat = GADSdat)
  #attr(out[[5]], "label") <- "3 - labeled fractional values"

  # too large labeled values
  out$r4_largeIntegers <- checkIntOverflow(GADSdat = GADSdat)
  #attr(out[[6]], "label") <- "4 - labeled values that are too large to coerce"

  # varLabels or valLabels too long
  out$r5_varLabels <- suppressMessages(checkVarLabels(GADSdat = GADSdat,
                                                      charLimits = "Stata"))
  out$r5_valLabels <- suppressMessages(checkValLabels(GADSdat = GADSdat,
                                                      charLimits = "Stata"))

  # labeled strings
  char_vars <- namesGADS(GADSdat)[give_GADSdat_classes(GADSdat) == "character"]
  ## deactivated because check_GADSdat() already blocks labeled (true) strings ##
  # meta_of_char_vars <- extractMeta(GADSdat, char_vars)
  # out$r6_labeledStrings <- meta_of_char_vars[!is.na(meta_of_char_vars$valLabel),
  #                                            c("varName", "varLabel", "value", "valLabel", "missings")]
  out$r6_labeledStrings <- GADSdat$labels[0,]

  # long strings
  limit_list <- getProgramLimit(version, "stringvars")
  report_long_strings <- data.frame(varName = character(),
                                    string = character())
  for (single_var in char_vars) {
    string_lengths <- nchar(GADSdat$dat[, single_var], type = limit_list$unit)
    string_too_long <- string_lengths > limit_list$value
    if (any(string_too_long)) {
      strings_truncated <- unlist(lapply(GADSdat$dat[string_too_long, single_var],
                                         truncate_string,
                                         n = 40, unit = "char", suffix = "..."))
      report_long_strings <- rbind(report_long_strings,
                                   data.frame(varName = single_var,
                                              string = strings_truncated))
    }
  }
  out$r7_longStrings <- report_long_strings

  # too many rows or columns
  row_limit <- getProgramLimit(version, "nrows")
  col_limit <- getProgramLimit(version, "ncols")
  out$r8_surplusRows <- ifelse(nrow(GADSdat$dat) > row_limit$value,
                               yes = nrow(GADSdat$dat) - row_limit$value,
                               no = 0)
  out$r8_surplusCols <- ifelse(ncol(GADSdat$dat) > col_limit$value,
                               yes = ncol(GADSdat$dat) - col_limit$value,
                               no = 0)

  # final verdict
  good_results <- lapply(out, function(result) {
    if (length(result) == 0) {
      return(TRUE)
    }
    if (identical(nrow(result), 0L)) {
      return(TRUE)
    }
    if (length(result) == 1 && result == 0) {
      return(TRUE)
    }
    return(FALSE)
  })
  out$verdict <- all(isTRUE(good_results))
  out <- out[c("verdict", names(out)[names(out) != "verdict"])]
  #attr(out[[1]], "label") <- "overall verdict"


  #attr(out[[7]], "label") <- "5 - long variable labels"
  #attr(out[[8]], "label") <- "5 - long value labels"
  # names(out) <- c("overall verdict",
  #                 "1 - variable names with dots",
  #                 "1 - variable names with special characters",
  #                 "2 - long variable names",
  #                 "3 - labeled fractional values",
  #                 "4 - labeled values that are too large to coerce",
  #                 "5 - long variable labels",
  #                 "5 - long value labels")
  return(out)
}
