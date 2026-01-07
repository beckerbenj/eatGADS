
#### Check for Stata export
#############################################################################
#' Check a \code{GADSdat} for compatibility with \code{Stata}.
#'
#' This function performs all relevant checks to assess if a \code{GADSdat} complies with all of
#'  Stata's dataset requirements. Run this before exporting a dataset as \code{.dta}, using
#'  \link{write_stata}.
#'
#' Specifically, the following requirements are tested:
#' \tabular{rll}{
#'  dots_in_varNames + special_chars_in_varNames \tab - \tab Variable names do not contain dots or special characters. \cr
#'  varName_length \tab - \tab Variable names are not longer than the specific limit (\link{checkVarNames}). \cr
#'  labeled_fractionals \tab - \tab There are no labeled fractional values like \code{99.9} (\link{checkLabeledFractionals}). \cr
#'  large_integers \tab - \tab All labeled values can be coerced \code{as.integer} (\link{checkIntOverflow}). \cr
#'  varLabel_length + valLabel_length \tab - \tab Variable labels and value labels are not longer than the specific limits (\link{checkVarLabels}; \link{checkValLabels}). \cr
#'  long_strings \tab - \tab String variables do not contain string values that are longer than the specific limit. \cr
#'  too_many_rows + too_many_cols \tab - \tab The numbers of rows/observations and columns/variables do not exceed the specific limits. \cr
#' }
#' Limits to different aspects of the dataset differ between the versions of the software. By
#'  default (\code{version = "Stata"}), compliance with the limits for \code{Stata 19/SE} is
#'  checked. Checks against the limits for \code{Stata 19/BE} or \code{Stata 19/MP} can be
#'  requested by specifying \code{version} with the corresponding string.
#'  For more details, see \link{program_limits}.
#'
#' @param GADSdat A \code{GADSdat} object.
#' @param version Optional single string to request checks for a specific Stata version
#'  (see details).
#'
#' @returns Either \code{NULL} if all checks are passed successfully, or a \code{list} of all
#'  check results (see details for explanations for the keywords) if any problem was detected,
#'  including a verdict whether any of the problems is critical (\code{"hard issue"}) or will
#'  be solved automatically in the export process by truncating (\code{"soft issue"}).
#'
#' @family dataset compliance checks
#'
#' @examples
#' check4Stata(pisa)
#'
#' @export
check4Stata <- function(GADSdat, version = c("Stata", "Stata 19/BE", "Stata 19/MP")) {
  check_GADSdat(GADSdat)
  version <- match.arg(version, several.ok = TRUE)[[1]]

  out <- list()

  # varNames with dots or special characters -> show-stopping
  other_gads <- suppressMessages(checkVarNames(GADSdat = GADSdat,
                                               checkKeywords = FALSE,
                                               checkDots = TRUE,
                                               charLimits = NULL))
  names_fixEncoding <- suppressMessages(fixEncoding(namesGADS(GADSdat)))
  out$dots_in_varNames <- namesGADS(GADSdat)[!namesGADS(GADSdat) %in% namesGADS(other_gads)]
  out$special_chars_in_varNames <- namesGADS(GADSdat)[!namesGADS(GADSdat) %in% names_fixEncoding]

  # varNames too long -> show-stopping
  names_truncated <- suppressMessages(checkVarNames(GADSdat = namesGADS(GADSdat),
                                                    checkKeywords = FALSE,
                                                    checkDots = FALSE,
                                                    charLimits = "Stata"))
  out$varName_length <- namesGADS(GADSdat)[!namesGADS(GADSdat) %in% names_truncated]

  # labeled fractional values -> show-stopping
  out$labeled_fractionals <- checkLabeledFractionals(GADSdat = GADSdat)

  # too large labeled values -> show-stopping
  out$large_integers <- checkIntOverflow(GADSdat = GADSdat)

  # varLabels or valLabels too long -> will be truncated
  out$varLabel_length <- suppressMessages(checkVarLabels(GADSdat = GADSdat,
                                                         charLimits = "Stata"))
  out$valLabel_length <- suppressMessages(checkValLabels(GADSdat = GADSdat,
                                                         charLimits = "Stata"))

  # labeled strings -> show-stopping
  char_vars <- namesGADS(GADSdat)[give_GADSdat_classes(GADSdat) == "character"]
  ## deactivated because check_GADSdat() already blocks labeled (true) strings ##

  # long strings -> exportable but not usable
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
  out$long_strings <- report_long_strings

  # too many rows or columns -> show-stopping
  row_limit <- getProgramLimit(version, "nrows")
  col_limit <- getProgramLimit(version, "ncols")
  out$too_many_rows <- ifelse(nrow(GADSdat$dat) > row_limit$value,
                              yes = nrow(GADSdat$dat) - row_limit$value,
                              no = 0)
  out$too_many_cols <- ifelse(ncol(GADSdat$dat) > col_limit$value,
                              yes = ncol(GADSdat$dat) - col_limit$value,
                              no = 0)

  # final verdict
  req_hard <- c("dots_in_varNames",
                "special_chars_in_varNames",
                "varName_length",
                "labeled_fractionals",
                "large_integers",
                "too_many_rows",
                "too_many_cols")
  req_soft <- c("varLabel_length",
                "valLabel_length",
                "long_strings")
  res_hard <- lapply(out[req_hard], function(result) {
    if (length(result) == 0 |
        (is.data.frame(result) && nrow(result) == 0) |
        (length(result) == 1 && result == 0)) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  })
  res_soft <- lapply(out[req_soft], function(result) {
    if (length(result) == 0 |
        (is.data.frame(result) && nrow(result) == 0) |
        (length(result) == 1 && result == 0)) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  })

  if (isTRUE(all(unlist(res_hard))) && isTRUE(all(unlist(res_soft)))) {
    return(NULL)
  }
  if (isTRUE(all(unlist(res_hard))) && !isTRUE(all(unlist(res_soft)))) {
    out$verdict <- "soft issue"
  } else {
    out$verdict <- "hard issue"
  }
  out <- out[c("verdict", names(out)[names(out) != "verdict"])]
  return(out)
}
