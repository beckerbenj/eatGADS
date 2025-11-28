
#### Check Value Labels
#############################################################################
#' Check Lengths of Labels.
#'
#' Check if the value or variable labels of a \code{GADSdat} comply with the length limits imposed
#'  by \code{SPSS} or \code{Stata}.
#'
#' If more than one program name is given in \code{charLimits}, the most restrictive limit will be
#'  applied. For details about program-specific limits, see \link{program_limits}.
#'
#' Please note that setting \code{printLength} to \code{NULL} (and thereby deactivating label
#'  truncation) might not actually result in the printing of the full length of the exceedingly
#'  long labels if you are using RStudio. The program's own limits on the number of characters
#'  printed to the console may still apply
#'  (see \href{https://stackoverflow.com/questions/36800475/avoid-string-printed-to-console-getting-truncated-in-rstudio}{Stack Overflow}).
#'
#' @param GADSdat A \code{GADSdat} object.
#' @param charLimits Character vector of the program(s) against whose limit(s) the labels should
#'  be checked.
#' @param vars Optional character vector of the variables whose value labels should be checked.
#'  By default, all value labels will be checked.
#' @param printLength Single numeric value. The first n = \code{printLength} characters of each
#'  long label will be reported. Alternatively, specify \code{NULL} to deactivate the truncation
#'  and have the function report the full labels (not recommended; also see details).
#'
#' @return A \code{data.frame}, reporting every (truncated) long \code{varLabel}/\code{valLabel},
#'  their respective \code{length} in the relevant \code{unit} and the \code{varName} in which
#'  they occur. For \code{checkValLabels}, the labeled \code{value}, as well as whether that value
#'  actually occurs in the data (\code{empty}), is also reported.
#'
#' @family dataset compliance checks
#'
#' @examples
#' # check value labels
#' pisa2 <- pisa
#' pisa2$labels[4, "valLabel"] <- paste0(rep("abcdefg", 4300), collapse = "")
#' checkValLabels(pisa2)
#'
#' @describeIn checkValLabels Check value labels for length limits.
#' @export
checkValLabels <- function(GADSdat, charLimits = c("SPSS", "Stata"),
                           vars = namesGADS(GADSdat), printLength = 40) {
  check_GADSdat(GADSdat)
  program <- match.arg(charLimits, several.ok = TRUE)
  check_vars_in_GADSdat(GADSdat, vars = vars)
  if (!is.null(printLength)) {
    check_numericArgument(printLength)
  }

  out <- data.frame(varName = character(),
                    value = numeric(),
                    valLabel = character(),
                    length = numeric(),
                    unit = character(),
                    empty = logical())
  limit_list <- getProgramLimit(program = program,
                                component = "valLabels")
  all_meta <- extractMeta(GADSdat, vars = vars)
  if (all(is.na(all_meta$valLabel))) {
    warning("None of the selected vars have any labeled values. No checks were performed.")
    return(out)
  }

  label_meta <- all_meta[!is.na(all_meta$valLabel),
                         c("varName", "value", "valLabel")]
  label_meta$length <- nchar(label_meta$valLabel, type = limit_list$unit)
  label_meta$too_long <- label_meta$length > limit_list$value
  if (!any(label_meta$too_long)) {
    return(out)
  }

  column_list <- c("varName", "value", "valLabel", "length")
  out[1:sum(label_meta$too_long), column_list] <- label_meta[label_meta$too_long, column_list]
  out$unit <- limit_list$unit

  for (single_var in unique(out$varName)) {
    empty_valLabels <- checkEmptyValLabels(GADSdat, vars = single_var)[[single_var]]$value
    out[out$varName == single_var, "empty"] <- out[out$varName == single_var, "value"] %in% empty_valLabels
  }

  if (!is.null(printLength)) {
    out$valLabel <- unlist(lapply(out$valLabel,
                                  truncate_string,
                                  n = printLength + 3, unit = "char", suffix = "..."))
  }
  return(out)
}
