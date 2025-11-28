
#### Check Variable Labels
#############################################################################
#' @describeIn checkValLabels Check variable labels for length limits.
#'
#' @examples
#' # check variable labels
#' pisa2$labels[1, "varLabel"] <- paste0(rep("abcdefg", 12), collapse = "")
#' checkVarLabels(pisa2)
#'
checkVarLabels <- function(GADSdat, charLimits = c("SPSS", "Stata"),
                           vars = namesGADS(GADSdat), printLength = 40) {
  check_GADSdat(GADSdat)
  program <- match.arg(charLimits, several.ok = TRUE)
  check_vars_in_GADSdat(GADSdat, vars = vars)
  if (!is.null(printLength)) {
    check_numericArgument(printLength)
  }

  out <- data.frame(varName = character(),
                    varLabel = character(),
                    length = numeric(),
                    unit = character())
  limit_list <- getProgramLimit(program = program,
                                component = "varLabels")
  all_meta <- extractMeta(GADSdat, vars = vars)
  if (all(is.na(all_meta$varLabel))) {
    warning("None of the selected vars have a variable label. No checks were performed.")
    return(out)
  }

  label_meta <- all_meta[!is.na(all_meta$varLabel),
                         c("varName", "varLabel")]
  label_meta$length <- nchar(label_meta$varLabel, type = limit_list$unit)
  label_meta$too_long <- label_meta$length > limit_list$value
  if (!any(label_meta$too_long)) {
    return(out)
  }

  column_list <- c("varName", "varLabel", "length")
  out[1:sum(label_meta$too_long), column_list] <- label_meta[label_meta$too_long, column_list]
  out$unit <- limit_list$unit

  if (!is.null(printLength)) {
    out$varLabel <- unlist(lapply(out$varLabel,
                                  truncate_string,
                                  n = printLength + 3, unit = "char", suffix = "..."))
  }
  return(out)
}
