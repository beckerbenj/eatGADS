
#### Check Value Labels
#############################################################################
#' Check value labels for length limits.
#'
#' Check if the value labels of a \code{GADSdat} comply with the length limits imposed by
#'  \code{SPSS} or \code{Stata}.
#'
#' If more than one program name is given in \code{charLimits}, the most restrictive limit will be
#'  applied. For details about program specific limits, see \link{program_limits}.
#'
#' Please note that setting \code{printLength} to \code{NULL} (and thereby deactivating label
#'  truncation) might not actually result in the printing of the full length of the exceedingly
#'  long labels if you are using RStudio. The program's own limits to how many characters are
#'  printed to the console may still apply
#'  (see \href{https://stackoverflow.com/questions/36800475/avoid-string-printed-to-console-getting-truncated-in-rstudio}{Stack Overflow}).
#'
#'@param GADSdat A \code{GADSdat} object.
#'@param charLimits Character vector of the program(s) against whose limit(s) the labels should
#' be checked.
#'@param vars Optional character vector of the variables whose value labels should be checked.
#' By default, all value labels will be checked.
#'@param printLength Single numeric value. The first n = \code{printLength} characters will be
#' reported. Alternatively, specify \code{NULL} to deactivate the truncation and have the function
#' report the full labels (not recommended; see details).
#'
#'@return Returns a \code{data.frame} with every (truncated) long \code{valLabel} along with the
#' \code{varName} and \code{value}, with which it occurs, its character length (\code{charLength}),
#' and whether the value the label is attached to actually occurs in the data (\code{empty}).
#'
#'@examples
#' pisa2 <- pisa
#' pisa2$labels[4, "valLabel"] <- paste0(rep("abcdefg", 4300), collapse = "")
#' checkValLabels(pisa2)
#'
#'@export
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
    warning("None of the selected vars have any labeled values. No value labels were checked.")
    return(out)
  }

  label_meta <- all_meta[!is.na(all_meta$valLabel),
                         c("varName", "value", "valLabel", "missings")]
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
