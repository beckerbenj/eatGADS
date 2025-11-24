
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
#' to be added
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
                    charLength = numeric(),
                    empty = logical())
  limit_list <- getProgramLimit(program = program,
                                component = "valLabels")
  all_meta <- extractMeta(GADSdat, vars = vars)
  vars_without_valLabels <- NULL

  for (single_var in vars) {
    single_meta <- all_meta[all_meta$varName == single_var,]
    # any valLabels to check?
    if (all(is.na(single_meta$valLabel))) {
      vars_without_valLabels <- c(vars_without_valLabels, single_var)
      next
    }

    # any long labels to report?
    single_var_valLabels <- single_meta$valLabel
    label_lengths <- nchar(single_var_valLabels, type = limit_list$unit)
    long_label_pointer <- which(label_lengths > limit_list$value)
    if (identical(length(long_label_pointer), 0L)) {
      next
    }

    if (!is.null(printLength)) {
      long_labels <- unlist(lapply(single_var_valLabels[long_label_pointer],
                                   truncate_string,
                                   n = printLength, unit = "char", suffix = ""))
    }

    add_out <- single_meta[long_label_pointer, c("varName", "value")]
    add_out$valLabel <- long_labels
    add_out$charLength <- label_lengths[long_label_pointer]
    add_out$empty <- FALSE

    # any empty long labels?
    empty_valLabels <- checkEmptyValLabels(GADSdat, vars = single_var)[[single_var]]$value
    if (!is.null(empty_valLabels) && any(empty_valLabels %in% add_out$value)) {
      add_out[add_out$value %in% empty_valLabels] <- TRUE
    }
    out <- rbind(out, add_out)
  }

  if (!is.null(vars_without_valLabels) && !identical(length(vars), ncol(GADSdat$dat))) {
    message("The following variables do not have any value labels to check: ",
            vars_without_valLabels)
  }
  return(out)
}
