
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
#'@param GADSdat A \code{GADSdat} object.
#'@param charLimits Character vector of the program(s) against whose limit(s) the labels should
#' be checked.
#'@param vars Optional character vector of the variables whose value labels should be checked.
#' By default, all value labels will be checked.
#'
#'@return Returns a \code{data.frame}, listing every \code{valLabel} that exceeds the limit and its
#' character length, along with the \code{varName}, \code{value}, and whether the value the label
#' is attached to actually occurs in the data (\code{empty}).
#'
#'@examples
#' to be added
#'
#'@export
checkValLabels <- function(GADSdat, charLimits = c("SPSS", "Stata"), vars = namesGADS(GADSdat)) {
  check_GADSdat(GADSdat)
  program <- match.arg(charLimits, several.ok = TRUE)
  check_vars_in_GADSdat(GADSdat, vars = vars)

  out <- data.frame(varName = character(),
                    value = numeric(),
                    valLabel = character(),
                    charLength = numeric(),
                    empty = logical())
}
