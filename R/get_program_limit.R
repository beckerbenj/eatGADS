
#' @title Get program specific limits
#'
#' @description
#' Get different limits for datasets specific to \code{SPSS} and \code{Stata}. This function
#'  primarily serves as a helper to the (sub)functions of the dataset compliance check family.
#'
#' @details
#' \code{SPSS} and \code{Stata} impose different limits to different aspects of their data sets:
#' \itemize{
#'  \item \code{varNames}: length of variable names
#'  \item \code{varLabels}: length of variable labels
#'  \item \code{valLabels}: length of value labels
#'  \item \code{stringvars}: length of strings in character variables
#'  \item \code{nrows}: number of observations
#'  \item \code{ncols}: number of variables
#' }
#'
#' @references \href{https://www.stata.com/products/comparison-of-limits/}{Stata: Comparison of limits}
#'
#' @param component Single string. Which limits should be returned?
#' @param version Optional. Overwrites \code{program} with a specific version of a program (see details).
#' @param program Character vector. Name(s) of the program(s) whose limits should be returned.
#'
#' @returns A list of two elements: \code{x} (numeric size of the limit) and
#'  \code{unit} ("char", "byte", or "generic"). If more than one \code{program} is specified,
#'  the most restrictive limit will be returned.
#'
#' @family dataset compliance checks
#'
#' @examples
#' get_program_limit("SPSS", "varname")
get_program_limit <- function(program = c("SPSS", "Stata"),
                              component = c("varname", "varlabel", "vallabel", "stringvar",
                                            "rows", "cols"),
                              version = NULL) {
  program <- match.arg(program, several.ok = TRUE)
  check_characterArgument(component)
  if (!is.null(version)) check_characterArgument(version)

  program_limits <- list(x = matrix(c(64, 256, 120, 32767, 2^31-1, 2^31-1,
                                      32, 80, 32, 2*10^6, 2^31-29, 32767),
                                    ncol = 2, nrow = 6,
                                    dimnames = list(c("varname", "varlabel", "vallabel", "stringvar",
                                                      "rows", "cols"),
                                                    c("SPSS", "Stata"))),
                         unit = matrix(c("byte", "char", "char", "byte", "generic", "generic",
                                         "char", "char", "char", "byte", "generic", "generic"),
                                       ncol = 2, nrow = 6,
                                       dimnames = list(c("varname", "varlabel", "vallabel", "stringvar",
                                                         "rows", "cols"),
                                                       c("SPSS", "Stata"))))
  x <- unit <- NULL
  for (i in seq_along(program)) {
    newx <- program_limits$x[component, program[[i]]]
    newunit <- program_limits$unit[component, program[[i]]]
    if (isTRUE(newx < x) ||
        (unit == "byte" && newunit == "char") ||
        is.null(unit)) {
      unit <- newunit
    }
    x <- min(x, newx)
  }
  out <- list(x = x,
              unit = unit)
  return(out)
}
