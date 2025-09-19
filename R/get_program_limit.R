
#' @title Get program specific limits
#'
#' @description
#' Get different limits for datasets specific to \code{SPSS} and \code{Stata}. This function
#'  primarily serves as a helper to the (sub)functions of the dataset compliance check family.
#'
#' @details
#' \code{SPSS} and \code{Stata} impose different limits to their data sets.
#'
#' @references \href{https://www.stata.com/products/comparison-of-limits/}{Stata: Comparison of limits}
#'
#' @param program Character. Name(s) of the program(s) whose limits should be returned (see details).
#' @param component Single string. Which limits should be returned?
#' @param version Optional. Overwrites \code{program} with a specific version of a program (see details).
#'
#' @returns A list of two elements: \code{x} (numeric size of the limit) and
#'  \code{unit} ("char", "byte", or "generic")
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
  out <- list(x = program_limits$x[component, program],
              unit = program_limits$unit[component, program])
  return(out)
}
