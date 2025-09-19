
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
#' Run \code{get_program_limit()} to see all limits that are collected in this function.
#'
#' @references \href{https://www.stata.com/products/comparison-of-limits/}{Stata: Comparison of limits}
#'
#' @param version Optional. Overwrites \code{program} with a specific version of a program (see details).
#' @param program Character vector. Name(s) of the program(s) whose limits should be returned.
#' @param component Optional single string. Which limits should be returned? If this is "all" or
#'  not specified, all limits for the chosen \code{program}(s) are returned.
#'
#' @returns A list of two elements: \code{x} (numeric size of the limit) and
#'  \code{unit} ("char", "byte", or "generic"). If more than one \code{program} is specified and
#'  \code{component} is not "all", the most restrictive limit will be returned.
#'
#' @family dataset compliance checks
#'
#' @examples
#' # Get all limits that Stata imposes on its datasets
#' eatGADS::get_program_limit("Stata", "all")
#'
#' # Get the specific limit on variable name lengths under SPSS
#' eatGADS::get_program_limit("SPSS", "varNames")
get_program_limit <- function(program = c("SPSS", "Stata"),
                              component = c("all",
                                            "varNames", "varLabels", "valLabels", "stringvars",
                                            "nrows", "ncols"),
                              version = NULL) {
  program <- match.arg(program, several.ok = TRUE)
  if (component[[1]] == "all") {
    component <- "all"
  } else {
    component <- match.arg(component, several.ok = FALSE)
  }
  if (!is.null(version)) check_characterArgument(version)

  program_limits <- list(x = matrix(c(64, 256, 120, 32767, 2^31-1, 2^31-1,
                                      32, 80, 32, 2*10^6, 2^31-29, 32767),
                                    ncol = 2, nrow = 6,
                                                    c("SPSS", "Stata"))),
                                    dimnames = list(c("varNames", "varLabels", "valLabels", "stringvars",
                                                      "nrows", "ncols"),
                         unit = matrix(c("byte", "char", "char", "byte", "generic", "generic",
                                         "char", "char", "char", "byte", "generic", "generic"),
                                       ncol = 2, nrow = 6,
                                                       c("SPSS", "Stata"))))
                                       dimnames = list(c("varNames", "varLabels", "valLabels", "stringvars",
                                                         "nrows", "ncols"),
  if (component == "all") {
    filtered_x <- program_limits$x[, program]
    filtered_unit <- program_limits$unit[, program]
    return(list(x = filtered_x,
                unit = filtered_unit))
  }

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
