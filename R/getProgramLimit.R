
#' @title Get program specific limits
#'
#' @description
#' Get the (most restrictive) limit that \code{SPSS} and/or \code{Stata} imposes on a specific
#'  aspect of a data set.
#'
#' @details
#' \code{SPSS} and \code{Stata} impose different limits to different components of their data sets,
#'  e.g., on the length of variable names. For details and a list of relevant limits, see \link{program_limits}.
#'  Additionally, limits may vary between software versions. This primarily applies to \code{Stata}'s
#'  product tiers, but also for (very) old \code{SPSS} version.
#'
#' For \code{program}, \code{"SPSS"} and \code{"Stata"} imply \code{SPSS 30} and \code{Stata 19/SE},
#'  respectively, as these are the most relevant version among the ones implemented here.
#'
#' If more than one program/version name is given in \code{program}, the most restrictive limit will
#'  be returned.
#'
#' @param program Character vector of the programs/program version that should be considered.
#' @param component Single string. Which limits should be returned?
#'
#' @returns A list of two elements: \code{x} (numeric size of the limit) and
#'  \code{unit} ("char", "byte", or "generic").
#'
#' @family dataset compliance checks
#'
#' @examples
#' # Show all implemented limits
#' program_limits
#'
#' # Get the specific limit on variable name lengths under SPSS
#' getProgramLimit("SPSS", "varNames")
#'
#' # Get the variable name length limit a data set has to adhere to to be compatible with both SPSS and Stata 19/SE
#' getProgramLimit(c("Stata", "SPSS"), "varNames")
#' @export
getProgramLimit <- function(program = c("SPSS", "Stata", "Stata 19/BE", "Stata 19/MP"),
                            component = c("varNames", "varLabels", "valLabels", "stringvars",
                                          "nrows", "ncols")) {
  if (any(!program %in% eval(formals()$program))) {
    warning(program[!program %in% eval(formals()$program)], " is not a valid program/version and will be omited.")
  }
  program <- match.arg(program, several.ok = TRUE)
  component <- match.arg(component, several.ok = FALSE)

  # program_limits is saved as its own object in data/program_limits.rda

  program_limits <- program_limits

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
