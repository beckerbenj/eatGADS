
#' Get program specific limits
#'
#' Get the (most restrictive) \link[=program_limits]{limits} that \code{SPSS} and/or \code{Stata}
#'  imposes on a specific aspect of a dataset.
#'
#' For more details about program specific limits as well as a full list, see \link{program_limits}.
#'  In \code{program}, \code{"SPSS"} implies \code{SPSS 30}, and \code{"Stata"} implies
#'  \code{Stata 19/SE}, as these are the most relevant version among the ones implemented here.
#'  If more than one program/version name is given in \code{program}, the most restrictive limit
#'  will be returned.
#'
#' @param program Character vector of the programs/program version that should be considered.
#' @param component Single string. Which limits should be returned?
#'
#' @return A list of two elements: \code{value} (numeric size of the limit) and
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
#' # Get the variable name length limit a dataset has to adhere to to be compatible with
#' #  both SPSS and Stata 19/SE
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
  program_limits <- program_limits[program_limits$program %in% program,]
  program_limits <- program_limits[program_limits$component == component,]
  if (nrow(program_limits) == 0) {
    stop("Unexpected error while collecting program limits: No limits found for component '",
         component, "' and program(s) '", program, "'.",
         call. = FALSE)
  }

  value <- unit <- NULL
  for (i in seq_along(program)) {
    single_program_limit <- program_limits[program_limits$program == program[[i]],]
    new_value <- single_program_limit$value
    new_unit <- single_program_limit$unit
    if (isTRUE(new_value < value) ||
        (unit == "byte" && new_unit == "char") ||
        is.null(unit)) {
      unit <- new_unit
    }
    value <- min(value, new_value)
  }
  out <- list(value = value,
              unit = unit)
  return(out)
}
