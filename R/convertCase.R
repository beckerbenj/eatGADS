####
#############################################################################
#' Modify upper and lower case for strings.
#'
#' Convert a character vector, all character variables in a \code{data.frame} or selected variables in a \code{GADSdat} to
#' upper, lower or first letter upper and everything else lower case.
#'
#'@param x A character vector, \code{data.frame}, or \code{GADSdat}.
#'@param case What case should the strings be converted to?
#'@param ...	further arguments passed to or from other methods.
#'@param vars What variables in the GADSdat should the conversion be applied to?
#'
#'@return Returns the converted object.
#'
#'@examples
#' # for character
#' convertCase(c("Hi", "HEllo", "greaT"), case = "upperFirst")
#'
#' # for GADSdat
#' input_g <- import_DF(data.frame(v1 = 1:3, v2 = c("Hi", "HEllo", "greaT"),
#'                           stringsAsFactors = FALSE))
#' convertCase(input_g, case = "upperFirst", vars = "v2")
#'
#'
#'@export
convertCase <- function(x, case = c("lower", "upper", "upperFirst"), ...){
  UseMethod("convertCase")
}

#'@export
convertCase.character <- function(x, case = c("lower", "upper", "upperFirst"), ...){
  case <- match.arg(case)
  if(identical(case, "lower")) return(tolower(x))
  if(identical(case, "upper")) return(toupper(x))
  if(identical(case, "upperFirst")) {
    x2 <- tolower(x)
    substr(x2, 1, 1) <- toupper(substr(x2, 1, 1))
    return(x2)
  }
}

#'@export
convertCase.data.frame <- function(x, case = c("lower", "upper", "upperFirst"), ...){
  # only transform character variables
  which_character <- which(sapply(x, is.character))

  for(i in which_character) {
    x[[i]] <- convertCase(x[[i]])
  }
  x
}

#' @describeIn convertCase convert case for \code{GADSdats}
#'@export
convertCase.GADSdat <- function(x, case = c("lower", "upper", "upperFirst"), vars, ...){
  check_GADSdat(x)
  if(!is.character(vars) && length(vars) > 0) stop("vars needs to be a character vector of at least length 1.")
  check_vars_in_GADSdat(x, vars = vars)

  for(var_nam in vars) {
    if(!is.character(x$dat[[var_nam]])) stop(var_nam, " is not a character variable and can not be case converted.")
    x$dat[[var_nam]] <- convertCase(x$dat[[var_nam]], case = case)
  }

  x
}
