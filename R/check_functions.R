check_single_varName <- function(var, argumentName = "varName") {
  if(!is.character(var)) stop("'", argumentName, "' is not a character vector.")
  if(!length(var) == 1) stop("'", argumentName, "' must be of length 1.")
}


check_vars_in_GADSdat <- function(GADSdat, vars, argName = "vars") {
  dup_vars <- vars[duplicated(vars)]
  if(length(dup_vars) > 0) stop("There are duplicates in '", argName,"': ",
                                paste(dup_vars, collapse = ", "))

  other_vars <- vars[!vars %in% namesGADS(GADSdat)]
  if(length(other_vars) > 0) stop("The following '", argName,"' are not variables in the GADSdat: ",
                                  paste(other_vars, collapse = ", "))
  return()
}

check_vars_in_vector <- function(vec, vars, vec_nam) {
  dup_vars <- vars[duplicated(vars)]
  if(length(dup_vars) > 0) stop("There are duplicates in 'vars': ",
                                paste(dup_vars, collapse = ", "))

  other_vars <- vars[!vars %in% vec]
  if(length(other_vars) > 0) stop("The following 'vars' are not variables in the ", vec_nam, ": ",
                                  paste(other_vars, collapse = ", "))
  return()
}

check_logicalArgument <- function(arg, argName) {
  if(!is.logical(arg) || length(arg) != 1) {
    stop("'", argName, "' needs to be a logical vector of length 1.")
  }
  return()
}

check_characterArgument <- function(arg, argName) {
  if(!is.character(arg) || length(arg) != 1) {
    stop("'", argName, "' needs to be a character vector of length 1.")
  }
  return()
}

check_numericArgument <- function(arg, argName) {
  if(!is.numeric(arg) || length(arg) != 1) {
    stop("'", argName, "' needs to be a numeric vector of length 1.")
  }
  return()
}
