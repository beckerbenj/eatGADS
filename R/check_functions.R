check_vars_in_GADSdat <- function(GADSdat, vars, argName = "vars", GADSdatName = "GADSdat") {
  dup_vars <- vars[duplicated(vars)]
  if(length(dup_vars) > 0) stop("There are duplicates in '", argName,"': ",
                                paste(dup_vars, collapse = ", "),
                                call. = FALSE)

  nams <- namesGADS(GADSdat)
  if(is.list(nams)) nams <- unlist(nams)
  other_vars <- vars[!vars %in% nams]
  if(length(other_vars) > 0) stop("The following '", argName,"' are not variables in the ", GADSdatName, ": ",
                                  paste(other_vars, collapse = ", "),
                                  call. = FALSE)
  return()
}

check_vars_in_vector <- function(vec, vars, vec_nam) {
  dup_vars <- vars[duplicated(vars)]
  if(length(dup_vars) > 0) stop("There are duplicates in 'vars': ",
                                paste(dup_vars, collapse = ", "),
                                call. = FALSE)

  other_vars <- vars[!vars %in% vec]
  if(length(other_vars) > 0) stop("The following 'vars' are not variables in the ", vec_nam, ": ",
                                  paste(other_vars, collapse = ", "),
                                  call. = FALSE)
  return()
}

check_logicalArgument <- function(arg, argName) {
  if (missing(argName)) {
    argName <- deparse(substitute(arg))
  }
  if(!is.logical(arg) || length(arg) != 1) {
    stop("'", argName, "' needs to be a logical vector of length 1.",
         call. = FALSE)
  }
  return()
}

check_characterArgument <- function(arg, argName) {
  if (missing(argName)) {
    argName <- deparse(substitute(arg))
  }
  if(!is.character(arg) || length(arg) != 1) {
    stop("'", argName, "' needs to be a character vector of length 1.",
         call. = FALSE)
  }
  return()
}

check_numericArgument <- function(arg, argName) {
  if (missing(argName)) {
    argName <- deparse(substitute(arg))
  }
  if(!is.numeric(arg) || length(arg) != 1) {
    stop("'", argName, "' needs to be a numeric vector of length 1.",
         call. = FALSE)
  }
  return()
}
