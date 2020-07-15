
#### Shorten multiple text variables
#############################################################################
#' Shorten multiple text variables while giving NA codes.
#'
#' Remove text variables from a certain number from \code{GADSdat} while coding overflowing answers as complete missings.
#'
#' In some cases, multiple text variables contain the information of one variable (e.g. multiple answers to an open item).
#'
#'@param GADSdat A \code{GADSdat} object.
#'@param vars A character vector with the names of the text variables.
#'@param max_num Maximum number of text variables. Additional text variables will be removed and NA codes given accordingly.
#'@param na_value Which NA value should be given in cases of too many values on text variables.
#'
#'@return Returns the modified \code{GADSdat}.
#'
#'@examples
#'#to be done
#'
#'@export
remove_2NA_char <- function(GADSdat, vars, max_num = 2, na_value) {
  UseMethod("remove_2NA_char")
}

#'@export
remove_2NA_char.GADSdat <- function(GADSdat, vars, max_num = 2, na_value) {
  check_GADSdat(GADSdat)
  if(!is.numeric(max_num) && length(max_num) == 1 && max_num > 0) stop("max_num needs to be a single numeric value greater than 0.")

  dat <- max_num_strings2NA(GADSdat$dat, vars = vars, max_num = max_num, na_value = na_value)
  # cut text variables
  remove_vars <- vars[-(1:max_num)]
  dat2 <- dat[, !names(dat) %in% remove_vars, drop = FALSE]

  updateMeta(GADSdat, dat2)
}

# count text variables, give missings if more than x left
max_num_strings2NA <- function(dat, vars, max_num, na_value) {
  #dat[, vars] <- ifelse(!is.na(dat[, max_num]), yes = NA, no = dat[, vars])
  stopifnot(is.numeric(max_num) && length(max_num) == 1)
  stopifnot(is.character(vars) && length(vars) > 1)

  max_var <- vars[max_num + 1]

  for(i in seq(nrow(dat))) {
    if(!is.na(dat[i, max_var])) {
      dat[i, vars] <- na_value
    }
  }
  dat
}
