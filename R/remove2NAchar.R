
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
remove2NAchar <- function(GADSdat, vars, max_num = 2, na_value, na_label) {
  UseMethod("remove2NAchar")
}

#'@export
remove2NAchar.GADSdat <- function(GADSdat, vars, max_num = 2, na_value, na_label) {
  check_GADSdat(GADSdat)
  if(!is.numeric(max_num) || length(max_num) != 1 && max_num > 0) stop("'max_num' needs to be a single numeric value greater than 0.")
  if(!is.numeric(na_value) || length(na_value) != 1) stop("'na_value' needs to be a single numeric value.")
  if(!is.character(na_label) || length(na_label) != 1) stop("'na_label' needs to be a single character value.")

  dat <- max_num_strings2NA(GADSdat$dat, vars = vars, max_num = max_num, na_value = na_value)
  # cut text variables
  remove_vars <- vars[-(1:max_num)]
  dat2 <- dat[, !names(dat) %in% remove_vars, drop = FALSE]

  ## modify meta deta (maybe make this to and addValueLabel function?)
  GADSdat_out <- updateMeta(GADSdat, dat2)
  for(i in vars[!vars %in% remove_vars]) {
    GADSdat_out <- changeValLabels(GADSdat_out, varName = i, value = na_value, valLabel = na_label)
    GADSdat_out <- changeMissings(GADSdat_out, varName = i, value = na_value, missings = "miss")
  }
  GADSdat_out
}

# count text variables, give missings if more than x left
max_num_strings2NA <- function(dat, vars, max_num, na_value) {
  #dat[, vars] <- ifelse(!is.na(dat[, max_num]), yes = NA, no = dat[, vars])
  stopifnot(is.numeric(max_num) && length(max_num) == 1)
  stopifnot(is.character(vars) && length(vars) > 1)

  max_var <- vars[max_num + 1]
  if(max_num >= length(vars)) return(dat)

  for(i in seq(nrow(dat))) {
    if(!is.na(dat[i, max_var])) {
      dat[i, vars] <- na_value
    }
  }
  dat
}
