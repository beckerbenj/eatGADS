

#### Create data.frame for plausability check of numerical variables
#############################################################################
#' Create data.frame for specification of numerical plausibility checks.
#'
#' All numerical variables without value labels in a \code{GADSdat} are selected and a \code{data.frame} is created, which allows the specification
#' of minima and maxima.
#'
#' This function is currently under development.
#'
#'
#'@param GADSdat A \code{GADSdat} object.
#'
#'@return A data.frame with the following variables:
#'\item{variable}{All numerical variables in the \code{GADSdat}}
#'\item{varLabel}{Corresponding variable labels}
#'\item{min}{Minimum value for the specific variable.}
#'\item{max}{Maximum value for the specific variable.}
#'\item{value_new}{Which value should be inserted if values exceed the specified range?}
#'
#'@examples
#' # tbd
#'
#'@export
createNumCheck <- function(GADSdat) {
  UseMethod("createNumCheck")
}

#'@export
createNumCheck.GADSdat <- function(GADSdat) {
  check_GADSdat(GADSdat)

  all_names <- namesGADS(GADSdat)
  all_meta <- extractMeta(GADSdat)
  labeled_names <- unique(all_meta[(is.na(all_meta$missings) | all_meta$missings != "miss") & !is.na(all_meta$value), "varName"])
  txt_names <- all_names[sapply(GADSdat$dat, is.character)]

  num_names <- all_names[!all_names %in% c(labeled_names, txt_names)]
  num_varLabels <- sapply(num_names, function(x) unique(extractMeta(GADSdat, x)[, "varLabel"]))

  data.frame(variable = num_names,
             varLabel = num_varLabels,
             min = NA, max = NA,
             value_new = NA,
             stringsAsFactors = FALSE)
}



#### write inbetween function to obtain info on invalid info
# look at Felix function for comparison
# then create

# probably no specific recoding necessary


#### Apply num check
#############################################################################
#' Apply recodes according to a numerical check data.frame.
#'
#' Applies recodes as specified by a \code{numCheck} \code{data.frame}, as created by \code{\link{createNumCheck}}.
#'
#' This function is currently under development.
#'
#'@param GADSdat A \code{GADSdat} object.
#'@param numCheck A \code{data.frame} as created by \code{\link{createNumCheck}}.
#'
#'@return A recoded \code{GADSdat}.
#'
#'@examples
#' # tbd
#'
#'@export
applyNumCheck <- function(GADSdat, numCheck) {
  UseMethod("applyNumCheck")
}

#'@export
applyNumCheck.GADSdat <- function(GADSdat, numCheck) {
  check_GADSdat(GADSdat)
  check_numCheck(GADSdat, numCheck)

  for(varName in numCheck$variable) {
    new_value <- numCheck[numCheck$variable == varName, "value_new"]
    min_value <- numCheck[numCheck$variable == varName, "min"]
    max_value <- numCheck[numCheck$variable == varName, "max"]

    #browser()

    GADSdat$dat[which(GADSdat$dat[[varName]] < min_value), varName] <- new_value
    GADSdat$dat[which(GADSdat$dat[[varName]] > max_value), varName] <- new_value
  }

  GADSdat
}

check_numCheck <- function(GADSdat, numCheck) {
  if(!is.data.frame(numCheck)) stop("numCheck needs to be a data.frame created by createNumCheck")
  if(!all(numCheck$variable %in% namesGADS(GADSdat))) stop("Not all variables in numCheck are variables in the GADSdat.")
  if(!identical(names(numCheck), c("variable", "varLabel", "min", "max", "value_new"))) stop("numCheck is formatted incorrectly. Check column names.")
  if(is.character(numCheck$min)) stop("Column 'min' containts non numeric values.")
  if(is.character(numCheck$max)) stop("Column 'max' containts non numeric values.")
  if(is.character(numCheck$value_new)) stop("Column 'value_new' containts non numeric values.")

  if(any(duplicated(numCheck$variable))) stop("numCheck containts duplicated rows.")
  return()
}




