
#### Collapse multiple recodings
#############################################################################
#' Collapse two columns of a lookup table.
#'
#' Collapse two columns or format a single column of a lookup table created by \code{\link{createLookup}}.
#'
#' If a lookup table is created by \code{\link{createLookup}}, different recoding columns can be specified by the \code{addCols} argument.
#' This might be the case if two rater suggest recodes or one rater corrects recodes by another rater in a separate column.
#' After the recoding columns have been filled out, \code{collapseColumns} can be used to either:
#'
#' (a) Collapse two recoding columns into one recoding column. This might be desirable, if the two columns contain missing values.
#' \code{prioritize} can be used to specify, which of the two columns should be prioritized if both columns contain valid values.
#'
#' (b) Format the lookup table for \code{\link{applyLookup}}, if \code{recodeVars} is a single variable.
#' This simply renames the single variable specified under \code{recodeVars}.
#'
#'@param lookup For example a lookup table \code{data.frame} as created via \code{\link{createLookup}}.
#'@param recodeVars Character vector of variable names which should be collapsed (currently only up to two variables are supported).
#'@param prioritize Character vector of length 1. Which of the variables in \code{recodeVars} should be prioritized,
#'if multiple values are available? If \code{recodeVars} is of length 1, this argument can be omitted.
#'
#'@return Returns a \code{data.frame} that can be used for \code{\link{applyLookup}}, with the columns:
#'\item{variable}{Variable names}
#'\item{value}{Old values}
#'\item{value_new}{New values. Renamed and/or collapsed column.}
#'
#'@examples
#' ## (a) Collapse two columns
#' # create example recode data.frame
#' lookup_raw <- data.frame(variable = c("var1"), value = c("germa", "German", "dscherman"),
#'            recode1 = c(NA, "English", "German"),
#'            recode2 = c("German", "German", NA), stringsAsFactors = FALSE)
#'
#' # collapse columns
#' lookup <- collapseColumns(lookup_raw, recodeVars = c("recode1", "recode2"), prioritize = "recode2")
#'
#' ## (b) Format one column
#' # create example recode data.frame
#' lookup_raw2 <- data.frame(variable = c("var1"), value = c("germa", "German", "dscherman"),
#'            recode1 = c("German", "German", "German"), stringsAsFactors = FALSE)
#'
#' # collapse columns
#' lookup2 <- collapseColumns(lookup_raw2, recodeVars = c("recode1"))
#'
#'@export
collapseColumns <- function(lookup, recodeVars, prioritize) {
  if(!is.data.frame(lookup)) stop("'lookup' must be a data.frame.")
  if(!length(recodeVars) %in% 1:2) stop("More recode variables than 2 are currently not supported.")
  if(!all(recodeVars %in% names(lookup))) stop("All variables names in 'recodeVars' need to be variables in 'lookup'.")
  if(any(sapply(lookup[, recodeVars], is.factor))) stop("Variables specified under 'recodeVars' must not be factors.")

  if(length(recodeVars) == 1){
    names(lookup)[names(lookup) == recodeVars] <- "value_new"
    return(lookup)
  }

  if(length(prioritize) != 1) stop("'prioritize' must be of length 1.")
  if(!all(prioritize %in% recodeVars)) stop("All variables names in 'prioritize' need to be in 'recodeVars'.")

  lookup[, "value_new"] <- ifelse(is.na(lookup[[prioritize]]),
                                  yes = lookup[[recodeVars[!recodeVars %in% prioritize]]],
                                  no = lookup[[prioritize]])
  lookup[, names(lookup)[!names(lookup) %in% recodeVars]]
}
