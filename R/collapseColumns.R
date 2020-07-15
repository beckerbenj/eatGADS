
#### Collapse multiple recodings
#############################################################################
#' Collapse columns.
#'
#' Collapse columns of a lookup table created by \code{\link{createLookup}}.
#'
#' This function can for example be used to collapse multiple recoding variables from which one should be preferred if values for
#' both recoding variables are present.
#'
#'@param lookup For example a lookup table \code{data.frame} as created via \code{\link{createLookup}}.
#'@param recodeVars Character vector of variable names which should be collapsed.
#'@param prioritize Which of the variables should be prioritized, if multiple values are available?
#'
#'@return Returns a data frame that can be used for \code{\link{applyLookup}}.
#'
#'@examples
#' # create example recode data.frame
#' lookup_raw <- data.frame(variable = c("var1"), value = c("germa", "German", "dscherman"),
#'            recode1 = c(NA, "Englisch", "German"),
#'            recode2 = c("German", "German", NA))
#'
#' # collapse columns
#' lookup <- collapseColumns(lookup_raw, recodeVars = c("recode1", "recode2"), prioritize = "recode2")
#'
#'@export
collapseColumns <- function(lookup, recodeVars, prioritize) {
  if(!length(recodeVars) %in% 1:2) stop("More recode variables than 2 are currently not supported.")
  if(length(prioritize) != 1) stop("Prioritize must be of length = length(recodeVars) - 1.")
  if(!all(recodeVars %in% names(lookup))) stop("All variables names in recodeVars need to be variables in lookup.")
  if(!all(prioritize %in% recodeVars)) stop("All variables names in prioritize need to be in recodeVars.")

  if(length(recodeVars) == 1){
    names(lookup)[names(lookup) == recodeVars] <- "value_new"
    return(lookup)
  }
  lookup[, "value_new"] <- ifelse(is.na(lookup[[prioritize]]),
                                  yes = lookup[[recodeVars[!recodeVars %in% prioritize]]],
                                  no = lookup[[prioritize]])
  lookup[, names(lookup)[!names(lookup) %in% recodeVars]]
}
