####
#############################################################################
#' Inspect differences in a variable.
#'
#' Inspect differences within a single \code{GADSdat} or between two \code{GADSdat} objects for a specific variable.
#'
#' Two \code{GADSdat} objects can be compared using \code{\link{equalGADS}}.
#' If differences in the data for specific variables in the two objects occur,
#' these variables can be further inspected using \code{inspectDifferences}.
#' Differences on meta data-level can be inspected via \code{\link{inspectMetaDifferences}}.
#'
#'@param GADSdat A \code{GADSdat} object.
#'@param varName A character vector of length 1 containing the variable name.
#'@param other_GADSdat A second \code{GADSdat} object. If omitted, it is assumed that both variables are part of the
#'first \code{GADSdat}.
#'@param other_varName A character vector of length 1 containing the other variable name.
#'If omitted, it is assumed that both variables have identical names (as supplied in \code{varName}).
#'@param id A character vector of length 1 containing the unique identifier column of both \code{GADSdat}.
#'
#'@return A list.
#'
#'@examples
#' # create a second GADS with different data
#' pisa2 <- pisa
#' pisa2$dat$age[400:nrow(pisa$dat)] <- sample(pisa2$dat$age[400:nrow(pisa$dat)])
#'
#' # inspect via equalGADS()
#' equalGADS(pisa, pisa2)
#'
#' # inspect via inspectDifferences()
#' inspectDifferences(GADSdat = pisa, varName = "age", other_GADSdat = pisa2, id = "idstud")
#'
#'@export
inspectDifferences <- function(GADSdat, varName, other_GADSdat = GADSdat, other_varName = varName, id) {
  check_GADSdat(GADSdat)
  check_GADSdat(other_GADSdat)
  check_characterArgument(varName, argName = "varName")
  check_characterArgument(other_varName, argName = "other_varName")
  check_characterArgument(id, argName = "id")
  check_vars_in_GADSdat(GADSdat, vars = varName, argName = "varName", GADSdatName = "GADSdat")
  check_vars_in_GADSdat(other_GADSdat, vars = other_varName, argName = "other_varName", GADSdatName = "other_GADSdat")
  check_vars_in_GADSdat(GADSdat, vars = id, argName = "id", GADSdatName = "GADSdat")
  check_vars_in_GADSdat(other_GADSdat, vars = id, argName = "id", GADSdatName = "other_GADSdat")

  if(nrow(GADSdat$dat) != nrow(other_GADSdat$dat)) stop("'GADSdat' and 'other_GADSdat' have different row numbers.")
  if(any(is.na(GADSdat$dat[, id]))) stop("Missing values in 'id' column of 'GADSdat'.")
  if(any(is.na(other_GADSdat$dat[, id]))) stop("Missing values in 'id' column of 'other_GADSdat'.")
  if(any(GADSdat$dat[, id] != other_GADSdat$dat[, id])) stop("'id' column is not equal for 'GADSdat' and 'other_GADSdat'.")

  if(is.numeric(GADSdat$dat[, varName]) && !is.numeric(other_GADSdat$dat[, other_varName])) {
    stop("'varName' column is numeric in 'GADSdat' but 'other_varName' is not numeric in 'other_GADSdat'.")
  }
  if(!is.numeric(GADSdat$dat[, varName]) && is.numeric(other_GADSdat$dat[, other_varName])) {
    stop("'other_varName' column is numeric in 'other_GADSdat' but 'varName' is not numeric in 'GADSdat'.")
  }

  if(isTRUE(all.equal(GADSdat$dat[, varName], other_GADSdat$dat[, other_varName], scale = 1))) {
    return("all.equal")
  }

  unequal_rows <- c(which(other_GADSdat$dat[, other_varName] != GADSdat$dat[, varName]),
                    which(is.na(other_GADSdat$dat[, other_varName]) & !is.na(GADSdat$dat[, varName])),
                    which(!is.na(other_GADSdat$dat[, other_varName]) & is.na(GADSdat$dat[, varName])))
  unequal_case_dat2 <- other_GADSdat$dat[unequal_rows, ]
  unequal_case_dat1 <- GADSdat$dat[unequal_rows, ]

  # naming for cross_table
  nam_dnn <- c(varName, other_varName)
  if(identical(varName, other_varName)) {
    nam_dnn <- c("GADSdat", "other_GADSdat")
  }

  list(cross_table = table(GADSdat$dat[, varName], other_GADSdat$dat[, other_varName], useNA = "if",
                           dnn = nam_dnn),
       unequal_IDs = unequal_case_dat2[, id]
  )
}
