####
#############################################################################
#' Fill imputed values.
#'
#' Fill imputed values in a imputed \code{GADSdat_imp} object with original, not imputed values from a \code{GADSdat}.
#'
#' This function only fills in missing values in the imputed variable from the not imputed variable. It provides parts
#' of the functionality of \code{subImputations} but does not check whether values have been mistakenly imputed. However,
#' performance is increased substantially.
#'
#'@param GADSdat A \code{GADSdat} object.
#'@param GADSdat_imp A \code{GADSdat} object.
#'@param varName A character vector of length 1 containing the variable name in \code{GADSdat}.
#'@param varName_imp A character vector of length 1 containing the variable name in \code{GADSdat_imp}.
#'@param id A character vector of length 1 containing the unique identifier column of both \code{GADSdat}.
#'@param imp A character vector of length 1 containing the imputation number in \code{GADSdat_imp}.
#'
#'@return The modified \code{GADSdat_imp}..
#'
#'@examples
#' # tbd
#'
#'@export
fillImputations <- function(GADSdat, GADSdat_imp, varName, varName_imp = varName, id, imp) {
  check_GADSdat(GADSdat)
  check_GADSdat(GADSdat_imp)
  if(!is.character(varName) || length(varName) != 1) stop("'varName' must be a character of length 1.")
  if(!is.character(id) || length(id) != 1) stop("'id' must be a character of length 1.")
  if(!is.character(imp) || length(imp) != 1) stop("'imp' must be a character of length 1.")
  if(!varName %in% namesGADS(GADSdat)) stop("'varName' is not a variable in 'GADSdat'.")
  if(!varName_imp %in% namesGADS(GADSdat_imp)) stop("'varName_imp' is not a variable in 'GADSdat_imp'.")
  if(!id %in% namesGADS(GADSdat)) stop("'id' is not a variable in 'GADSdat'.")
  if(!id %in% namesGADS(GADSdat_imp)) stop("'id' is not a variable in 'GADSdat_imp'.")
  if(!imp %in% namesGADS(GADSdat_imp)) stop("'imp' is not a variable in 'GADSdat_imp'.")

  unique_imp_ids <- unique(GADSdat_imp$dat[, id])
  diff_ids <- unique_imp_ids[!unique_imp_ids %in% GADSdat$dat[, id]]
  if(length(diff_ids) > 0) stop("These 'id' values in 'GADSdat_imp' are not in 'GADSdat': ",
                                paste(diff_ids, collapse = ", "))


  ## compare meta data?
  suppressMessages(unimp_GADS <- extractVars(GADSdat, vars = c(id, varName)))
  suppressMessages(unimp_dat <- extractData(GADSdat = unimp_GADS, convertMiss = TRUE, convertLabels = "numeric"))

  #browser()
  # could be written more efficiently

  # still open: how to deal with missing codes in GADSdat
  # -> not common for codebook to have missing codes in imputed variables! (like grades)
  # -> but isn't it actual information?

  suppressMessages(mini_noimp <- extractVars(GADSdat, vars = varName))
  no_imp_var <- extractData(mini_noimp, convertLabels = "numeric")[[1]]

  for(imp_num in unique(GADSdat_imp$dat[, imp])) {
    single_imp_dat <- GADSdat_imp$dat[GADSdat_imp$dat[, imp] == imp_num, ]
    GADSdat_imp$dat[GADSdat_imp$dat[, imp] == imp_num, varName_imp] <- ifelse(is.na(single_imp_dat[, varName_imp]),
                                                                              yes = no_imp_var,
                                                                              no = single_imp_dat[, varName_imp])
  }


  GADSdat_imp
}
