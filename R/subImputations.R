####
#############################################################################
#' Substitute imputed values.
#'
#' Substitute imputed values in a imputed \code{GADSdat_imp} object with original, not imputed values from a \code{GADSdat}.
#'
#'
#'@param GADSdat A \code{GADSdat} object.
#'@param GADSdat_imp A \code{GADSdat} object.
#'@param varName A character vector of length 1 containing the variable name.
#'@param id A character vector of length 1 containing the unique identifier column of both \code{GADSdat}.
#'@param imp A character vector of length 1 containing the imputation number in \code{GADSdat_imp}.
#'
#'@return The modified \code{GADSdat_imp}..
#'
#'@examples
#' # tbd
#'
#'@export
subImputations <- function(GADSdat, GADSdat_imp, varName, id, imp) {
  check_GADSdat(GADSdat)
  check_GADSdat(GADSdat_imp)
  if(!is.character(varName) || length(varName) != 1) stop("'varName' must be a character of length 1.")
  if(!is.character(id) || length(id) != 1) stop("'id' must be a character of length 1.")
  if(!is.character(imp) || length(imp) != 1) stop("'imp' must be a character of length 1.")
  if(!varName %in% namesGADS(GADSdat)) stop("'varName' is not a variable in 'GADSdat'.")
  if(!varName %in% namesGADS(GADSdat_imp)) stop("'varName' is not a variable in 'GADSdat_imp'.")
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

  # maybe improve performance? but slows down!
  #imp_dat <- as.data.table(GADSdat_imp$dat)
  #setkeyv(imp_dat, cols = id)

  count <- 0
  for(single_id in unique_imp_ids) {
    unimp_value <- unimp_dat[unimp_dat[, id] == single_id, varName]
    if(!is.na(unimp_value)) {
      #imp_values <- unique(imp_dat[get(id) == single_id, ][[varName]])
      imp_values <- unique(GADSdat_imp$dat[GADSdat_imp$dat[, id] == single_id, varName])
      if(length(imp_values) != 1 || imp_values != unimp_value) {
        count <- count + 1
        GADSdat_imp$dat[GADSdat_imp$dat[, id] == single_id, varName] <- unimp_value
      }
    }
  }
  message("Values for ", count, " 'id's have been substituted.")

  GADSdat_imp
}
