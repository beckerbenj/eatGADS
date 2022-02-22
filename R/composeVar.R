
####
#############################################################################
#' Create a composite variable.
#'
#' Create a composite variable out of two variables.
#'
#' A common use case for creating a composite variable is if there are multiple sources for the same information.
#' For example, a child and the parents are asked about the child's native language. In such cases a composite variable
#' contains information from both variables, meaning that one source is preferred and the other source is used
#' to substitute missing values.
#'
#'
#'@param GADSdat For example a lookup table \code{data.frame} as created via \code{\link{createLookup}}.
#'@param sourceVars Character vector of variable names which represent the sources of information.
#'@param primarySourceVar Character vector containing a single variable name. Which of the \code{sourceVars} should be preferred?
#'@param newVar Character vector containing the name of the new composite variable.
#'
#'@return The modified \code{GADSdat}.
#'
#'@examples
#' #tbd
#'
#'@export
composeVar <- function(GADSdat, sourceVars, primarySourceVar, newVar) {
  check_GADSdat(GADSdat)
  check_vars_in_GADSdat(GADSdat, vars = sourceVars)
  if(!is.character(sourceVars) || length(sourceVars) != 2) stop("'sourceVars' must be a character vector of length 2.")
  if(!is.character(primarySourceVar) || length(primarySourceVar) != 1) stop("'primarySourceVar' must be a character vector of length 1.")
  if(!is.character(primarySourceVar) || length(newVar) != 1) stop("'newVar' must be a character vector of length 1.")
  if(!primarySourceVar %in% sourceVars) stop("'primarySourceVar' must be a name in 'sourceVars'.")

  # compare meta (otherwise error?)

  # compose
  otherSourceVar <- sourceVars[sourceVars != primarySourceVar]
  dat_vec <- extractData(GADSdat)[[primarySourceVar]]
  comp_var <- ifelse(is.na(dat_vec), yes = GADSdat$dat[[otherSourceVar]],
                                       no = GADSdat$dat[[primarySourceVar]])

  ## put into GADS, add meta
  dat_out <- data.frame(GADSdat$dat, comp_var, stringsAsFactors = FALSE)
  names(dat_out)[ncol(dat_out)] <- newVar
  GADSdat_out <- updateMeta(GADSdat, newDat = dat_out)
  GADSdat_out2 <- reuseMeta(GADSdat_out, varName = newVar, other_GADSdat = GADSdat_out, other_varName = primarySourceVar)

  #browser()
  # sort
  index_primarySource <- which(namesGADS(GADSdat) == primarySourceVar)
  new_order <- order(c(seq(ncol(GADSdat$dat)), index_primarySource - 0.5))
  new_order_names <- namesGADS(GADSdat_out2)[new_order]
  GADSdat_out3 <- orderLike(GADSdat_out2, new_order_names)

  GADSdat_out
}
