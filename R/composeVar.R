
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
#'@param GADSdat \code{GADSdat} or \code{all_GADSdat} object imported via eatGADS.
#'@param sourceVars Character vector of length two containing the variable names which represent the sources of information.
#'@param primarySourceVar Character vector containing a single variable name. Which of the \code{sourceVars} should be preferred?
#'@param newVar Character vector containing the name of the new composite variable.
#'@param checkVarName Logical. Should \code{newVar} be checked by \code{\link{checkVarNames}}?
#'
#'@return The modified \code{GADSdat}.
#'
#'@examples
#'# example data
#' dat <- data.frame(ID = 1:4,
#' nat_lang_child = c("Engl", "Ger", "missing", "missing"),
#' nat_lang_father = c("Engl", "Engl", "Ger", "missing"),
#' stringsAsFactors = TRUE)
#' gads <- import_DF(dat)
#' changeMissings(gads, "nat_lang_child", value = 3, missings = "miss")
#' changeMissings(gads, "nat_lang_father", value = 3, missings = "miss")
#'
#'# compose variable
#' composeVar(gads, sourceVars = c("nat_lang_child", "nat_lang_father"),
#'            primarySourceVar = "nat_lang_child", newVar = "nat_lang_comp")
#'
#'
#'@export
composeVar <- function(GADSdat, sourceVars, primarySourceVar, newVar, checkVarName = TRUE) {
  UseMethod("composeVar")
}
#'@export
composeVar.GADSdat <- function(GADSdat, sourceVars, primarySourceVar, newVar, checkVarName = TRUE) {
  check_GADSdat(GADSdat)
  check_vars_in_GADSdat(GADSdat, vars = sourceVars)
  check_logicalArgument(checkVarName, argName = "checkVarName")
  if(!is.character(sourceVars) || length(sourceVars) != 2) {
    stop("'sourceVars' must be a character vector of length 2.")
  }
  check_characterArgument(primarySourceVar)
  check_characterArgument(newVar)
  if(!primarySourceVar %in% sourceVars) {
    stop("'primarySourceVar' must be a name in 'sourceVars'.")
  }
  if(checkVarName) {
    newVar <- checkVarNames(newVar)
  }

  otherSourceVar <- sourceVars[sourceVars != primarySourceVar]
  suppressWarnings(extracted_dat <- extractData(GADSdat))
  dat_vec1 <- extracted_dat[[primarySourceVar]]
  dat_vec2 <- extracted_dat[[otherSourceVar]]

  # compare meta (otherwise error?)
  compare_meta_value_level(GADSdat, varName1 = primarySourceVar, varName2 = otherSourceVar, argumentName = "'sourceVars'")

  # compose (if primary not missing or both missing => primary)
  comp_var <- ifelse(is.na(dat_vec1), yes = ifelse(is.na(dat_vec2),
                                                  yes = GADSdat$dat[[primarySourceVar]],
                                                  no = GADSdat$dat[[otherSourceVar]]),
                                       no = GADSdat$dat[[primarySourceVar]])

  ## put into GADS, add meta
  dat_out <- data.frame(GADSdat$dat, comp_var, stringsAsFactors = FALSE)
  names(dat_out)[ncol(dat_out)] <- newVar
  suppressMessages(GADSdat_out <- updateMeta(GADSdat, newDat = dat_out, checkVarNames = FALSE))
  GADSdat_out2 <- reuseMeta(GADSdat_out, varName = newVar,
                            other_GADSdat = GADSdat_out, other_varName = primarySourceVar, addValueLabels = TRUE)

  # sort
  index_primarySource <- which(namesGADS(GADSdat) == primarySourceVar)
  new_order <- order(c(seq(ncol(GADSdat$dat)), index_primarySource - 0.5))
  new_order_names <- namesGADS(GADSdat_out2)[new_order]
  GADSdat_out3 <- orderLike(GADSdat_out2, new_order_names)

  GADSdat_out3
}

compare_meta_value_level <- function(GADSdat, varName1, varName2, argumentName) {
  primary_meta <- extractMeta(GADSdat, varName1)[, c("value", "valLabel", "missings")]
  other_meta <- extractMeta(GADSdat, varName2)[, c("value", "valLabel", "missings")]
  row.names(primary_meta) <- row.names(other_meta) <- NULL
  if(!identical(TRUE, all.equal(primary_meta, other_meta))) stop("Meta data on value level ('value', 'valLabel', 'missings') of ", argumentName, " must be identical.")
  return(NULL)
}



