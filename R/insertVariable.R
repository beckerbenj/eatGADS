####
#############################################################################
#' Reorder a single variable in a \code{GADSdat}.
#'
#' Reorder a single variables in a \code{GADSdat}. The variable (\code{var}) can be inserted right after another variable (\code{varBefore}).
#'
#' The variables in the \code{dat} and in the \code{labels} section are ordered. For reordering the whole \code{GADSdat}, see
#' \code{\link{orderLike}}.
#'
#'@param GADSdat A \code{GADSdat} object.
#'@param var Character string of the variable name which should be sorted.
#'@param varBefore Character string of the variable name which should be followed by \code{var}.
#'
#'@return Returns a \code{GADSdat} object.
#'
#'@examples
#' pisa2 <- insertVariable(pisa, var = "migration", varBefore = "idclass")
#'
#'@export
insertVariable <- function(GADSdat, var, varBefore) {
  UseMethod("insertVariable")
}
#'@export
insertVariable.GADSdat <- function(GADSdat, var, varBefore) {
  check_GADSdat(GADSdat)
  check_single_varName(var)
  check_single_varName(varBefore)
  check_vars_in_GADSdat(GADSdat, vars = c(var, varBefore))

  nams <- namesGADS(GADSdat)
  where_varBefore <- match(varBefore, nams)
  where_var <- match(var, nams)
  newOrder_n <- c(setdiff(seq(where_varBefore), where_var),
                   where_var)
  if(length(newOrder_n) < length(nams)) newOrder_n <- c(newOrder_n, setdiff(seq(from = where_varBefore + 1, to = length(nams)), where_var))
  #browser()
  # problem is where_varBefore is last variable
  newOrder <- nams[newOrder_n]
  orderLike(GADSdat, newOrder = newOrder)
}
