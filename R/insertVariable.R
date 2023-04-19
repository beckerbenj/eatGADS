####
#############################################################################
#' Reorder a single variable in a \code{GADSdat}.
#'
#' Reorder a single variables in a \code{GADSdat}. The variable (\code{var}) can be inserted right after another variable (\code{after}) or at the beginning
#'of the \code{GADSdat} via \code{after = NULL}.
#'
#' The variables in the \code{dat} and in the \code{labels} section are ordered. For reordering the whole \code{GADSdat}, see
#' \code{\link{orderLike}}.
#'
#'@param GADSdat A \code{GADSdat} object.
#'@param var Character string of the variable name which should be sorted.
#'@param after Character string of the variable name after which \code{var} should be inserted. If \code{NULL}, \code{var} is inserted at the beginning of the
#'\code{GADSdat}.
#'
#'@return Returns a \code{GADSdat} object.
#'
#'@examples
#' # Insert variable 'migration' after variable 'idclass'
#' pisa2 <- insertVariable(pisa, var = "migration", after = "idclass")
#'
#' # Insert variable 'idclass' at the beginning of the data set
#' pisa2 <- insertVariable(pisa, var = "idclass", after = NULL)
#'@export
insertVariable <- function(GADSdat, var, after = NULL) {
  UseMethod("insertVariable")
}
#'@export
insertVariable.GADSdat <- function(GADSdat, var, after = NULL) {
  check_GADSdat(GADSdat)
  check_single_varName(var)
  check_vars_in_GADSdat(GADSdat, vars = var)
  if(!is.null(after)) {
    check_single_varName(after)
    check_vars_in_GADSdat(GADSdat, vars = after)
  }

  nams <- namesGADS(GADSdat)
  where_after <- match(after, nams)
  if(length(where_after) == 0) where_after <- 0
  where_var <- match(var, nams)

  newOrder_n <- c(setdiff(seq2(from = 1, to = where_after), where_var),
                   where_var)
  if(length(newOrder_n) < length(nams)) newOrder_n <- c(newOrder_n, setdiff(seq(from = where_after + 1, to = length(nams)), where_var))

  newOrder <- nams[newOrder_n]
  orderLike(GADSdat, newOrder = newOrder)
}



seq2 <- function(from, to) {
  if (from > to) {
    integer()
  }
  else {
    seq.int(from, to)
  }
}
