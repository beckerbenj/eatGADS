####
#############################################################################
#' Reorder a single variable in a \code{GADSdat}.
#'
#' Reorder a single variable in a \code{GADSdat}. The variable (\code{var}) can be inserted right after another variable (\code{after}) or at the beginning
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
#' pisa2 <- relocateVariable(pisa, var = "migration", after = "idclass")
#'
#' # Insert variable 'idclass' at the beginning of the data set
#' pisa2 <- relocateVariable(pisa, var = "idclass", after = NULL)
#'@export
relocateVariable <- function(GADSdat, var, after = NULL) {
  UseMethod("relocateVariable")
}
#'@export
relocateVariable.GADSdat <- function(GADSdat, var, after = NULL) {
  check_GADSdat(GADSdat)
  check_single_varName(var)
  check_vars_in_GADSdat(GADSdat, vars = var)

  nams <- namesGADS(GADSdat)
  where_var <- match(var, nams)

  if(!is.null(after)) {
    check_single_varName(after)
    check_vars_in_GADSdat(GADSdat, vars = after)
    where_after <- match(after, nams)
  } else {
    where_after <- 0
  }

  first_split <- nams[seq2(from = 1, to = where_after)]
  first_split <- first_split[which(var != first_split)]

  second_split <- nams[seq2(from = where_after + 1, to = length(nams))]
  second_split <- second_split[which(var != second_split)]

  newOrder <- c(first_split, var, second_split)

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
