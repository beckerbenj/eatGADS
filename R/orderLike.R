#### Order like a character vector
#############################################################################
#' Order the variables in a \code{GADSdat}.
#'
#' Order the variables in a \code{GADSdat} according to a character vector. If there are discrepancies between the two sets, a warning is issued.
#'
#' The variables in the \code{dat} and in the \code{labels} section are ordered. Variables not contained in the character vector are moved to the end of the data.
#'
#'@param GADSdat A \code{GADSdat} object.
#'@param newOrder A character vector containing the order of variables.
#'
#'@return Returns a \code{GADSdat} object.
#'
#'
#'@export
orderLike <- function(GADSdat, newOrder) {
  UseMethod("orderLike")
}
#'@export
orderLike.GADSdat <- function(GADSdat, newOrder) {
  check_GADSdat(GADSdat)
  if(!is.character(newOrder) || !length(newOrder) > 0) stop("newOrder is not a character vector.")

  comp <- compare_and_order(set1 = names(GADSdat$dat), set2 = newOrder, name1 = "GADSdat", name2 = "new Order")

  newDat <- GADSdat$dat[, c(comp[["in_both_ordered"]], comp[["not_in_set2"]])]
  newLabels <- GADSdat$labels[order(match(GADSdat$labels$varName, names(newDat))), ]
  new_GADSdat(dat = newDat, labels = newLabels)
}


compare_and_order <- function(set1, set2, name1 = "set1", name2 = "set2", FUN = warning) {
  not_in_set1 <- setdiff(set2, set1)
  not_in_set2 <- setdiff(set1, set2)
  in_both_ordered <- intersect(set2, set1)

  call. <- FALSE
  if(identical(FUN, message)) call. <- NULL

  if(length(not_in_set1) > 0) FUN("The following variables are not in ", name1, ": ", paste(not_in_set1, collapse = ", "), call. = call.)
  if(length(not_in_set2) > 0) FUN("The following variables are not in ", name2, ": ", paste(not_in_set2, collapse = ", "), call. = call.)
  list(not_in_set1 = not_in_set1, not_in_set2 = not_in_set2, in_both_ordered = in_both_ordered)
}


