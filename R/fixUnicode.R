####
#############################################################################
#' Fix encoding problems.
#'
#' Fix encoding problems of a \code{GADSdat} which was encoded presumably using \code{UTF-8} and imported using \code{ASCII} encoding.
#'
#'@param x A \code{GADSdat} object.
#'
#'@return The modified \code{GADSdat_imp}..
#'
#'@examples
#' # tbd
#'
#'@export
fixEncoding <- function(x) {
  UseMethod("fixEncoding")
}

fixEncoding.GADSdat <- function(x) {
  check_GADSdat(x)
  GADSdat <- x

  stop("Not implemented yet.")
  # fix data

  # fix meta data
  GADSdat_out
}



fixEncoding.character <- function(x) {
  lookup <- data.frame(unicode = c("C\026", "C..\023", "C\034", "C..\\$", "C\\$", "C..6", "C6", "C..<", "C<", "C..8", "C\037", "\001", "\025"),
             substitute = c("Oe", "Ue", "Ue", "ae", "ae", "oe", "oe", "ue", "ue", "ss", "ss", "", ""),
             stringsAsFactors = FALSE)
  for(i in seq(nrow(lookup))) {
    x <- gsub(lookup[i, "unicode"], lookup[i, "substitute"], x)
  }
  x
}

