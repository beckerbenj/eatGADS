
#### Import R-data with explicit metadata
#############################################################################
#' Import R data frame with a explicit meta data sheet
#'
#' Function to create a \code{GADSdat} object based on a \code{dat} \code{data.frame} and a \code{labels} \code{data.frame}.
#'
#' A \code{GADSdat} is basically a \code{list} with two elements: a \code{dat} and a \code{labels} \code{data.frame}. If these elements are
#' separated, they can be cleanly tied together again by \code{import_raw2}. The function performs extensive checks on the integrity of the
#' resulting \code{GADSdat} object. See \code{\link{import_spss}} and \code{\link{import_raw}} for further details.
#'
#'@param dat A \code{dat} \code{data.frame} containing all actual data.
#'@param labels A \code{labels} \code{data.frame} containing all meta data.
#'
#'@return Returns a \code{GADSdat} object.
#'
#'@examples
#'dat <- data.frame(ID = 1:5, grade = c(1, 1, 2, 3, 1))
#'varLabels <- data.frame(varName = c("ID", "grade"),
#'                        varLabel = c("Person Identifier", "School grade Math"),
#'                        stringsAsFactors = FALSE)
#'valLabels <- data.frame(varName = c("grade", "grade", "grade"),
#'                        value = c(1, 2, 3),
#'                        valLabel = c("very good", "good", "sufficient"),
#'                        missings = c("valid", "valid", "valid"),
#'                        stringsAsFactors = FALSE)
#'
#'gads <- import_raw(df = dat, varLabels = varLabels, valLabels = valLabels, checkVarNames = FALSE)
#'
#'# separate the GADSdat object
#'dat <- gads$dat
#'labels <- gads$labels
#'
#'# rejoin it
#'dat <- import_raw2(dat, labels)
#'
#'@export
import_raw2 <- function(dat, labels) {
  if(!is.data.frame(dat)) stop("'dat' needs to be a data frame.")
  if(!is.data.frame(labels)) stop("'labels' needs to be a data frame.")
  if(any(sapply(dat, is.factor))) stop("At least one of the variables in 'dat' is a factor. All meta information on value level has to be stored in valLabels.")

  GADSdat <- new_GADSdat(dat = dat, labels = labels)
  check_GADSdat(GADSdat)

  GADSdat
}
