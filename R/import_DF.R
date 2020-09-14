#### Import R-data
#############################################################################
#' Import R \code{data.frame}
#'
#' Function to import a \code{data.frame} object for use in \code{eatGADS} while extracting value labels from factors.
#'
#' Factors are integers with labeled variable levels. \code{import_DF} extracts these labels and stores them in a separate meta data data.frame.
#' See \code{\link{import_spss}} for detailed information.
#'
#'@param df A \code{data.frame}.
#'@param checkVarNames Should variable names be checked for violations of \code{SQLite} and \code{R} naming rules?
#'
#'@return Returns a list with the actual data \code{dat} and a data frame with all meta information in long format \code{labels}.
#'
#'@examples
#'dat <- import_DF(iris, checkVarNames = FALSE)
#'
#'# Inspect Meta data
#'extractMeta(dat)
#'
#'# Extract Data
#'dat <- extractData(dat, convertLabels = "character")
#'
#'@export
import_DF <- function(df, checkVarNames = TRUE) {
  if(!is.data.frame(df)) stop("df needs to be a data frame.")
  zeroLevels <- sapply(df, function(dfVar) is.factor(dfVar) && identical(levels(dfVar), character(0)))
  if(any(zeroLevels)) warning("The following variables in the data are factors with zero valid levels: ",
                           paste(names(zeroLevels)[zeroLevels], collapse = ", "))

  out <- prepare_labels(rawDat = df, checkVarNames = checkVarNames, labeledStrings = FALSE)
  out
}
