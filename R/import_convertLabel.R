#### Import from convertLabel
#############################################################################
#' Import an object imported via \code{convertLabel}
#'
#' Function to import a \code{data.frame} object created by \code{convertLabel} for use in \code{eatGADS}. If possible, importing data via \code{\link{import_spss}} should always be preferred.
#'
#' \code{convertLabel} from \code{R} package \code{eatAnalysis} converts an object imported via \code{read.spss} (from the \code{foreign} package) to a \code{data.frame} with factors and variable labels stored in variable attributes.
#'
#'@param df A \code{data.frame}.
#'@param checkVarNames Should variable names be checked for violations of \code{SQLite} and \code{R} naming rules?
#'
#'@return Returns a list with the actual data \code{dat} and a data frame with all meta information in long format \code{labels}.
#'
#'
#'@export
import_convertLabel <- function(df, checkVarNames = TRUE) {
  if(!is.data.frame(df)) stop("df needs to be a data frame.")
  out <- prepare_labels(rawDat = df, checkVarNames = checkVarNames, labeledStrings = FALSE)

  varLabels <- sapply(df, function(x) {
    out <- attr(x, "varLabel")
    if(is.null(out)) return(NA)
    out
  })
  out2 <- changeVarLabels(out, varName = names(varLabels), varLabel = varLabels)
  out2
}
