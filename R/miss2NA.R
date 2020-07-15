
#### Missings to NA
#############################################################################
#' Recode Missings to \code{NA}
#'
#' Recode Missings to \code{NA} according to missing labels in label \code{data.frame}.
#'
#' Missings are imported as their values via \code{\link{import_spss}}. Using the value labels in the labels \code{data.frame},
#' \code{miss2NA} recodes these missings codes to \code{NA}.
#'
#'@param GADSdat A \code{GADSdat} object.
#'
#'@return Returns the data frame with \code{NA} instead of missing codes.
#'
#'@examples
#'# Example data set
#'#to be done
#'
#'@export
miss2NA <- function(GADSdat) {
  UseMethod("miss2NA")
}

#'@export
miss2NA.GADSdat <- function(GADSdat) {
  check_GADSdat(GADSdat)
  datL <- lapply(names(GADSdat$dat), function(nam) {
    recodeVar(var = GADSdat$dat[, nam], labs = GADSdat$labels[GADSdat$labels$varName == nam, ])
  })
  dat <- as.data.frame(datL, stringsAsFactors = FALSE)
  names(dat) <- names(GADSdat$dat)
  dat
}

recodeVar <- function(var, labs){
  # extract missing labels
  mLabs <- labs[labs$miss == "miss", ]
  mCodes <- na_omit(mLabs[, "value", drop = TRUE])
  # recode
  var[var %in% mCodes] <- NA
  var
}

#### Idee:
# data.frame mit Conversion-Regeln als Input, muss von Hand spezifiziert werden oder als data im packge -> default?
