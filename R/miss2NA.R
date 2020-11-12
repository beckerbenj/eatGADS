
#### Missings to NA
#############################################################################
#' Recode Missings to \code{NA}
#'
#' Recode Missings to \code{NA} according to missing labels in label \code{data.frame}.
#'
#' Missings are imported as their values via \code{\link{import_spss}}. Using the value labels in the labels \code{data.frame},
#' \code{miss2NA} recodes these missings codes to \code{NA}. This function is mainly intended for internal use.
#'
#'@param GADSdat A \code{GADSdat} object.
#'
#'@return Returns a \code{data.frame} with \code{NA} instead of missing codes.
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

