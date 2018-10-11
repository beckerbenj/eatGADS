#### Missings to NA
#############################################################################
#' Recode Missings to NA
#'
#' Recode Missings to NA according to missing labels in label data frame.
#'
#' Missings are imported as their values via import_spss. Using the value labels in the labels data frame, miss2NA recodes these missings codes to NA.
#'
#'@param importedSPSS Data frame imported via [import_spss].
#'
#'@return Returns the data frame with NA instead of missing codes.
#'
#'@examples
#'# Example data set
#'to be done
#'
#'@export
miss2NA <- function(labeledDat) {
  #
  datL <- lapply(names(labeledDat$dat), function(nam) {
    recodeVar(var = labeledDat$dat[, nam], labs = labeledDat$labels[labeledDat$labels$varName == nam, ])
  })
  dat <- as.data.frame(datL)
  names(dat) <- names(labeledDat$dat)
  dat
}



recodeVar <- function(var, labs){
  # extract missing labels
  mLabs <- labs[labs$miss == "miss", ]
  mCodes <- mLabs[, "value", drop = TRUE]
  # recode
  var[var %in% mCodes] <- NA
  var
}



#### Idee:
# data.frame mit Conversion-Regeln als Input, muss von Hand spezifiziert werden oder als data im packge -> default?
