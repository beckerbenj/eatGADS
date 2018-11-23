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
miss2NA <- function(GADSdat) {
  UseMethod("miss2NA")
}

#'@export
miss2NA.GADSdat <- function(GADSdat) {
  check_GADSdat(GADSdat)
  datL <- lapply(names(GADSdat$dat), function(nam) {
    recodeVar(var = GADSdat$dat[, nam], labs = GADSdat$labels[GADSdat$labels$varName == nam, ])
  })
  dat <- as.data.frame(datL)
  names(dat) <- names(GADSdat$dat)
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


#### Get Metainformation
#############################################################################
#' Get Metainformation and Labels
#'
#' Exctrat metainformation, variable and values labels from GADSdat.
#'
#' Metainformation is stored tidily in a GADSdat and can be extracted via extractMeta for a single or multiple variables.
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
extractMeta <- function(GADS_object, vars) {
  UseMethod("extractMeta")
}
#'@export
extractMeta.GADSdat <- function(GADS_object, vars){
  check_GADSdat(GADS_object)
  if(!all(vars %in% GADS_object$labels$varName)) stop("At least one of vars is not a variable in the GADSdat.", call. = FALSE)
  GADS_object$labels[GADS_object$labels$varName %in% vars, ]
}
#'@export
extractMeta.all_GADSdat <- function(GADS_object, vars){
  check_all_GADSdat(GADS_object)
  if(!all(vars %in% GADS_object$allLabels$varName)) stop("At least one of vars is not a variable in the all_GADSdat.", call. = FALSE)
  GADS_object$labels[GADS_object$labels$varName %in% vars, ]
}
## Version for labels data frame or changeTable (if more functions for changeTables are implemented add it as an own S3 class)
#'@export
extractMeta.data.frame <- function(GADS_object, vars){
  legal_names_labels <- c("varName", "varLabel", "format", "display_width", "class", "value", "valLabel", "missings", "data_table")
  legal_names_changeTable <- paste(legal_names_labels, "_new", sep = "")
  legal_names <- c(legal_names_labels, legal_names_changeTable)
  if(!all(names(GADS_object) %in% legal_names)) {
    stop("GADS_object has to be of type GADSdat, all_GADSdat or has to be a labels data frame created from GADS import functions.")
  }
  if(!all(vars %in% GADS_object$varName)) stop("At least one of vars is not a variable in the labels data frame.", call. = FALSE)
  GADS_object[GADS_object$varName %in% vars, ]
}



