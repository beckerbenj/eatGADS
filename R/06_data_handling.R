#### extractData
#############################################################################
#' Extract Data
#'
#' Extract data.frame from a \code{GADSdat} object for analyses in R.
#'
#' A \code{GADSdat} object includes actual data and the corresponding meta data information. extractData extracts the data and applies relevant meta information (missing conversion, value labels), so the data can be used for analyses in R. Careful: If factors are extracted, the underlying integers will not equal the original underlying integers and \code{as.numeric} will probably yield undesired results.
#'
#'@param GADSdat A \code{GADSdat} object.
#'@param convertMiss Should values labeled as missings be recoded to \code{NA}?
#'@param convertLabels If \code{"numeric"}, values remain as numerics. If \code{"factor"} or \code{"character"}, values are recoded to their labels. Corresponding variable type is applied.
#'@param dropPartialLabels Should partially complete labels be dropped? Often, this is the desired behaviour.
#'@param convertVariables Character vector of variables names, which labels should be applied to. If not specified, value labels are applied to all variables.
#'
#'@return Returns a data frame.
#'
#'@examples
#'# Example data set
#'#to be done
#'
#'@export
extractData <- function(GADSdat, convertMiss = TRUE, convertLabels = "character", dropPartialLabels = TRUE, convertVariables) {
  UseMethod("extractData")
}

#'@export
extractData.GADSdat <- function(GADSdat, convertMiss = TRUE, convertLabels = "character", dropPartialLabels = TRUE, convertVariables) {
  check_GADSdat(GADSdat)
  if(!convertLabels %in% c("character", "factor", "numeric") && length(convertLabels) == 1) stop("Argument convertLabels incorrectly specified.")
  dat <- GADSdat$dat
  labels <- GADSdat$labels
  ## missings
  if(identical(convertMiss, TRUE)) dat <- miss2NA(GADSdat)
  ## labels
  dat <- labels2values(dat = dat, labels = labels, convertLabels = convertLabels, convertMiss = convertMiss,
                       dropPartialLabels = dropPartialLabels, convertVariables = convertVariables)
  dat
}

# converts labels to values
labels2values <- function(dat, labels, convertLabels, convertMiss, dropPartialLabels, convertVariables) {
  if(identical(convertLabels, "numeric")) return(dat)
  # Which variables should their value labels be applied to?
  if(missing(convertVariables)) convertVariables <- names(dat)
  stopifnot(is.character(convertVariables) && length(convertVariables) > 0)
  change_labels <- labels[labels[, "varName"] %in% convertVariables, ]    # careful, from here use only change_labels!
  # check value labels, remove incomplete labels from insertion to protect variables
  if(identical(dropPartialLabels, TRUE)) {
    drop_labels <- unlist(lapply(unique(labels$varName), check_labels, dat = dat, labels = labels, convertMiss = convertMiss))
    change_labels <- change_labels[!change_labels$varName %in% drop_labels, ]
  }
  # convert labels into values
  changed_variables <- character(0)
  # early return, if no values are to be recoded
  if(nrow(change_labels) == 0) return(dat)
  # recode values
  for(i in seq(nrow(change_labels))) {
    curRow <- change_labels[i, , drop = FALSE]
    #browser()
    if(!is.na(curRow$valLabel)) {
      ## preserve numeric type of variable if possible (although not sure whether this could realistically be the case...)
      curRow$valLabel <- suppressWarnings(eatTools::asNumericIfPossible(curRow$valLabel, force.string = FALSE))
      # so far fastest: maybe car? mh...
      dat[which(dat[, curRow$varName] == curRow$value), curRow$varName] <- curRow$valLabel
      # dat[, curRow$varName] <- ifelse(dat[, curRow$varName] == curRow$value, curRow$valLabel, dat[, curRow$varName])
      changed_variables <- unique(c(curRow$varName, changed_variables))
    }
  }
  # convert characters to factor if specified
  if(identical(convertLabels, "factor")) {
    for(i in changed_variables) dat[, i] <- as.factor(dat[, i])
  }
  dat
}

# check if variable is correctly labeled, issues warning
check_labels <- function(varName, dat, labels, convertMiss) {
  # if(varName == "VAR3") browser()
  real_values <- na_omit(unique(dat[[varName]]))
  labeled_values <- na_omit(labels[labels$varName == varName, "value"])
  ## either all labeled
  if(all(real_values %in% labeled_values)) return()
  ## or no labels except missings (if missings are recoded, else this is irrelevant)
  if(identical(convertMiss, TRUE)) {
    labeled_values <- na_omit(labels[labels$varName == varName & labels$missings == "valid", "value"])
    if(length(labeled_values) == 0) return(varName)
  }
  warning("Variable ", varName, " is partially labeled. Value labels will be dropped for this variable variable.\n",
          "Labeled values are: ", paste(labeled_values, collapse = ", "), call. = FALSE)

  varName
  #warning("Variable ", varName, " is partially labeled. Value labels will be dropped for this variable variable.\nExisting values are: ",
  #        paste(real_values, collapse = ", "), "\n", "Labeled values are: ", paste(labeled_values_noMiss, collapse = ", "), call. = FALSE)
}

na_omit <- function(vec) {
  vec[!is.na(vec)]
}


#### Missings to NA
#############################################################################
#' Recode Missings to \code{NA}
#'
#' Recode Missings to \code{NA} according to missing labels in label data frame.
#'
#' Missings are imported as their values via \code{\link{import_spss}}. Using the value labels in the labels data frame, \code{miss2NA} recodes these missings codes to \code{NA}.
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


#### Get Metainformation
#############################################################################
#' Get Metainformation and Labels
#'
#' Exctrat metainformation, variable and values labels from an \code{eatGADS} object with meta information. This can be a \code{GADSdat}, an \code{all_GADSdat}, a labels \code{data.frame}, or the path to an existing data base.
#'
#' Metainformation is stored tidily in a GADSdat and can be extracted via extractMeta for a single or multiple variables.
#'
#'@param GADS_object A \code{GADSdat} object.
#'@param vars A character vector containing variable names. If \code{NULL} (default), alle available metainformation is returned.
#'
#'@return Returns a long format data frame with meta information.
#'
#'@examples
#'# Example data set
#'#to be done
#'
#'@export
extractMeta <- function(GADS_object, vars = NULL) {
  UseMethod("extractMeta")
}
#'@export
extractMeta.GADSdat <- function(GADS_object, vars = NULL){
  check_GADSdat(GADS_object)
  extractMeta_helper(labels = GADS_object$labels, vars = vars)
}
#'@export
extractMeta.all_GADSdat <- function(GADS_object, vars = NULL){
  check_all_GADSdat(GADS_object)
  extractMeta_helper(labels = GADS_object$allLabels, vars = vars)
}
## Version for labels data frame or changeTable (if more functions for changeTables are implemented add it as an own S3 class)
#'@export
extractMeta.data.frame <- function(GADS_object, vars = NULL){
  legal_names_labels <- c("varName", "varLabel", "format", "display_width", "labeled", "value", "valLabel", "missings", "data_table")
  legal_names_changeTable <- paste(legal_names_labels, "_new", sep = "")
  legal_names <- c(legal_names_labels, legal_names_changeTable)
  if(!all(names(GADS_object) %in% legal_names)) {
    stop("GADS_object has to be of type GADSdat, all_GADSdat or has to be a labels data frame created from GADS import functions.")
  }
  extractMeta_helper(labels = GADS_object, vars = vars)
}
#'@export
extractMeta.character <- function(GADS_object, vars = NULL){
  if(length(GADS_object) != 1) stop("GADS_object is not a character of length 1.")
  # checks for filePath are in eatDB
  labs <- labelsGADS(GADS_object)
  extractMeta.data.frame(GADS_object = labs, vars = vars)
}





## common helper function
extractMeta_helper <- function(vars, labels) {
  if(is.null(vars)) return(labels)
  misMatches <- vars[!vars %in% labels$varName]
  if(length(misMatches) > 0) stop("The following vars are not a variable in the GADSdat:\n", paste(misMatches, collapse = ", "), call. = FALSE)
  labels[labels$varName %in% vars, ]
}



