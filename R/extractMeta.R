
#### Get Metainformation
#############################################################################
#' Get Meta Data
#'
#' Extract meta data (e.g. variable and values labels) from an \code{eatGADS} object. This can be a \code{GADSdat}, an \code{all_GADSdat},
#' a labels \code{data.frame}, or the path to an existing data base.
#'
#' Meta data is stored tidily in all \code{GADSdat} objects as a separate long format data frame. This information can be extracted for a single or
#' multiple variables.
#'
#'@param GADSobject Either a \code{GADSdat} object or a path to an existing \code{eatGADS} data base.
#'@param vars A character vector containing variable names. If \code{NULL} (default), all available meta information is returned.
#'
#'@return Returns a long format data frame with meta information.
#'
#'@examples
#'# Extract Meta data from data base
#'db_path <- system.file("extdata", "pisa.db", package = "eatGADS")
#'extractMeta(db_path, vars = c("schtype", "sameteach"))
#'
#'# Extract Meta data from loaded/imported GADS
#'extractMeta(pisa, vars = c("schtype", "sameteach"))
#'
#'@export
extractMeta <- function(GADSobject, vars = NULL) {
  UseMethod("extractMeta")
}
#'@export
extractMeta.GADSdat <- function(GADSobject, vars = NULL){
  check_GADSdat(GADSobject)
  extractMeta_helper(labels = GADSobject$labels, vars = vars)
}
#'@export
extractMeta.all_GADSdat <- function(GADSobject, vars = NULL){
  check_all_GADSdat(GADSobject)
  extractMeta_helper(labels = GADSobject$allLabels, vars = vars)
}
## Version for labels data frame or changeTable (if more functions for changeTables are implemented add it as an own S3 class)
#'@export
extractMeta.data.frame <- function(GADSobject, vars = NULL){
  legal_names_labels <- c("varName", "varLabel", "format", "display_width", "labeled", "value", "valLabel", "missings", "data_table")
  legal_names_changeTable <- paste(legal_names_labels, "_new", sep = "")
  legal_names <- c(legal_names_labels, legal_names_changeTable)
  if(!all(names(GADSobject) %in% legal_names)) {
    stop("GADS_object has to be of type GADSdat, all_GADSdat or has to be a labels data frame created from GADS import functions.")
  }
  extractMeta_helper(labels = GADSobject, vars = vars)
}
#'@export
extractMeta.character <- function(GADSobject, vars = NULL){
  if(length(GADSobject) != 1) stop("GADS_object is not a character of length 1.")
  # checks for filePath are in eatDB
  labs <- labelsGADS(GADSobject)
  extractMeta.data.frame(GADSobject = labs, vars = vars)
}





## common helper function
extractMeta_helper <- function(vars, labels) {
  if(is.null(vars)) return(labels)
  misMatches <- vars[!vars %in% labels$varName]
  if(length(misMatches) > 0) stop("The following vars are not a variable in the GADSdat:\n", paste(misMatches, collapse = ", "), call. = FALSE)
  labels[labels$varName %in% vars, ]
}



