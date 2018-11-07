
#### Import SPSS data
#############################################################################
#' Import SPSS data
#'
#' Function to import sav files while extracting variable and value labels.
#'
#' SPSS files (.sav) store variable and value labels. Via import_spss data frames are imported and transformed to data frames. The meta-information is stored seperately in a long format data frame. Important Note: To get labels, missings have to be given explicit labels in SPSS! Additional missing column is generated.
#'
#'@param filePath Source file location, ending on .sav
#'
#'@return Returns a list with a) the actual data and b) a data frame with all variable and value labels in long format.
#'
#'@examples
#'# Example data set
#'to be done
#'
#'@export
import_spss <- function(filePath, checkVarNames = TRUE, labeledStrings = FALSE) {
  df <- load_spss(filePath = filePath)
  out <- prepare_labels(rawDat = df, checkVarNames = checkVarNames, labeledStrings = labeledStrings)
  out
}

#### Import RDS
#############################################################################
#' Import R data
#'
#' Function to import an RDS-saved file.
#'
#' ...
#'
#'@param filePath Source file location.
#'
#'@return Returns a list with a) the actual data and b) a data frame with all variable and value labels in long format.
#'
#'@examples
#'# Example data set
#'to be done
#'
#'@export
import_RDS <- function(filePath, checkVarNames = TRUE) {
  df <- load_R(filePath = filePath)
  out <- prepare_labels(rawDat = df, checkVarNames = checkVarNames, labeledStrings = FALSE)
  out
}

#### Import R-data
#############################################################################
#' Import R data frame
#'
#' Function to import a data frame from R.
#'
#' ...
#'
#'@param filePath Source file location.
#'
#'@return Returns a list with a) the actual data and b) a data frame with all variable and value labels in long format.
#'
#'@examples
#'# Example data set
#'to be done
#'
#'@export
import_DF <- function(df, checkVarNames = TRUE) {
  out <- prepare_labels(rawDat = df, checkVarNames = checkVarNames, labeledStrings = FALSE)
  out
}

# 01) Load data depending on format ---------------------------------------------------------
# import (keep NAs how they are coded to later mark values as missings but keep them seperatable)
load_spss <- function(filePath) {
  rawDat <- haven::read_spss(file = filePath, user_na = TRUE)
  new_savDat(rawDat)
}
# create S3 object savDat for internal use
new_savDat <- function(rawDat) {
  stopifnot(is.data.frame(rawDat))
  structure(rawDat, class = c("savDat", "data.frame"))
}

load_R <- function(filePath) {
  rawDat <- readRDS(file = filePath)
  new_data.frame(rawDat)
}
# create S3 object RDat for internal use
new_data.frame <- function(rawDat) {
  stopifnot(is.data.frame(rawDat))
  rawDat
}


# 02) Prepare and extract data ---------------------------------------------------------
prepare_labels <- function(rawDat, checkVarNames, labeledStrings) {
  # 1) check and prepare variable names
  if(identical(checkVarNames, TRUE)) names(rawDat) <- unlist(lapply(names(rawDat), transf_names))

  # 2) extract labels
  label_df <- extract_labels(rawDat = rawDat, labeledStrings = labeledStrings)

  # 3) depends on class! strip away labels from rawDat for spss, convert factors for R
  plainDat <- data.frame(lapply(rawDat, strip_attributes), stringsAsFactors = FALSE)

  # output
  new_GADSdat(dat = plainDat, labels = label_df)
}

# create S3 object GADSdat for User (needs interface!)
new_GADSdat <- function(dat, labels) {
  stopifnot(is.data.frame(dat) && is.data.frame(labels))
  structure(list(dat = dat, labels = labels), class = "GADSdat")
}
# GADSdat validator
check_GADSdat <- function(GADSdat) {
  if(!"GADSdat" %in% class(GADSdat)) stop("All input objects have to be of class GADSdat", call. = FALSE)
  if(!is.list(GADSdat) && length(GADSdat) == 2) stop("GADSdat has to be a list with length two", call. = FALSE)
  if(!identical(names(GADSdat), c("dat", "labels"))) stop("List elements of a GADSdat object have to be 'dat' and 'labels'", call. = FALSE)
  if(!is.data.frame(GADSdat$dat)) stop("dat element has to be a data frame", call. = FALSE)
  if(!is.data.frame(GADSdat$labels)) stop("labels element has to be a data frame", call. = FALSE)

  # internals
  if(!identical(unique(GADSdat$labels$varName), names(GADSdat$dat))) {
    stop("Illegal names or order of names in label data frame. Make sure to use the import functions to create GADSdata objects.", call. = FALSE)
  }
}


# 02.1) Check variable names ---------------------------------------------------------
# function for preparing of variable names (to be in line with sqlite rules)
transf_names <- function(vec_name) {
  NewName <- vec_name
  if(identical(vec_name, "group")) NewName <- "groupVar"
  if(grepl("\\.", vec_name))       NewName <- gsub("\\.", "_", vec_name)
  NewName <- make.names(NewName)

  if(!identical(NewName, vec_name)) message(paste(vec_name, "has been renamed to", NewName))
  NewName
}

# 02.2) extract labels ---------------------------------------------------------
extract_labels <- function(rawDat, labeledStrings) {
  attr_vec <- c("varName", "varLabel", "format", "display_width", "class", "value", "valLabel", "missings")

  label_df <- extract_variable_level(rawDat = rawDat)
  val_labels <- call_extract_values(rawDat = rawDat, labeledStrings = labeledStrings)

  # merge results and out with all names
  if(!is.null(val_labels)) label_df <- plyr::join(label_df, val_labels, by = "varName", type = "left", match = "all")
  add_vars <- setdiff(attr_vec, names(label_df))
  # preserve specific format of variables
  label_df[add_vars] <- NA_character_
  if(all(is.na(label_df$value))) label_df$value <- as.integer(label_df$value)
  if(all(is.na(label_df$display_width))) label_df$display_widt <- as.integer(label_df$display_widt)

  label_df[attr_vec]
}


# 02.3) strip away variable labels and factors ---------------------------------------------------------
strip_attributes <- function(vec) {
  attributes(vec) <- NULL
  vec
}



