
#### Import SPSS data
#############################################################################
#' Import SPSS data
#'
#' Function to import \code{.sav} files while extracting meta information, e.g. variable and value labels.
#'
#' SPSS files (\code{.sav}) store variable and value labels and assign specific formatting to variables. \code{import_spss} imports data from SPSS, while storing this meta-information seperately in a long format data frame. Value labels and missing labels are used to identify missing values (see \code{\link{checkMissings}}).
#'
#'@param filePath Source file location, ending on \code{.sav}.
#'@param checkVarNames Should variable names be checked for vioalitions of \code{SQLite} and \code{R} naming rules?
#'@param labeledStrings Should strings as labeled values be allowed? This possibly corrupts all labeled values.
#'
#'@return Returns a list with the actual data \code{dat} and a data frame with all meta information in long format \code{labels}.
#'
#'@examples
#'\dontrun{
#'dat <- import_spss("t:/_R_Tutorials/R_Workshops/01_Allgemeine Einfuehrung/IQB-LV-2011_SchuelerInnen-Eltern_CF.sav",
#'                   checkVarNames = FALSE)
#'
#'# Inspect Meta data
#'extractMeta(dat)
#'
#'# Extract Data
#'dat <- extractData(dat, convertLabels = "character")
#'}
#'
#'@export
import_spss <- function(filePath, checkVarNames = TRUE, labeledStrings = FALSE) {
  df <- load_spss(filePath = filePath)
  out <- prepare_labels(rawDat = df, checkVarNames = checkVarNames, labeledStrings = labeledStrings)
  out
}

#### Import RDS
#############################################################################
#' Import RDS object
#'
#' Function to import \code{.RDS} files while extracting value labels from factors.
#'
#' Factors are integers with labeled variable levels. \code{import_RDS} extracts these labels and stores them in a seperate meta data data.frame. See \code{\link{import_DF}} for detailed information.
#'
#'@param filePath Source file location, ending on \code{.RDS}.
#'@param checkVarNames Should variable names be checked for vioalitions of \code{SQLite} and \code{R} naming rules?
#'
#'@return Returns a list with the actual data \code{dat} and a data frame with all meta information in long format \code{labels}.
#'
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
#' Function to import a \code{data.frame} object for use in \code{eatGADS} while extracting value labels from factors.
#'
#' Factors are integers with labeled variable levels. \code{import_DF} extracts these labels and stores them in a seperate meta data data.frame. See \code{\link{import_spss}} for detailed information.
#'
#'@param df A \code{data.frame}.
#'@param checkVarNames Should variable names be checked for vioalitions of \code{SQLite} and \code{R} naming rules?
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
  out <- prepare_labels(rawDat = df, checkVarNames = checkVarNames, labeledStrings = FALSE)
  out
}

#### Import from convertLabel
#############################################################################
#' Import an object imported via \code{convertLabel}
#'
#' Function to import a \code{data.frame} object created by \code{convertLabel} for use in \code{eatGADS}. If possible, importing data via \code{\link{import_spss}} should always be prefred.
#'
#' \code{convertLabel} converts an object imported via \code{foreign::read.spss} to a data.frame with factors and variable labels stored in variable attributes.
#'
#'@param df A \code{data.frame}.
#'@param checkVarNames Should variable names be checked for vioalitions of \code{SQLite} and \code{R} naming rules?
#'
#'@return Returns a list with the actual data \code{dat} and a data frame with all meta information in long format \code{labels}.
#'
#'@examples
#'# no examples
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


#### Import R-data with explicit metadata
#############################################################################
#' Import R data frame with explicit meta data sheets
#'
#' Function to import a \code{data.frame} object for use in \code{eatGADS} while adding explicit variable and value meta information through separate \code{data.frames}.
#'
#' The argument \code{varLables} has to contain exactly two variables, namely \code{varName} and \code{varLabel}. \code{valLables} has to contain exactly four variables, namely \code{varName}, \code{value}, \code{valLabel} and \code{missings}. The column \code{value} can only contain numerical values. The column \code{missings} can only contain the values \code{"valid"} and \code{"miss"}. Variables of type \code{factor} are not supported in any of the \code{data.frames}.
#'
#'@param df A \code{data.frame}.
#'@param varLabels A \code{data.frame} containing the variable labels. All variables in the data have to have exactly one column in this data.frame.
#'@param valLabels A \code{data.frame} containing the value labels. All referenced variables have to appear in the data, but not all variables in the data have to receive value labels. Can be omitted.
#'@param checkVarNames Should variable names be checked for vioalitions of \code{SQLite} and \code{R} naming rules?
#'
#'@return Returns a list with the actual data \code{dat} and with all meta information in long format \code{labels}.
#'
#'@examples
#'dat <- data.frame(ID = 1:5, grade = c(1, 1, 2, 3, 1))
#'varLabels <- data.frame(varName = c("ID", "grade"), varLabel = c("Person Identifier", "School grade Math"))
#'valLabels <- data.frame(varName = c("grade", "grade", "grade"),
#'                        value = c(1, 2, 3),
#'                        valLabel = c("very good", "good", "sufficient"),
#'                        missings = c("valid", "valid", "valid"))
#'
#'gads <- import_raw(df = dat, varLables = varLabels, valLabels = valLabels, checkVarNames = FALSE)
#'
#'# Inspect Meta data
#'extractMeta(gads)
#'
#'# Extract Data
#'dat <- extractData(gads, convertLabels = "character")
#'
#'@export
import_raw <- function(df, varLabels, valLabels = NULL, checkVarNames = TRUE) {
  if(!is.data.frame(df)) stop("df needs to be a data frame.")
  if(any(sapply(df, is.factor))) stop("One of the variables in df is a factor. All meta information on value level has to be stored in valLabels.")

  ## data import
  GADS_raw <- prepare_labels(rawDat = df, checkVarNames = checkVarNames, labeledStrings = FALSE) ## use import_df instead?

  #### varLabels
  check_varLabels(df = df, varLabels = varLabels)
  change_var <- getChangeMeta(GADS_raw, level = "variable")
  names(varLabels)[names(varLabels) == "varLabel"] <- "varLabel_new"
  change_var <- merge(change_var[, !names(change_var) %in% "varLabel_new"], varLabels, all = FALSE, sort = FALSE)
  change_var <- new_varChanges(change_var)
  GADS_raw2 <- applyChangeMeta(change_var, GADS_raw)

  #### valLabels (optional)
  if(!is.null(valLabels)) {
    check_valLabels(df = df, valLabels = valLabels)
    change_val_ori <- getChangeMeta(GADS_raw2, level = "value")
    variables_val_changes <- compare_and_order(change_val_ori$varName, valLabels$varName, FUN = paste) ## paste to suppress warning, maybe later as informative message?
    change_val <- change_val_ori[!change_val_ori$varName %in% variables_val_changes$in_both_ordered, ]
    names(valLabels)[names(valLabels) == "value"] <- "value_new"
    names(valLabels)[names(valLabels) == "valLabel"] <- "valLabel_new"
    names(valLabels)[names(valLabels) == "missings"] <- "missings_new"
    valLabels[, c("value", "valLabel", "missings")] <- NA
    change_val_new <- rbind(change_val, valLabels)
    change_val_new <- change_val_new[order(match(change_val_new$varName, unique(change_val_ori$varName))),
                                     match(names(change_val_new), names(change_val_ori))]
    change_val_new <- new_valChanges(change_val_new)
    GADS_raw2 <- applyChangeMeta(change_val_new, GADS_raw2)
  }

  GADS_raw2
}

# Check functions for import_raw
check_varLabels <- function(df, varLabels) {
  if(any(sapply(varLabels, is.factor))) stop("One of the variables in varLabels is a factor.")
  if(!is.data.frame(varLabels)) stop("varLabels has to be a data.frame.")
  if(!identical(names(varLabels), c("varName", "varLabel"))) stop("varLabels needs to contain the variables 'varName' and 'varLabel'.")

  compare_and_order(set1 = names(df), set2 = varLabels[["varName"]], name1 = "the data df", name2 = "varLabels", FUN = stop)
  dup_names <- varLabels[["varName"]][duplicated(varLabels[["varName"]])]
  if(length(dup_names) > 0) stop("The following variables have duplicated rows in varLabels: ", paste(dup_names, collapse = ", "))
}

check_valLabels <- function(df, valLabels) {
  if(any(sapply(valLabels, is.factor))) stop("One of the variables in valLabels is a factor.")
  if(!is.data.frame(valLabels)) stop("valLabels has to be a data.frame.")
  if(!identical(names(valLabels), c("varName", "value", "valLabel", "missings"))) stop("valLabels needs to contain the variables 'varName', 'value', 'varLabel' and 'missings'.")

  not_in_df <- setdiff(valLabels[["varName"]], names(df))
  if(length(not_in_df) > 0) stop("The following variables are not in the data df: ", paste(not_in_df, collapse = ", "))
  if(!is.numeric(valLabels$value)) stop("Value column of valLabels has to be numeric.")
  if(!all(valLabels$missings %in% c("valid", "miss"))) stop("All values in column 'missings' of valLabels must be either 'valid' or 'miss'.")
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


# 02.1) Check variable names ---------------------------------------------------------
# function for preparing of variable names (to be in line with sqlite rules)
transf_names <- function(vec_name) {
  NewName <- vec_name
  if(any(grepl(paste0("^", vec_name, "$"),  eatDB::sqlite_keywords, ignore.case = TRUE))) {
    NewName <- paste0(vec_name, "Var")
  }
  NewName <- make.names(NewName)
  if(grepl("\\.", NewName))       NewName <- gsub("\\.", "_", NewName)


  if(!identical(NewName, vec_name)) message(paste(vec_name, "has been renamed to", NewName))
  NewName
}


# 02.2) extract labels ---------------------------------------------------------
extract_labels <- function(rawDat, labeledStrings) {
  attr_vec <- c("varName", "varLabel", "format", "display_width", "labeled", "value", "valLabel", "missings")

  label_df <- extract_variable_level(rawDat = rawDat)
  val_labels <- call_extract_values(rawDat = rawDat, labeledStrings = labeledStrings)

  # merge results and out with all names
  if(!is.null(val_labels)) label_df <- plyr::join(label_df, val_labels, by = "varName", type = "left", match = "all")
  add_vars <- setdiff(attr_vec, names(label_df))
  # preserve specific format of variables
  label_df[add_vars] <- NA_character_
  if(all(is.na(label_df$value))) label_df$value <- as.integer(label_df$value)
  if(all(is.na(label_df$display_width))) label_df$display_width <- as.integer(label_df$display_width)

  label_df[attr_vec]
}


# 02.3) strip away variable labels and factors ---------------------------------------------------------
strip_attributes <- function(vec) {
  attributes(vec) <- NULL
  vec
}



