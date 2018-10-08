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
import_spss <- function(filePath, labeledStrings = FALSE) {
  # import (keep NAs how they are coded to later mark values as missings but keep them seperatable)
  rawDat <- haven::read_spss(file = filePath, user_na = TRUE)

  # 1) check and prepare variable names
  names(rawDat) <- unlist(lapply(names(rawDat), transf_names))

  # 2) extract labels
  label_df <- extract_labels(rawDat = rawDat, type = "SPSS", labeledStrings = labeledStrings)

  # 3) strip away labels from rawDat
  plainDat <- data.frame(lapply(rawDat, strip_allLabels), stringsAsFactors = FALSE)

  # output
  list(dat = plainDat, labels = label_df)
}

# 01) Prepare data ---------------------------------------------------------
# function for preparing of variable names (to be in line with sqlite rules)
transf_names <- function(vec_name) {
  NewName <- vec_name
  if(identical(vec_name, "group")) {
    NewName <- "groupVar"
    message(paste(vec_name, "has been renamed to", NewName))
  }
  if(grepl("\\.", vec_name)) {
    NewName <- gsub("\\.", "_", vec_name)
    message(paste(vec_name, "has been renamed to", NewName))
  }
  NewName
}


# 02) extract labels ---------------------------------------------------------
# actually 2 functions, but important to keep code @ 1 place
extract_labels <- function(rawDat, old_labels = NULL, type = "SPSS", labeledStrings) {
  ## spss version of function
  if(identical(type, "SPSS")) {
    # a) extract variable labels
    var_labels <- extract_varLabels(spss_df = rawDat)
    # b) extract values labels
    val_labels <- extract_valueLabels(df = rawDat, type = type, labeledStrings = labeledStrings)

    # Merge into one label DF
    label_df <- merge(var_labels, val_labels, all = TRUE)
  }

  ## R version of function
  if(identical(type, "R")) {
    # a) extract values labels from factors
    fac_labels <- extract_valueLabels(df = rawDat, type = type, labeledStrings = labeledStrings)
    # b) create emtpy df if no variable and value labels so far
    if(is.null(old_labels)) {
      old_labels <- data.frame(matrix(ncol = 5, nrow = 0))
      names(old_labels) <- c("varName", "varLabel", "value", "label", "missings")
    }
    # Merge into one label DF (keep columns in order)
    label_df <- merge(old_labels, fac_labels, all = TRUE)[, union(names(old_labels), names(fac_labels))]
  }
  label_df
}


# a) ----------- variable labels
extract_varLabels <- function(spss_df) {
  # check for unknown attributes (mostly to secure against changes in haven)
  all_attr <- unlist(lapply(spss_df, function(var) names(attributes(var))))
  unknown_attr <- all_attr[!all_attr %in% c("label", "format.spss", "display_width", "class", "labels", "na_range", "na_value")]
  if(length(unknown_attr) > 0) stop("Unknown attributes exported from haven:", unknown_attr, ". Please contact package author.")

  varLabels <- unlist(lapply(spss_df, extract_attribute, attr_name = "label"))
  varFormat <- unlist(lapply(spss_df, extract_attribute, attr_name = "format.spss"))
  varWidth <- unlist(lapply(spss_df, extract_attribute, attr_name = "display_width"))
  varClass <- unlist(lapply(spss_df, extract_attribute, attr_name = "class"))
  #browser()
  varLabel_df <- data.frame(names(spss_df), varLabels, varFormat, varWidth, varClass, stringsAsFactors = F)
  # create empty data frame if no variable labels in sav
  if(is.null(varLabel_df)) varLabel_df <- data.frame(matrix(ncol = 2, nrow = 0))
  # names
  names(varLabel_df) <- c("varName", "varLabel", "format", "display_width", "class")
  rownames(varLabel_df) <- NULL

  varLabel_df
}

# extract attributes and produce NA for not given attributes
extract_attribute <- function(var, attr_name) {
  out <- attr(var, attr_name)
  if(is.null(out)) out <- NA
  out
}

# b) ----------- value labels
# all variables, for SPSS and R
extract_valueLabels <- function(df, type = "SPSS", labeledStrings) {
  if(identical(type, "SPSS")) {
    FUN = extract_VL_SPSS
  } else if(identical(type, "R")) {
    FUN = extract_VL_R
  } else stop("Invalid type")

  # extract labels into one long format data frame
  valueList <- Map(FUN, var = df, varName = names(df), labeled_strings = labeledStrings)
  valLabel_df <- do.call(rbind, valueList)
  # add names to data frame, create emtpy data frame if no labels
  if(is.null(valLabel_df)) valLabel_df <- data.frame(matrix(ncol = 4, nrow = 0))
  names(valLabel_df) <- c("varName", "value", "label", "missings")
  rownames(valLabel_df) <- NULL
  valLabel_df
}

# single variable for SPSS
extract_VL_SPSS <- function(var, varName, labeled_strings = FALSE) {
  # check if there are value labels
  if(is.null(attributes(var)$labels)) return(NULL)
  # default behavior: transform value labels to numeric if possible, drop otherwise
  values <- attr(var, "labels")
  if(identical(labeled_strings, FALSE)) {
    values <- tryCatch(as.numeric(values), warning = function(w) w)
    if(any(class(values) == "warning")) {
      values <- NA
      warning("Values for ", varName, " cannot be coerced to numeric and have been dropped. \n")
    }
  }
  # extract value labels and return as long format df
  df <- data.frame(varName = rep(varName, length(attr(var, "labels"))),
                 value = values,
                 label = attr(attr(var, "labels"), "names"),
                 missings = NA,
                 stringsAsFactors = FALSE)

  ## extract missings and add as extra label
  # stopifnot(is.numeric(df$value))
  df <- extract_Miss_SPSS(var = var, label_df = df)

  rownames(df) <- NULL

  df
}

# extract if label is label for missing values
extract_Miss_SPSS <- function(var, label_df) {
  na_range <- attr(var, "na_range")
  na_value <- attr(var, "na_value")
  # check if any na_value without this label (check not performed for na_range!)
  lapply(na_value, function(val) {
    if(!val %in% label_df$value) {
        warning(val, " used as missing label for variable ", label_df$varName[1], " but no value label given. Label is dropped.")
  }})

  # add variable indicating missings
  if(!is.null(na_range) || !is.null(na_value)) {
    # give values for ifelse
    if(is.null(na_range)) na_range <- c(Inf, -Inf)
    if(is.null(na_value)) na_value <- numeric(0)
    label_df$missings <- ifelse(label_df$value >= na_range[1] & label_df$value <= na_range[2] |
                                  label_df$value %in% na_value, "miss", NA)
  }
  rownames(label_df) <- NULL
  label_df
}


# single variable for R (factors!)
extract_VL_R <- function(var, varName) {
  # check if it is a factor
  if(!is.factor(var)) return(NULL)
  # extract value labels
  labels <- levels(var)
  # create corresponding integers
  values <- seq_along(labels)
  df <- data.frame(varName = rep(varName, length(values)),
                   value = values,
                   label = labels,
                   missings = NA)

  rownames(df) <- NULL

  ### insert missing extraction, add col like in spss function

  df
}



# 03) strip away variable labels ---------------------------------------------------------
strip_allLabels <- function(vec) {
  attributes(vec) <- NULL
  vec
}


#### Import R-data
#############################################################################
#' Import R data
#'
#' Function to import an RDS-saved file (maybe change to list??)
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
import_RDS <- function(filePath) {
  # import
  l <- readRDS(file = filePath)
  rawDat <- l$dat
  label_df <- l$labels

  # 1) check and prepare variable names
  names(rawDat) <- unlist(lapply(names(rawDat), transf_names))

  # 2) extract labels
  label_df <- extract_labels(rawDat = rawDat, old_labels = label_df, type = "R")

  # 3) all factors to integers
  plainDat <- data.frame(lapply(rawDat, fac2int), stringsAsFactors = FALSE)

  # output
  list(dat = plainDat, labels = label_df)
}

# 03)  factors to integers ---------------------------------------------------------
fac2int <- function(vec) {
  if(!is.factor(vec)) return(vec)
  as.integer(vec)
}

