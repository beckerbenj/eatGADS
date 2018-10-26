
#### Merge value and variable label data frames
#############################################################################
#' Prepare data and metadata
#'
#' Function to transform list of data-frames and metainformation ready for data base creation
#'
#' The function createDB takes a list of data frames and a single data frame with metainformation as input. This function transforms lists from import_SPSS and import_RDS into a list of data frames (in the same order as inputted, which is then used for the merging order in createDB). Additionally the seperate lists of metainformation for each data frame are merged and a data frame unique identifier is added.
#'
#'@param ... Lists including data and label element.
#'
#'@return Returns a list with a list of all data frames and a single data frame containing all variable and value labels.
#'
#'@examples
#'# Example data set
#'to be done
#'
#'@export
mergeLabels <- function(...) {
  UseMethod("mergeLabels")
}

#'@export
mergeLabels.GADSdat <- function(...) {
  l <- list(...)
  # 1) checks
  lapply(l, check_GADSdat)
  if(is.null(names(l))) stop("All input has to be named! See help for further clarification.")
  if(any(is.na(names(l)) || any(names(l) == ""))) stop("All input has to be named! See help for further clarification.")
  if(any(duplicated(names(l)))) stop("Names for data frames are duplicated!")
  ## add checks for sqlite compatability, maybe from eatDB?

  # 2) extract elements
  dat_list <- lapply(l, function(x) x$dat)
  label_list <- lapply(l, function(x) x$labels)

  # 3) add data frame identifier and create single long format df, drop duplicate rows
  label_list <- Map(add_DFname, df = label_list, name = names(label_list))
  label_df <- do.call(rbind, label_list)
  rownames(label_df) <- NULL

  # output: list of data frames ready for data base creation, and data frame with labels
  new_all_GADSdata(dfList = dat_list, allLabelDF = label_df)
}

# 03)  add data frame identifier ---------------------------------------------------------
add_DFname <- function(df, name) {
  data.frame(df, data_table = rep(name, nrow(df)), stringsAsFactors = FALSE)
}


# create S3 all_GADSdata object
new_all_GADSdata <- function(dfList, allLabelDF) {
  stopifnot(is.list(dfList))
  stopifnot(is.data.frame(allLabelDF))
  structure(list(dfList = dfList, allLabelDF = allLabelDF), class = "all_GADSdata")
}
