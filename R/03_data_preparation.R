
#### Merge value and variable label data frames
#############################################################################
#' Prepare data and metadata
#'
#' Function to transform list of data-frames and metainformation ready for data base creation
#'
#' The function createDB takes a list of data frames and a single data frame with metainformation as input. This function transforms lists from import_SPSS and import_RDS into a list of data frames (in the same order as inputted, which is then used for the merging order in createDB). Additionally the seperate lists of metainformation for each data frame are merged and a data frame unique identifier is added.
#'
#'@param ... \code{GADSdat} objects, as named arguments in the correct merge order.
#'
#'@return Returns an \code{all_GADSdat} object, which consists of list with a list of all data frames \code{"datList"} and a single data frame containing all meta data information \code{"allLabels"}.
#'
#'@examples
#'# Example data set
#'#to be done
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
  new_all_GADSdat(datList = dat_list, allLabels = label_df)
}

# 03)  add data frame identifier ---------------------------------------------------------
add_DFname <- function(df, name) {
  data.frame(df, data_table = rep(name, nrow(df)), stringsAsFactors = FALSE)
}


# create S3 all_GADSdata object
new_all_GADSdat <- function(datList, allLabels) {
  stopifnot(is.list(datList))
  stopifnot(is.data.frame(allLabels))
  structure(list(datList = datList, allLabels = allLabels), class = c("all_GADSdat", "list"))
}

check_all_GADSdat <- function(all_GADSdat) {
  if(!"all_GADSdat" %in% class(all_GADSdat)) stop("Input object has to be of class all_GADSdat", call. = FALSE)
  if(!is.list(all_GADSdat) && length(all_GADSdat) == 2) stop("all_GADSdat has to be a list with length two", call. = FALSE)
  if(!identical(names(all_GADSdat), c("datList", "allLabels"))) stop("List elements of a all_GADSdat object have to be 'datList' and 'allLabels'", call. = FALSE)
  if(!is.list(all_GADSdat$datList)) stop("dat element has to be a list", call. = FALSE)
  if(!is.data.frame(all_GADSdat$allLabels)) stop("labels element has to be a data frame", call. = FALSE)

  # internals
  dats <- all_GADSdat$datList
  labs <- all_GADSdat$allLabels
  if(!"data_table" %in% names(labs)) stop("data_table column is missing in labels data frame.")
  for(i in names(dats)) {
    temp_gads <- new_GADSdat(dat = dats[[i]], labels = labs[labs[, "data_table"] == i, names(labs) != "data_table"])
    check_GADSdat(temp_gads)
  }

  #varNames_labels <- unique(unlist(lapply(all_GADSdat$dat, names)))
  #if(!identical(unique(all_GADSdat$allLabels$varName), varNames_labels)) {
   # stop("Illegal names or order of names in label data frame. Make sure to use the import functions to create GADSdata objects.", call. = FALSE)
  #}
}

