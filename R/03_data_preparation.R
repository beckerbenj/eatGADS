
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
  # 1) checks (allow NULL as element for trend_GADSdat and Linking errors)
  lapply(l, function(l_element) {
    if(!is.null(l_element)) check_GADSdat(l_element)
  })
  if(is.null(names(l))) stop("All input has to be named! See help for further clarification.")
  if(any(is.na(names(l))) || any(names(l) == "")) stop("All input has to be named! See help for further clarification.")
  if(any(duplicated(names(l)))) stop("Names for data frames are duplicated!")
  ## add checks for sqlite compatability, maybe from eatDB?

  # 2) extract elements
  dat_list <- lapply(l, function(x) x$dat)
  label_list <- lapply(l, function(x) x$labels)

  # 3) add data frame identifier and create single long format df, drop duplicate rows
  label_df <- merge_labels_dfs(label_list)

  # output: list of data frames ready for data base creation, and data frame with labels
  new_all_GADSdat(datList = dat_list, allLabels = label_df)
}

# 03)  add data frame identifier ---------------------------------------------------------
add_DFname <- function(df, name) {
  if(is.null(df)) return(df)
  data.frame(df, data_table = rep(name, nrow(df)), stringsAsFactors = FALSE)
}

merge_labels_dfs <- function(label_list, name = names(label_list)) {
  label_list <- Map(add_DFname, df = label_list, name = name)
  label_df <- do.call(rbind, label_list)
  rownames(label_df) <- NULL
  label_df
}


# create S3 all_GADSdata object
new_all_GADSdat <- function(datList, allLabels) {
  stopifnot(is.list(datList))
  stopifnot(is.data.frame(allLabels))
  structure(list(datList = datList, allLabels = allLabels), class = c("all_GADSdat", "list"))
}

check_all_GADSdat <- function(all_GADSdat, GADSdatChecks = TRUE) {
  if(!"all_GADSdat" %in% class(all_GADSdat)) stop("Input object has to be of class all_GADSdat", call. = FALSE)
  if(!is.list(all_GADSdat) && length(all_GADSdat) == 2) stop("all_GADSdat has to be a list with length two", call. = FALSE)
  if(!identical(names(all_GADSdat), c("datList", "allLabels"))) stop("List elements of a all_GADSdat object have to be 'datList' and 'allLabels'", call. = FALSE)
  if(!is.list(all_GADSdat$datList)) stop("dat element has to be a list", call. = FALSE)
  if(!is.data.frame(all_GADSdat$allLabels)) stop("labels element has to be a data frame", call. = FALSE)

  # internals
  if(!"data_table" %in% names(all_GADSdat[["allLabels"]])) stop("data_table column is missing in labels data frame.")
  # avoid infinite functions calls with extractGADSdat
  if(!GADSdatChecks) return()
  for(i in names(all_GADSdat[["datList"]])) {
    # browser()
    temp_gads <- extractGADSdat(all_GADSdat = all_GADSdat, name = i)
    check_GADSdat(temp_gads)
  }

  #varNames_labels <- unique(unlist(lapply(all_GADSdat$dat, names)))
  #if(!identical(unique(all_GADSdat$allLabels$varName), varNames_labels)) {
   # stop("Illegal names or order of names in label data frame. Make sure to use the import functions to create GADSdata objects.", call. = FALSE)
  #}
}



#### Extract GADSdat from all_GADSdat object (especially useful for Meta changes)
#############################################################################
#' Extract single GADSdat from all_GADSdat
#'
#' Function to extract a single \code{GADSdat} from an \code{all_GADSdat} object
#'
#' The function createDB takes a list of data frames and a single data frame with metainformation as input. This function transforms lists from import_SPSS and import_RDS into a list of data frames (in the same order as inputted, which is then used for the merging order in createDB). Additionally the seperate lists of metainformation for each data frame are merged and a data frame unique identifier is added.
#'
#'@param all_GADSdat \code{all_GADSdat} object, containing a table with the corresponding GADSdat name
#'@param name A character vector with length 1 with the name of the single GADSdat
#'
#'@return Returns an \code{GADSdat} object.
#'
#'@examples
#'# Example data set
#'#to be done
#'
#'@export
extractGADSdat <- function(all_GADSdat, name) {
  UseMethod("extractGADSdat")
}
#'@export
extractGADSdat.all_GADSdat <- function(all_GADSdat, name) {
  check_all_GADSdat(all_GADSdat, GADSdatChecks = FALSE)
  if(!is.character(name) || !length(name) == 1) stop("name has to be a character vector of length 1.")
  if(!name %in% names(all_GADSdat[["datList"]])) stop("name has to be the name of a GADSdat element of all_GADSdat.")

  extracted_meta <- all_GADSdat[["allLabels"]][all_GADSdat[["allLabels"]]$data_table == name, ]
  extracted_meta <- extracted_meta[, names(extracted_meta) != "data_table"]
  rownames(extracted_meta) <- NULL
  out_GADSdat <- new_GADSdat(dat = all_GADSdat[["datList"]][[name]], labels = extracted_meta)
  check_GADSdat(out_GADSdat)

  out_GADSdat
}


#### Split up a wide/long format GADS in hierarchical levels
#############################################################################
#' Split GADS
#'
#' Function to split a \code{GADSdat} with all variables in it into hierarchical levels.
#'
#' The function takes a \code{GADSdat} object and splits it into its desired hierarchical levels. Meta data include therefore then references via \code{data_table} to the corresponding hierarchical level.
#'
#'@param GADSdat \code{GADSdat} objects, as named arguments in the correct merge order.
#'@param nameList A list with character vector. The names in the list correspond the the hierarchy levels.
#'
#'@return Returns an \code{all_GADSdat} object, which consists of list with a list of all data frames \code{"datList"} and a single data frame containing all meta data information \code{"allLabels"}. For more details see also \code{\link{mergeLabels}}.
#'
#'@examples
#'# Example data set
#'#to be done
#'
#'@export
splitGADS <- function(GADSdat, nameList) {
  UseMethod("splitGADS")
}

#'@export
splitGADS.GADSdat <- function(GADSdat, nameList) {
  # 1) checks
  check_GADSdat(GADSdat)
  # add checks for names argument
  if(!(is.list(nameList) && length(nameList) > 1)) stop("nameList needs to be a list at least of length 2.")
  if(is.null(names(nameList)) || any(names(nameList) == "")) stop("All elements of nameList must be named.")
  if(any(!unlist(nameList) %in% names(GADSdat[["dat"]]))) stop("All names used in nameList vectors have to be names in the GADSdat.")

  # 2) split GADS up
  dat_list <- lapply(nameList, function(x) GADSdat[["dat"]][, x])
  dat_list <- Map(drop_duplicates, df = dat_list, df_name = names(dat_list))

  # 3) add data frame identifier (and formatting for equality)
  ref_list <- lapply(names(nameList), FUN = function (x) data.frame(data_table = x, varName = nameList[[x]], stringsAsFactors = FALSE))
  ref_df <- do.call("rbind", ref_list)
  label_df <- merge(GADSdat[["labels"]], ref_df, by = "varName", all = FALSE)
  label_df <- do.call("rbind", lapply(names(nameList), function(df_name) label_df[label_df[, "data_table"] == df_name, ]))
  rownames(label_df) <- NULL

  # output: list of data frames ready for data base creation, and data frame with labels
  new_all_GADSdat(datList = dat_list, allLabels = label_df)
}

# remove duplicates (when original data is in long format); remove rownames for equality
drop_duplicates <- function(df, df_name = "data.frame") {
  out <- unique(df)
  if(nrow(df) > nrow(out)) message("Rows have been dropped from ", df_name)
  rownames(out) <- NULL
  out
}





