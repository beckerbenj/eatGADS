
#### Split up a wide/long format GADS in hierarchical levels
#############################################################################
#' Split \code{GADSdat} into hierarchy levels.
#'
#' Split a \code{GADSdat} into multiple, specified hierarchical levels.
#'
#' The function takes a \code{GADSdat} object and splits it into its desired hierarchical levels (a \code{all_GADSdat} object).
#' Hierarchy level of a variable is also accessible in the meta data via the column \code{data_table}. If not all variable names
#' are included in the \code{nameList}, the missing variables will be dropped.
#'
#'@param GADSdat A \code{GADSdat} object.
#'@param nameList A list of character vectors. The names in the list correspond the the hierarchy levels.
#'
#'@return Returns an \code{all_GADSdat} object, which consists of list with a list of all data frames \code{"datList"} and
#'a single data frame containing all meta data information \code{"allLabels"}. For more details see also \code{\link{mergeLabels}}.
#'
#'@examples
#'# see createGADS vignette
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





