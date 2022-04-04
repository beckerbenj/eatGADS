#### Reuse Meta
#############################################################################
#' Use meta data for a variable from another \code{GADSdat}.
#'
#' Transfer meta information from one \code{GADSdat} to another.
#'
#' Transfer of meta information can mean substituting the complete meta information, only adding value labels, adding only
#' \code{"valid"} or adding only \code{"miss"} missing labels.
#' See the arguments \code{missingLabels} and \code{addValueLabels} for further information.
#'
#'@param GADSdat \code{GADSdat} object imported via \code{eatGADS}.
#'@param varName Name of the variable that should get the new meta data.
#'@param other_GADSdat \code{GADSdat} object imported via \code{eatGADS} including the desired meta information. Can also be a GADS db or an \code{all_GADSdat} object.
#'@param other_varName Name of the variable that should get the new meta data in the \code{other_GADSdat}.
#'@param missingLabels How should meta data for missing values be treated? If \code{NULL}, missing values are transferred as all other labels. If \code{"drop"}, missing labels are dropped (useful for imputed data). If \code{"leave"}, missing labels remain untouched. If \code{"only"}, all valid value labels are dropped.
#'@param addValueLabels Should only value labels be added and all other meta information retained?
#'
#'@return Returns the original object with updated meta data.
#'
#'@examples
#'# see createGADS vignette
#'
#'@export
reuseMeta <- function(GADSdat, varName, other_GADSdat, other_varName = NULL, missingLabels = NULL, addValueLabels = FALSE) {
  UseMethod("reuseMeta")
}
#'@export
reuseMeta.GADSdat <- function(GADSdat, varName, other_GADSdat, other_varName = NULL, missingLabels = NULL, addValueLabels = FALSE) {
  if(!is.null(missingLabels) && !missingLabels %in% c("drop", "leave", "only")) stop("Invalid input for argument missingLabels.")
  if(!varName %in% names(GADSdat$dat)) stop("varName is not a variable in the GADSdat.")
  # extract meta data
  if(is.null(other_varName)) other_varName <- varName
  new_meta <- extractMeta(other_GADSdat, other_varName)
  # compatability with meta data from all_GADSdat or data base (variable can be foreign key and occur multiply)
  if("data_table" %in% names(new_meta)) {
    first_data_table <- new_meta[1, "data_table"]
    new_meta <- new_meta[new_meta$data_table == first_data_table, ]
    new_meta <- new_meta[, names(new_meta) != "data_table"]
  }
  new_meta[, "varName"] <- varName
  # If value labels are added (via addValueLabels = TRUE or missingLabels = "leave"), make meta information on variable level compatible
  if(addValueLabels || identical(missingLabels, "leave")) {
    for(i in c("varLabel", "format", "display_width")) {
      new_meta[, i] <- GADSdat$labels[GADSdat$labels$varName == varName, i][1]
    }
  }

  remove_rows <- which(GADSdat$labels$varName == varName)

  # special missing value labels treatment
  if(identical(missingLabels, "drop")) new_meta <- drop_missing_labels(new_meta)
  if(identical(missingLabels, "only")) new_meta <- drop_valid_labels(new_meta)
  if(identical(missingLabels, "leave")) {
    new_meta <- drop_missing_labels(new_meta)
    remove_rows <- which(GADSdat$labels$varName == varName & GADSdat$labels$missings != "miss")
    if(identical(new_meta$labeled, "no")) new_meta <- new_meta[-1, ]
  }

  # only not remove rows, if already labeled (to prevent empty row)
  if(addValueLabels && GADSdat$labels[remove_rows, "labeled"][1] != "no") {
    remove_rows <- numeric()
  }
  # change to labeled in new meta, if labels are added!
  if((addValueLabels || identical(missingLabels, "leave")) && nrow(new_meta) > 0 && new_meta[1, "labeled"] == "yes") {
    GADSdat$labels[GADSdat$labels$varName == varName, "labeled"] <- "yes"
  }

  # insert new meta information, remove old, sort
  labels <- GADSdat$labels
  if(length(remove_rows) > 0) labels <- labels[-remove_rows, ]
  labels <- rbind(labels, new_meta)
  labels <- labels[order(match(labels$varName,names(GADSdat$dat))), ]
  row.names(labels) <- NULL

  out <- new_GADSdat(dat = GADSdat$dat, labels = labels)
  check_GADSdat(out)
  out
}

# drop missing value labels from meta data for a single variable
drop_missing_labels <- function(meta) {
  if(length(unique(meta$varName)) != 1) stop("This function only works for meta information of a single variable.")
  meta_new <- meta[which(meta$missings == "valid"), ]
  if(nrow(meta_new) == 0) {
    meta_new <- meta[1, ]
    meta_new$missings <- meta_new$valLabel <- NA_character_
    meta_new$value <- NA_integer_
    meta_new$labeled <- "no"
  }
  row.names(meta_new) <- NULL
  meta_new
}

drop_valid_labels <- function(meta) {
  if(length(unique(meta$varName)) != 1) stop("This function only works for meta information of a single variable.")
  meta_new <- meta[which(meta$missings == "miss"), ]
  if(nrow(meta_new) == 0) {
    meta_new <- meta[1, ]
    meta_new$missings <- meta_new$valLabel <- NA_character_
    meta_new$value <- NA_integer_
    meta_new$labeled <- "no"
  }
  row.names(meta_new) <- NULL
  meta_new
}
