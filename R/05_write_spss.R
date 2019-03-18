
#### Writing sav files
#############################################################################
#' Write data frame to sav file
#'
#' Preliminary Version! Function to write data frame with labels for values and variables to SPSS-file.
#'
#' ...
#'
#'@param GADSdat Data frame.
#'@param filePath Path of sav file to write.
#'
#'@return Writes sav-file, returns nothing.
#'
#'@examples
#'# tbd
#'
#'@export
write_spss <- function(GADSdat, filePath) {
  UseMethod("write_spss")
}

#'@export
write_spss.GADSdat <- function(GADSdat, filePath) {
  # 1) check input
  check_GADSdat(GADSdat)
  # currently no checks

  # 2) add labels to df
  df <- addLabels(df = GADSdat$dat, label_df = GADSdat$labels)
  #browser()
  # 3) write spss-file
  haven::write_sav(df, path = filePath)

  return()
}

###  add labels all variables   ---------------------------------------------------------
addLabels <- function(df, label_df) {
  for(n in names(df)) {
    single_label_df <- label_df[label_df$varName == n, ]
    # add labels if any rows in label data frame
    if(nrow(single_label_df) > 0) {
      attributes(df[, n]) <- addLabels_single(label_df = single_label_df)
    }
  }
  df
}

###  add labels to a single variable ---------------------------------------------------------
addLabels_single <- function(label_df) {
  out <- list()
  # attributes on variable level
  out[["label"]] <- unique(label_df[, "varLabel"])
  out[["format.spss"]] <- unique(label_df[, "format"])
  out[["display_width"]] <- unique(label_df[, "display_width"])
  labeled <- unique(label_df[, "labeled"])
  # check
  unique_attr <- unlist(lapply(out, length))
  stopifnot(all(unique_attr)  <= 1)
  # out[["class"]] <- strsplit(out[["class"]], split = ", ")[[1]]
  if(identical(labeled, "yes")) out[["class"]] <- c("haven_labelled_spss", "haven_labelled")
  if(identical(labeled, "no")) out[["class"]] <- NA_character_

  # missing labels, if any
  miss_values <- label_df[which(label_df$missings == "miss"), "value"]
  if(length(miss_values) > 0)  out[["na_values"]] <- miss_values

  # value labels, if any
  value_label_df <- label_df[!is.na(label_df$value), ]
  if(nrow(value_label_df) > 0) {
    out[["labels"]] <- value_label_df[, "value"]
    names(out[["labels"]]) <- value_label_df[, "valLabel"]
  }
  #if(any(label_df$varName == "TESTUNG")) browser()
  out
}




