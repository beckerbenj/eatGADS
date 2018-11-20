
#### Writing sav files
#############################################################################
#' Write data frame to sav file
#'
#' Function to write data frame with labels for values and variables to SPSS-file.
#'
#' ...
#'
#'@param df Data frame.
#'@param filePath Path of the existing db file.
#'@param filePath Path of sav file to write.
#'
#'@return Writes sav-file, returns nothing.
#'
#'@examples
#'# # See vignette.
#'
#'@export
writeDB_SPSS <- function(df, label_df, filePath) {
  # 1) check input
  # currently no checks

  # 2) add labels to df
  df <- addLabels(df = df, label_df = label_df)
  #browser()
  # 3) write spss-file
  haven::write_sav(df, path = filePath)

  return()
}

###  add labels all variables   ---------------------------------------------------------
addLabels <- function(df, label_df) {
  for(n in names(df)) {
    single_label_df <- label_df[label_df$varName == n, ]
    #browser()
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
  out[["class"]] <- unique(label_df[, "class"])
  # check
  unique_attr <- unlist(lapply(out, length))
  stopifnot(all(unique_attr)  <= 1)
  # out[["class"]] <- strsplit(out[["class"]], split = ", ")[[1]]
  if(identical(out[["class"]], "labeled")) out[["class"]]<- c("haven_labelled_spss", "haven_labelled")

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




