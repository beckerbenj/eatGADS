
#### Writing sav files
#############################################################################
#' Write data frame to sav file
#'
#' Function to write data frame with labels for values and variables to SPSS-file.
#'
#' ...
#'
#'@param filePath Path of the existing db file.

#'@return Returns a data frame.
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
  # variable labels
  out[["label"]] <- unique(label_df[, "varLabel"])
  stopifnot(length(out[["label"]]) == 1)

  # variable format, display width
  out[["format.spss"]] <- unique(label_df[, "format"])
  out[["display_width"]] <- unique(label_df[, "display_width"])

  # missing labels, if any
  miss_values <- label_df[which(label_df$missings == "miss"), "value"]
  if(length(miss_values) > 0)  out[["na_values"]] <- miss_values

  # class
  # out[["class"]] <- c("labelled_spss", "labelled")
  if(unique(label_df[, "class"]) == "haven_labelled")  out[["class"]] <- c("haven_labelled")

  # value labels, if any
  value_label_df <- label_df[!is.na(label_df$value), ]
  if(nrow(value_label_df) > 0) {
    out[["labels"]] <- value_label_df[, "value"]
    names(out[["labels"]]) <- value_label_df[, "label"]
  }
  #if(any(label_df$varName == "TESTUNG")) browser()
  out
}




