
#### Export to haven format
#############################################################################
#' Transform a \code{GADSdat} to a \code{tibble}
#'
#' \code{haven}'s \code{\link[haven]{read_spss}} stores data together with meta data (e.g. value and variable labels) in a
#' \code{tibble} with attributes on variable level. This function transforms a \code{GADSdat} object to such a \code{tibble}.
#'
#' This function is mainly intended for internal use.
#'
#'@param GADSdat \code{GADSdat} object imported via \code{eatGADS}.
#'
#'@return Returns a \code{tibble}.
#'
#'@examples
#'pisa_tbl <- export_tibble(pisa)
#'
#'@export
export_tibble <- function(GADSdat) {
  UseMethod("export_tibble")
}

#'@export
export_tibble.GADSdat <- function(GADSdat) {
  # 1) check input
  check_GADSdat(GADSdat)
  # currently no further checks (depends on haven behaviour if this were necessary)
  check_var_type(GADSdat)

  df <- GADSdat$dat
  label_df <- GADSdat$labels
  for(n in names(df)) {
    single_label_df <- label_df[label_df$varName == n, ]
    # add labels if any rows in label data frame
    if(nrow(single_label_df) > 0) {
      attributes(df[, n]) <- addLabels_single(label_df = single_label_df, varClass = class(df[[n]]))
    }
  }
  tibble::as_tibble(df)
}

check_var_type <- function(GADSdat) {
  for(varName in namesGADS(GADSdat)) {
    format.spss <- GADSdat$labels[GADSdat$labels$varName == varName, "format"][1]
    class_var <- class(GADSdat$dat[, varName])
    if(!is.na(format.spss) && grepl("^A", format.spss) && class_var == "numeric" ) stop("Incompatible R variable type and format.spss for variable ", varName)
    if(!is.na(format.spss) && grepl("^F", format.spss) && class_var == "character" ) stop("Incompatible R variable type and format.spss for variable ", varName)
  }
  return()
}


###  add labels to a single variable ---------------------------------------------------------
addLabels_single <- function(label_df, varClass) {
  out <- list()
  # attributes on variable level
  if(!all(is.na(label_df[, "varLabel"]))) out[["label"]] <- unique(label_df[, "varLabel"])
  if(!all(is.na(label_df[, "format"]))) out[["format.spss"]] <- unique(label_df[, "format"])
  if(!all(is.na(label_df[, "display_width"]))) out[["display_width"]] <- unique(label_df[, "display_width"])
  labeled <- unique(label_df[, "labeled"])
  # check
  unique_attr <- unlist(lapply(out, length))
  stopifnot(all(unique_attr)  <= 1)

  # missing labels, if any
  miss_values <- label_df[which(label_df$missings == "miss"), "value"]
  if(length(miss_values) > 0 && length(miss_values) <= 3)  out[["na_values"]] <- miss_values
  if(length(miss_values) > 3)  out[["na_range"]] <- range(miss_values)

  # out[["class"]] <- strsplit(out[["class"]], split = ", ")[[1]]
  # give specific class depending on a) value labels are needed or not and b) missings are given or not:
  if(identical(labeled, "yes")) out[["class"]] <- c("haven_labelled")
  if(identical(labeled, "yes") & all(is.na(label_df$value))) out[["class"]] <- c("haven_labelled_spss")
  any_miss <- length(miss_values) > 0
  if(identical(out[["class"]], "haven_labelled") && any_miss) out[["class"]] <- c("haven_labelled_spss", "haven_labelled")

  # experimental class modification (due to haven 2.3.0 classes got modified)
  #stopifnot(varClass %in% c("numeric", "character"))
  #out[["class"]] <- c(out[["class"]], "vctrs_vctr", ifelse(varClass == "numeric", yes = "double", no = "character"))

  # value labels, if any
  value_label_df <- label_df[!is.na(label_df$value), ]
  if(nrow(value_label_df) > 0) {
    out[["labels"]] <- value_label_df[, "value"]
    names(out[["labels"]]) <- value_label_df[, "valLabel"]
  }

  # value labels and missing codes need to have the same class as the variable format
  #if(all(label_df$varName == "groupVar")) browser()
  if((length(out[["format.spss"]]) > 0 && grepl("^A", unique(out[["format.spss"]]))) || identical(varClass, "character")) {
    #if((length(out[["format.spss"]]) > 0 && grepl("^A", unique(out[["format.spss"]])))) {
    out[["na_values"]] <- as.character(out[["na_values"]])
    if(nrow(value_label_df) > 0) {
      out[["labels"]] <- as.character(out[["labels"]])
      names(out[["labels"]]) <- value_label_df[, "valLabel"]
    }
    #if(identical(labeled, "yes")) out[["class"]] <- c("haven_labelled")
  }

  #if(any(label_df$varName == "TESTUNG")) browser()
  out
}

