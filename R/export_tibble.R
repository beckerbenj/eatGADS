
#### Export to haven format
#############################################################################
#' Transform a \code{GADSdat} to a \code{tibble}
#'
#' \code{haven}'s \code{\link[haven]{read_spss}} stores data together with meta data (e.g. value and variable labels) in a
#' \code{tibble} with attributes on variable level. This function transforms a \code{GADSdat} object to such a \code{tibble}.
#'
#' This function is mainly intended for internal use. For further documentation see also \code{\link{write_spss}}.
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
      attributes(df[, n]) <- addLabels_single(varName = n, label_df = single_label_df, raw_dat = df[[n]])
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
addLabels_single <- function(varName, label_df, raw_dat) {
  attr_list <- list()
  varClass <- class(raw_dat)
  # attributes on variable level
  if(!all(is.na(label_df[, "varLabel"]))) attr_list[["label"]] <- unique(label_df[, "varLabel"])
  if(!all(is.na(label_df[, "format"]))) attr_list[["format.spss"]] <- unique(label_df[, "format"])
  if(!all(is.na(label_df[, "display_width"]))) attr_list[["display_width"]] <- unique(label_df[, "display_width"])
  labeled <- unique(label_df[, "labeled"])
  # check
  unique_attr <- unlist(lapply(attr_list, length))
  stopifnot(all(unique_attr)  <= 1)

  # missing labels, if any
  attr_list <- add_miss_tags(varName = varName, attr_list = attr_list, label_df = label_df, raw_dat = raw_dat)

  # give specific class depending on a) value labels are needed or not and b) missings are given or not:
  any_miss <- !is.null(attr_list$na_values) || !is.null(attr_list$na_range)

  if(identical(labeled, "yes")) {
    attr_list[["class"]] <- c("haven_labelled")
    if(all(is.na(label_df$value))) {
      attr_list[["class"]] <- c("haven_labelled_spss")
    } else if(any_miss){
      attr_list[["class"]] <- c("haven_labelled_spss", "haven_labelled")
    }
  }

  # experimental class modification (due to haven 2.3.0 classes got modified)
  #stopifnot(varClass %in% c("numeric", "character"))
  #attr_list[["class"]] <- c(attr_list[["class"]], "vctrs_vctr", ifelse(varClass == "numeric", yes = "double", no = "character"))

  # value labels, if any
  value_label_df <- label_df[!is.na(label_df$value), ]
  if(nrow(value_label_df) > 0) {
    attr_list[["labels"]] <- value_label_df[, "value"]
    names(attr_list[["labels"]]) <- value_label_df[, "valLabel"]
  }

  # value labels and missing codes need to have the same class as the variable format
  if((length(attr_list[["format.spss"]]) > 0 &&
      grepl("^A", unique(attr_list[["format.spss"]]))) ||
     identical(varClass, "character")) {
    attr_list[["na_values"]] <- as.character(attr_list[["na_values"]])
    if(nrow(value_label_df) > 0) {
      attr_list[["labels"]] <- as.character(attr_list[["labels"]])
      names(attr_list[["labels"]]) <- value_label_df[, "valLabel"]
    }
    #if(identical(labeled, "yes")) attr_list[["class"]] <- c("haven_labelled")
  }

  attr_list
}

# missing tag conversion to suit SPSS style (maximum 3 discrete missing tags or range of missing tags)
add_miss_tags <- function(varName, attr_list, label_df, raw_dat) {
  miss_values <- label_df[which(label_df$missings == "miss"), "value"]

  if(length(miss_values) > 0){
    # compatibel case
    if(length(miss_values) <= 3)  {
      attr_list[["na_values"]] <- miss_values
    # incompatibel case
    } else {
      full_range <- range(miss_values)
      # check variable class
      if(is.character(raw_dat)) {
        stop("Conversion of missing tags for variable '", varName,
             "' to SPSS conventions is not possible, as too many missings are declared for a character variable (maximum 3). Adjust missing tags to export to tibble or write to SPSS.")
      }
      # check other labels
      non_miss_labels <- label_df[is.na(label_df$missings) | label_df$missings == "valid", "value"]
      accidental_miss_tags_labels <- non_miss_labels[!is.na(non_miss_labels) &
                                                       non_miss_labels >= full_range[1] & non_miss_labels <= full_range[2]]
      if(length(accidental_miss_tags_labels) > 0) {
        stop("Conversion of missing tags for variable '", varName,
             "' to SPSS conventions is not possible, as the new missing range (",
             full_range[1], " to ", full_range[2],
             ") would include the following labeled values not tagged as missing: ",
             paste(accidental_miss_tags_labels, collapse = ", "),
             ". Adjust missing tags to export to tibble or write to SPSS.")
      }

      # check actual data
      other_raw_dat <- raw_dat[!raw_dat %in% miss_values]
      accidental_miss_tags_data <- other_raw_dat[!is.na(other_raw_dat) &
                                                   other_raw_dat >= full_range[1] & other_raw_dat <= full_range[2]]
      if(length(accidental_miss_tags_data) > 0) {
        stop("Conversion of missing tags for variable '", varName,
             "' to SPSS conventions is not possible, as the new missing range (",
             full_range[1], " to ", full_range[2],
             ") would include the following values not tagged as missing in the data: ",
             paste(accidental_miss_tags_data, collapse = ", "),
             ". Adjust missing tags to export to tibble or write to SPSS.")
      }

      attr_list[["na_range"]] <- full_range
    }
  }



  attr_list
}
