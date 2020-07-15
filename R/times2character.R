

# 02.2.a) attributes on variable level ---------------------------------------------------
times2character <- function(rawDat) {
  UseMethod("times2character")
}


## Function works for labelled and unlabelled time and date variables (which are handeled very inconsistently by haven under user_na = T)
#'@export
times2character.savDat<- function(rawDat) {
  varClass <- unlist(lapply(rawDat, extract_attribute, attr_name = "format.spss"))

  hms_vars <- names(varClass)[grepl("^TIME", varClass)]
  for(hms_var in hms_vars) {
    old_attributes <- attributes(rawDat[[hms_var]])
    new_attributes <- old_attributes

    #if(hms_var == "VAR2") browser()
    new_var_zapped <- haven::zap_labels(rawDat[[hms_var]])
    new_var <- as.character(hms::as_hms(as.numeric(new_var_zapped)))

    ## Conserving labels + missing codes (deprecated)
    #if(!is.null(old_attributes[["labels"]])) new_attributes[["labels"]] <- as.character(hms::as_hms(unname(old_attributes[["labels"]])))
    #names(new_attributes[["labels"]]) <- names(old_attributes[["labels"]])
    #if(!is.null(old_attributes[["na_values"]])) new_attributes[["na_values"]] <- as.character(hms::as_hms(unname(old_attributes[["na_values"]])))

    if(!is.null(new_attributes[["labels"]]) || !is.null(new_attributes[["na_values"]])) {
      warning("Value labels and missing codes for 'TIMES' variables are not supported by eatGADS. Missing values are converted to NA and labels and missing codes are dropped from meta data for variable ", hms_var)
    }
    new_attributes[["labels"]] <- new_attributes[["na_values"]] <- NULL

    new_attributes[["format.spss"]] <- "A8"
    new_attributes[["units"]] <- NULL
    #if(is.null(old_attributes[["labels"]])) browser()
    if(is.null(new_attributes[["labels"]])) {
      new_attributes[["class"]] <- NULL
    } else {
      if(!any(grepl("haven_labelled", new_attributes[["class"]]))) new_attributes[["class"]] <-  "haven_labelled"
    }

    rawDat[[hms_var]] <- new_var
    attributes(rawDat[[hms_var]]) <- new_attributes
  }

  date_vars <- names(varClass)[grepl("^DATE", varClass)]
  for(date_var in date_vars) {
    old_attributes <- attributes(rawDat[[date_var]])
    new_attributes <- old_attributes

    #if("haven_labelled" %in% old_attributes[["class"]]) stop("Labelled dates are currently not supported by eatGADS.")
    if("haven_labelled" %in% old_attributes[["class"]]) {
      warning("Value labels and missing codes for 'DATE' variables are not supported by eatGADS and current implementation is experimental. Missing values are converted to NA and labels and missing codes are dropped from meta data for variable ", date_var)
      na_values <- as.Date(as.numeric(new_attributes$na_values)/86400, origin = "1582-10-14") ## label conversion
      new_var <- as.Date(haven::zap_labels(rawDat[[date_var]]), origin = "1970-01-01") ## data conversion
      new_var[new_var %in% na_values] <- NA
      new_var <- as.character(new_var)
      new_attributes[["labels"]] <- new_attributes[["na_values"]] <- NULL
    } else {
      new_var <- as.character(rawDat[[date_var]])
    }
    new_attributes[["format.spss"]] <- gsub("^DATE", replacement = "A", new_attributes[["format.spss"]])
    new_attributes[["class"]] <- NULL

    rawDat[[date_var]] <- new_var
    attributes(rawDat[[date_var]]) <- new_attributes
  }

  rawDat
}

#'@export
times2character.data.frame <- function(rawDat) {
  varClass <- unlist(lapply(rawDat, extract_attribute, attr_name = "class"))
  if(any(grepl("POSIX", varClass))) stop("POSIXct and POSIXlt are currently not supported by eatGADS.")
  rawDat
}


### set missings to NA
### remove value labels
### issue warning
