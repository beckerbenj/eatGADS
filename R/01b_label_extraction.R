
# 02.2.a) attributes on variable level ---------------------------------------------------
extract_variable_level <- function(rawDat) {
  UseMethod("extract_variable_level")
}

extract_variable_level.savDat<- function(rawDat) {
  # check for unknown attributes (mostly to secure against changes in haven)
  all_attr <- unlist(lapply(rawDat, function(var) names(attributes(var))))
  unknown_attr <- all_attr[!all_attr %in% c("label", "format.spss", "display_width", "class", "labels", "na_range", "na_values")]
  if(length(unknown_attr) > 0) stop("Unknown attributes exported from haven:", unknown_attr, ". Please contact package author.")

  varLabels <- unlist(lapply(rawDat, extract_attribute, attr_name = "label"))
  varFormat <- unlist(lapply(rawDat, extract_attribute, attr_name = "format.spss"))
  varWidth <- unlist(lapply(rawDat, extract_attribute, attr_name = "display_width", NA_type = NA_real_))
  varClass <- unlist(lapply(rawDat, extract_attribute, attr_name = "class"))
  varLabel_df <- data.frame(names(rawDat), varLabels, varFormat, varWidth, varClass, stringsAsFactors = FALSE)
  # names
  names(varLabel_df) <- c("varName", "varLabel", "format", "display_width", "class")
  rownames(varLabel_df) <- NULL

  varLabel_df
}

extract_variable_level.data.frame <- function(rawDat) {
  # is factor variable to add?
  data.frame(varName = names(rawDat))
}


# extract attributes and produce NA for not given attributes
extract_attribute <- function(var, attr_name, NA_type = NA_character_) {
  out <- attr(var, attr_name)
  if(is.null(out)) out <- NA_type
  if(length(out) > 1) out <- paste(out, collapse = ", ")
  out
}


# 02.2.b) attributes on value level ---------------------------------------------------
# all variables, for SPSS and R
call_extract_values <- function(rawDat, labeledStrings) {
  # extract labels into one long format data frame
  valueList <- Map(extract_value_level, var = rawDat, varName = names(rawDat), labeledStrings = labeledStrings)
  valLabel_df <- do.call(rbind, valueList)
  rownames(valLabel_df) <- NULL
  valLabel_df
}

extract_value_level <- function(var, varName, labeledStrings) {
  UseMethod("extract_value_level")
}

extract_value_level.default <- function(var, varName, labeledStrings) {
  NULL
}

# single variable for R (factors!)
extract_value_level.factor <- function(var, varName, labeledStrings) {
  df <- data.frame(varName = rep(varName, length(levels(var))),
                   value = seq_along(labels),
                   valLabel = levels(var),
                   missings = NA_character_,
                   stringsAsFactors = FALSE)
  ### insert missing extraction, add col like in spss function
  rownames(df) <- NULL
  df
}

# single variable for SPSS
extract_value_level.haven_labelled <- function(var, varName, labeledStrings = FALSE) {
  # check if there are value labels
  if(is.null(attributes(var)$labels)) return(NULL)
  # default behavior: transform value labels to numeric if possible, change values to NA for string values
  values <- attr(var, "labels")
  if(identical(labeledStrings, FALSE)) {
    values <- tryCatch(as.numeric(values), warning = function(w) {
      warning("Some or all values for ", varName, " cannot be coerced to numeric and are therefore changed to NA. \n", call. = FALSE)
      structure(suppressWarnings(as.numeric(values)), condition = "warning")
    })
  }
  # extract value labels and return as long format df
  df <- data.frame(varName = rep(varName, length(values)),
                   value = values,
                   valLabel = attr(attr(var, "labels"), "names"),
                   missings = NA,
                   stringsAsFactors = FALSE)

  ## extract missings and add as extra label
  df <- extract_Miss_SPSS(var = var, label_df = df)

  rownames(df) <- NULL
  df
}

# extract if label is label for missing values
extract_Miss_SPSS <- function(var, label_df) {
  na_range <- attr(var, "na_range")
  na_value <- attr(var, "na_value")
  # check if any na_value without this label (check not performed for na_range!)
  lapply(na_value, function(val) {
    if(!val %in% label_df$value) {
      warning(val, " used as missing label for variable ", label_df$varName[1], " but no value label given. Label is dropped.")
    }})

  # add variable indicating missings
  if(!is.null(na_range) || !is.null(na_value)) {
    # give values for ifelse
    if(is.null(na_range)) na_range <- c(Inf, -Inf)
    if(is.null(na_value)) na_value <- numeric(0)
    label_df$missings <- ifelse(label_df$value >= na_range[1] & label_df$value <= na_range[2] |
                                  label_df$value %in% na_value, "miss", NA)
  }
  rownames(label_df) <- NULL
  label_df
}


