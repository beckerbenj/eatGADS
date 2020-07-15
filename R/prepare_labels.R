
# 02) Prepare and extract data ---------------------------------------------------------
prepare_labels <- function(rawDat, checkVarNames, labeledStrings) {
  # 1) check and prepare variable names
  if(anyDuplicated(tolower(names(rawDat)))) names(rawDat) <- unduplicate(names(rawDat))
  if(identical(checkVarNames, TRUE)) names(rawDat) <- unlist(lapply(names(rawDat), transf_names))

  # 2) dates and times to character
  rawDat <- times2character(rawDat = rawDat)

  # 3) extract labels
  label_df <- extract_labels(rawDat = rawDat, labeledStrings = labeledStrings)

  # 3) depends on class! strip away labels from rawDat for spss, convert factors for R
  plainDat <- data.frame(lapply(rawDat, strip_attributes), stringsAsFactors = FALSE)

  # output
  new_GADSdat(dat = plainDat, labels = label_df)
}


# 02.1) Modify duplicate variable names ---------------------------------------------------------
# sqlite3 not case sensitive!
unduplicate <- function(x) {
  out <- x
  allower <- tolower(x)
  out[duplicated(allower)] <- paste(out[duplicated(allower)], 2, sep = "_")
  if(anyDuplicated(tolower(out))) out <- unduplicate(out)

  Map(function(vec_name, NewName) {
    if(!identical(NewName, vec_name)) message(paste(vec_name, "has been renamed to", NewName))
  }, vec_name = x, NewName = out)
  out
}

# 02.2) Check variable names ---------------------------------------------------------
# function for preparing of variable names (to be in line with sqlite rules)
transf_names <- function(vec_name) {
  NewName <- vec_name
  if(any(grepl(paste0("^", vec_name, "$"), eatDB::sqlite_keywords, ignore.case = TRUE))) {
    NewName <- paste0(vec_name, "Var")
  }
  NewName <- make.names(NewName)
  if(grepl("\\.", NewName))       NewName <- gsub("\\.", "_", NewName)

  if(!identical(NewName, vec_name)) message(paste(vec_name, "has been renamed to", NewName))
  NewName
}

# 02.3) extract labels ---------------------------------------------------------
extract_labels <- function(rawDat, labeledStrings) {
  attr_vec <- c("varName", "varLabel", "format", "display_width", "labeled", "value", "valLabel", "missings")

  label_df <- extract_variable_level(rawDat = rawDat)
  val_labels <- call_extract_values(rawDat = rawDat, labeledStrings = labeledStrings)

  # merge results and out with all names
  if(!is.null(val_labels)) label_df <- plyr::join(label_df, val_labels, by = "varName", type = "left", match = "all")
  add_vars <- setdiff(attr_vec, names(label_df))
  # preserve specific format of variables
  label_df[add_vars] <- NA_character_
  if(all(is.na(label_df$value))) label_df$value <- as.integer(label_df$value)
  if(all(is.na(label_df$display_width))) label_df$display_width <- as.integer(label_df$display_width)

  label_df[attr_vec]
}


# 02.3) strip away variable labels and factors ---------------------------------------------------------
strip_attributes <- function(vec) {
  attributes(vec) <- NULL
  vec
}



