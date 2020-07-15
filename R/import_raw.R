
#### Import R-data with explicit metadata
#############################################################################
#' Import R data frame with explicit meta data sheets
#'
#' Function to import a \code{data.frame} object for use in \code{eatGADS} while adding explicit variable and value meta information through
#' separate \code{data.frames}.
#'
#' The argument \code{varLables} has to contain exactly two variables, namely \code{varName} and \code{varLabel}. \code{valLables} has
#' to contain exactly four variables, namely \code{varName}, \code{value}, \code{valLabel} and \code{missings}. The column \code{value}
#' can only contain numerical values. The column \code{missings} can only contain the values \code{"valid"} and \code{"miss"}.
#' Variables of type \code{factor} are not supported in any of the \code{data.frames}.
#'
#'@param df A \code{data.frame}.
#'@param varLabels A \code{data.frame} containing the variable labels. All variables in the data have to have exactly one column in this data.frame.
#'@param valLabels A \code{data.frame} containing the value labels. All referenced variables have to appear in the data, but not all variables in the data have to receive value labels. Can be omitted.
#'@param checkVarNames Should variable names be checked for violations of \code{SQLite} and \code{R} naming rules?
#'
#'@return Returns a list with the actual data \code{dat} and with all meta information in long format \code{labels}.
#'
#'@examples
#'dat <- data.frame(ID = 1:5, grade = c(1, 1, 2, 3, 1))
#'varLabels <- data.frame(varName = c("ID", "grade"),
#'                        varLabel = c("Person Identifier", "School grade Math"),
#'                        stringsAsFactors = FALSE)
#'valLabels <- data.frame(varName = c("grade", "grade", "grade"),
#'                        value = c(1, 2, 3),
#'                        valLabel = c("very good", "good", "sufficient"),
#'                        missings = c("valid", "valid", "valid"),
#'                        stringsAsFactors = FALSE)
#'
#'gads <- import_raw(df = dat, varLabels = varLabels, valLabels = valLabels, checkVarNames = FALSE)
#'
#'# Inspect Meta data
#'extractMeta(gads)
#'
#'# Extract Data
#'dat <- extractData(gads, convertLabels = "character")
#'
#'@export
import_raw <- function(df, varLabels, valLabels = NULL, checkVarNames = TRUE) {
  if(!is.data.frame(df)) stop("df needs to be a data frame.")
  if(any(sapply(df, is.factor))) stop("At least one of the variables in df is a factor. All meta information on value level has to be stored in valLabels.")

  ## data import
  GADS_raw <- prepare_labels(rawDat = df, checkVarNames = checkVarNames, labeledStrings = FALSE) ## use import_df instead?

  #### varLabels
  check_varLabels(df = df, varLabels = varLabels)
  change_var <- getChangeMeta(GADS_raw, level = "variable")
  names(varLabels)[names(varLabels) == "varLabel"] <- "varLabel_new"
  change_var <- merge(change_var[, !names(change_var) %in% "varLabel_new"], varLabels, all = FALSE, sort = FALSE)
  change_var <- new_varChanges(change_var)
  GADS_raw2 <- applyChangeMeta(change_var, GADS_raw)

  #### valLabels (optional)
  if(!is.null(valLabels)) {
    check_valLabels(df = df, valLabels = valLabels)
    change_val_ori <- getChangeMeta(GADS_raw2, level = "value")
    variables_val_changes <- compare_and_order(change_val_ori$varName, valLabels$varName, FUN = paste) ## paste to suppress warning, maybe later as informative message?
    change_val <- change_val_ori[!change_val_ori$varName %in% variables_val_changes$in_both_ordered, ]
    names(valLabels)[names(valLabels) == "value"] <- "value_new"
    names(valLabels)[names(valLabels) == "valLabel"] <- "valLabel_new"
    names(valLabels)[names(valLabels) == "missings"] <- "missings_new"
    valLabels[, c("value", "valLabel", "missings")] <- NA
    change_val_new <- rbind(change_val, valLabels)
    change_val_new <- change_val_new[order(match(change_val_new$varName, unique(change_val_ori$varName))),
                                     match(names(change_val_new), names(change_val_ori))]
    change_val_new <- new_valChanges(change_val_new)
    GADS_raw2 <- applyChangeMeta(change_val_new, GADS_raw2)
  }

  GADS_raw2
}

# Check functions for import_raw
check_varLabels <- function(df, varLabels) {
  if(any(sapply(varLabels, is.factor))) stop("One of the variables in varLabels is a factor.")
  if(!is.data.frame(varLabels)) stop("varLabels has to be a data.frame.")
  if(!identical(names(varLabels), c("varName", "varLabel"))) stop("varLabels needs to contain the variables 'varName' and 'varLabel'.")

  compare_and_order(set1 = names(df), set2 = varLabels[["varName"]], name1 = "the data df", name2 = "varLabels", FUN = stop)
  dup_names <- varLabels[["varName"]][duplicated(varLabels[["varName"]])]
  if(length(dup_names) > 0) stop("The following variables have duplicated rows in varLabels: ", paste(dup_names, collapse = ", "))
}

check_valLabels <- function(df, valLabels) {
  if(any(sapply(valLabels, is.factor))) stop("One of the variables in valLabels is a factor.")
  if(!is.data.frame(valLabels)) stop("valLabels has to be a data.frame.")
  if(!identical(names(valLabels), c("varName", "value", "valLabel", "missings"))) stop("valLabels needs to contain the variables 'varName', 'value', 'valLabel' and 'missings'.")

  not_in_df <- setdiff(valLabels[["varName"]], names(df))
  if(length(not_in_df) > 0) stop("The following variables are not in the data df: ", paste(not_in_df, collapse = ", "))
  if(!is.numeric(valLabels$value)) stop("Value column of valLabels has to be numeric.")
  if(!all(valLabels$missings %in% c("valid", "miss"))) stop("All values in column 'missings' of valLabels must be either 'valid' or 'miss'.")
}
