#### Checks structure of trend gads
#############################################################################
#' Checks compatibility of two \code{eatGADS} data bases.
#'
#' This function checks if both data bases perform identical joins via foreign keys, if they contain the same variable names and if
#' these variables have the same value labels. Results of this comparison are reported on data table level as messages and as an output list.
#'
#' An error is thrown if the key structure or the data table structure differs between the two data bases. Differences regarding
#' meta data for missing value labels and for variables labels (and formatting) are ignored.
#'
#' Reported differences regarding meta data can be inspected further via \code{\link{inspectMetaDifferences}}.
#'
#'@param filePath1 Path of the first \code{eatGADS} \code{.db} file.
#'@param filePath2 Path of the second \code{eatGADS} \code{.db} file.
#'
#'@return Returns a report list.
#'
#'
#'@export
checkTrendStructure <- function(filePath1, filePath2) {
  check_keyStrcuture_TrendGADS(filePath1, filePath2)
  # Variables
  n1 <- namesGADS(filePath1)
  n2 <- namesGADS(filePath2)

  var_comp <- lapply(names(n1), function(dt_name) {
    message("Checking names for data table ", dt_name, "...")
    compare_and_order(set1 = n1[[dt_name]], set2 = n2[[dt_name]], name1 = "data base 1", name2 = "data base 2", FUN = message)
  })
  names(var_comp) <- names(n1)
  # Meta Data
  meta1 <- extractMeta(filePath1)
  meta2 <- extractMeta(filePath2)

  meta_comp <- lapply(names(n1), function(dt_name) {
    # if(dt_name == "NoImp") browser()
    meta_single1 <- meta1[meta1$data_table == dt_name & meta1$varName %in% var_comp[[dt_name]][["in_both_ordered"]], ]
    meta_single2 <- meta2[meta2$data_table == dt_name & meta2$varName %in% var_comp[[dt_name]][["in_both_ordered"]], ]
    message("Checking meta data for data table ", dt_name, "...")
    compare_meta(meta1 = meta_single1, meta2 = meta_single2)
  })
  names(meta_comp) <- names(n1)

  list("Variable Comparison" = var_comp, "Meta Data Comparison" = meta_comp)
}

## Compare meta data
compare_meta <- function(meta1, meta2) {
  diff_in_meta <- character()
  for(nam in unique(meta1$varName)) {
    var_meta1 <- meta1[meta1$varName == nam, c("value", "valLabel", "missings")]
    var_meta2 <- meta2[meta2$varName == nam, c("value", "valLabel", "missings")]

    # eliminate all possible irrelevant causes for inequality
    var_meta1 <- var_meta1[order(var_meta1$value), ]
    var_meta2 <- var_meta2[order(var_meta2$value), ]
    var_meta1 <- var_meta1[var_meta1$missings == "valid" | is.na(var_meta1$missings), ]
    var_meta2 <- var_meta2[var_meta2$missings == "valid" | is.na(var_meta2$missings), ]
    row.names(var_meta1) <- row.names(var_meta2) <- NULL

    # treat unlabeled variables as no value labels given
    if(nrow(var_meta1) == 1 && all(is.na(var_meta1[1, ]))) var_meta1 <- var_meta1[-1, ]
    if(nrow(var_meta2) == 1 && all(is.na(var_meta2[1, ]))) var_meta2 <- var_meta2[-1, ]

    test_eq <- all.equal(var_meta1, var_meta2)
    if(!identical(test_eq, TRUE)) diff_in_meta <- c(diff_in_meta, nam)
  }
  if(length(diff_in_meta) > 0 ) message("The following variables have different meta data on value level: ", paste(diff_in_meta, collapse = ", "))
  diff_in_meta
}
