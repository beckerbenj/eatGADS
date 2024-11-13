#' Drop duplicate IDs in a \code{GADSdat}.
#'
#' Drop rows with duplicate IDs in a \code{GADSdat} object based on numbers of missing values.
#'
#' If duplicate IDs occur, it is often desirable to keep the row with the least missing information.
#' Therefore, \code{dropDuplicateIDs} drops rows based on number of missing values
#' on the specified variables (\code{varNames}).
#'
#' If multiple rows have the same number of missing values, a warning is issued
#' and the first of the respective rows is kept.
#'
#'@param GADSdat A \code{GADSdat} object.
#'@param ID Name of the ID variable.
#'@param varNames Character vector of variable names: Sum of missing values
#' on these variables decide which rows are kept. Per default, all variables
#' except the ID variable are used.
#'
#'@return Returns the \code{GADSdat} with duplicate ID rows removed.
#'
#'@examples
#' # create example data set
#' gads_ori <- import_DF(data.frame(id_var = c(1, 2, 5, 4, 4),
#'   var1 = c(1, 2, -99, 1, -99)))
#' gads_ori <- changeMissings(gads_ori, varName = "var1",
#'   value = -99, missings = "miss")
#'
#' # drop duplicate IDs
#' dropDuplicateIDs(gads_ori, ID = "id_var")
#'
#'@export
dropDuplicateIDs <- function(GADSdat, ID, varNames = setdiff(namesGADS(GADSdat), ID)) {
  UseMethod("dropDuplicateIDs")
}

#'@export
dropDuplicateIDs.GADSdat <- function(GADSdat, ID, varNames = setdiff(namesGADS(GADSdat), ID)) {
  check_GADSdat(GADSdat)
  check_single_varName(ID, argumentName = "ID")
  check_vars_in_GADSdat(GADSdat, vars = ID, argName = "ID")
  check_vars_in_GADSdat(GADSdat, vars = varNames, argName = "varNames")

  suppressWarnings(dat <- extractData2(GADSdat, convertMiss = TRUE))
  duplicate_IDs <- unique(dat[duplicated(dat[, ID]), ID])

  if(length(duplicate_IDs) > 0) {
    # counting missing values and collecting all info in one data.frame
    row_nums <- which(dat[, ID] %in% duplicate_IDs)
    sum_missings <- rowSums(is.na(dat[row_nums, varNames, drop = FALSE]))
    missing_infos <- data.frame(row_nums = row_nums,
                             IDs = dat[row_nums, ID],
                             sum_missings = sum_missings)

    # select rows to keep/drop
    all_drop_rows <- numeric()
    for(duplicate_ID in duplicate_IDs) {
      single_missing_info <- missing_infos[missing_infos$IDs == duplicate_ID, ]

      #browser()
      keep_rows <- single_missing_info[
        single_missing_info$sum_missings == min(single_missing_info$sum_missings), ]
      if(nrow(keep_rows) > 1) {
        warning("Multiple rows with ", ID, " ",
                duplicate_ID, " have the same number of missing values (",
                keep_rows[1, "sum_missings"], ").")
        keep_rows <- keep_rows[1, ]
      }
      drop_rows <- single_missing_info[single_missing_info$row_nums != keep_rows$row_nums, ]
      all_drop_rows <- c(all_drop_rows, drop_rows$row_nums)
    }
  }

  GADSdat_out <- GADSdat
  GADSdat_out$dat <- GADSdat$dat[-all_drop_rows, ]

  GADSdat_out
}
