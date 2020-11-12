
#### Get data from Gads
#############################################################################
#' Get data from GADS data base.
#'
#'Extracts variables from a GADS data base. Only the specified variables are extracted. Note that this selection determines the format of
#'the \code{data.frame} that is extracted.
#'
#' See \code{\link[eatDB]{createDB}} and \code{\link[eatDB]{dbPull}} for further explanation of the query and merging processes.
#'
#'@param vSelect Character vector of variable names.
#'@param filePath Path of the existing \code{eatGADS} data base file.
#'
#'@return Returns a \code{GADSdat} object.
#'
#'@examples
#'# Use data base within package
#'db_path <- system.file("extdata", "pisa.db", package = "eatGADS")
#'pisa_gads <- getGADS(db_path, vSelect = c("schtype", "sameteach"))
#'
#'@export
getGADS <- function(vSelect = NULL, filePath) {
  GADSdat <- eatDB::dbPull(vSelect = vSelect, filePath = filePath)
  allLabels <- labelsGADS(filePath = filePath)
  selectLabels <- allLabels[allLabels$varName %in% names(GADSdat), , drop = FALSE]

  # select Meta data from first data table only (only relevant for foreign keys)
  fk_vars <- unique(unlist(lapply(eatDB::dbKeys(filePath)$fkList, function(fk) fk$Keys)))
  all_names <- namesGADS(filePath)
  for(fk_var in fk_vars) {
    data_table <- first_list_match(x = fk_var, vec_list = all_names)
    selectLabels <- drop_duplicate_meta(labels = selectLabels, varName = fk_var, data_table)
  }

  # drop irrelevant data_table column
  selectLabels <- selectLabels[, names(selectLabels) != "data_table"]
  new_GADSdat(dat = GADSdat, labels = selectLabels)
}

drop_duplicate_meta <- function(labels, varName, data_table) {
  labels[labels$varName != varName | labels$data_table == data_table, ]
}

first_list_match <- function(x, vec_list) {
  i <- 1
  while(!x %in% vec_list[[i]]) i <- i + 1
  names(vec_list)[i]
}
