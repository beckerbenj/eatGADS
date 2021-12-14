#### Checks structure of trend gads and linking errors
#############################################################################
#' Checks compatibility of GADS data bases with a linking error data base.
#'
#' This function checks if a linking error data base is compatible with the two trend \code{eatGADS} data bases. For checking the compatibility
#' of two \code{eatGADS} data bases see \code{\link{checkTrendStructure}}.
#'
#' This function inspects whether all linking error variables correspond to variables in the \code{eatGADS} data base and if the key variables
#' also correspond to existing variables in the trend \code{eatGADS} data bases.
#'
#'@param filePaths Character vectors with paths to the \code{eatGADS} db files.
#'@param lePath Path of the linking error \code{eatGADS} \code{.db} file.
#'
#'@return Returns a report list.
#'
#'
#'@export
checkLEStructure <- function(filePaths, lePath) {
  nam_list <- lapply(filePaths, namesGADS)
  namLE <- namesGADS(lePath)

  ## all Linking error variables in trend gads data bases (without LE_)?
  LE_variables <- grep("^LE_", unlist(namLE), value = TRUE)
  dep_variables <- gsub(pattern = "^LE_", replacement = "", LE_variables)

  #browser()
  dep_notIn_nam_list <- lapply(seq_along(nam_list), function(i) {
    nam <- nam_list[[i]]
    dep_notIn_nam <- setdiff(dep_variables, unlist(nam))
    if(length(dep_notIn_nam) > 0) message("The following variables have linking errors but are not variables in data base ",  i, ": ",
                                          paste0(dep_notIn_nam, collapse = ", "))
    dep_notIn_nam
  })

  ## all other variables should be primary keys
  le_pks <- unlist(eatDB::dbKeys(lePath)$pkList)
  le_keys <- setdiff(unlist(namLE), LE_variables)
  if(!all(le_pks %in% le_keys) || !all(le_keys %in% le_pks)) message("The linking error data base contains variables other than linking errors and key variables.")

  # all primary keys should be in trend gads data bases
  key_notIn_nam_list <- lapply(seq_along(nam_list), function(i) {
    nam <- nam_list[[i]]
    key_notIn_nam <- setdiff(le_pks, unlist(nam))
    if(length(key_notIn_nam) > 0) message("The following variables are key variables in the Linking Error data base but are not variables in data base ", i, ": ", paste0(key_notIn_nam, collapse = ", "))

    key_notIn_nam
  })

  list(dep_notIn_nam = dep_notIn_nam_list,
       key_notIn_nam = key_notIn_nam_list)
}


## tbd:
# check for year1 & year2 and exclude from other checks
# maybe from now on: linking error data bases have to have year1 & year2? getTrendsGADS requires this implicitly!
# also missing: checkTrendStructure!
