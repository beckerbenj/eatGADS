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
#'@param filePath1 Path of the first \code{eatGADS} \code{.db} file.
#'@param filePath2 Path of the second \code{eatGADS} \code{.db} file.
#'@param lePath Path of the linking error \code{eatGADS} \code{.db} file.
#'
#'@return Returns a report list.
#'
#'
#'@export
checkLEStructure <- function(filePath1, filePath2, lePath) {
  nam1 <- namesGADS(filePath1)
  nam2 <- namesGADS(filePath2)
  namLE <- namesGADS(lePath)

  ## all Linking error variables in trend gads data bases (without LE_)?
  LE_variables <- grep("^LE_", unlist(namLE), value = TRUE)
  dep_variables <- gsub(pattern = "^LE_", replacement = "", LE_variables)

  dep_notIn_nam1 <- setdiff(dep_variables, unlist(nam1))
  dep_notIn_nam2 <- setdiff(dep_variables, unlist(nam2))
  if(length(dep_notIn_nam1) > 0) message("The following variables have linking errors but are not variables in data base 1: ",
                                         dep_notIn_nam1)
  if(length(dep_notIn_nam2) > 0) message("The following variables have linking errors but are not variables in data base 2: ",
                                         dep_notIn_nam2)

  ## all other variables should be primary keys
  le_pks <- unlist(eatDB::dbKeys(lePath)$pkList)
  le_keys <- setdiff(unlist(namLE), LE_variables)
  if(!all(le_pks %in% le_keys) || !all(le_keys %in% le_pks)) message("The linking error data base contains variables other than linking errors and key variables.")

  # all primary keys should be in trend gads data bases
  key_notIn_nam1 <- setdiff(le_pks, unlist(nam1))
  key_notIn_nam2 <- setdiff(le_pks, unlist(nam2))
  if(length(key_notIn_nam1) > 0) message("The following variables are key variables in the Linking Error data base but are not variables in data base 1: ", key_notIn_nam1)
  if(length(key_notIn_nam2) > 0) message("The following variables are key variables in the Linking Error data base but are not variables in data base 2: ", key_notIn_nam2)

  list(dep_notIn_nam1 = dep_notIn_nam1, dep_notIn_nam2 = dep_notIn_nam2,
       key_notIn_nam1 = key_notIn_nam1, key_notIn_nam2 = key_notIn_nam2)
}
