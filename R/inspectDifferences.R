####
#############################################################################
#' Inspect differences in a variable.
#'
#' Inspect differences between two \code{GADSdat} objects in a specific variable.
#'
#' Two \code{GADSdat} objects can be compared using \code{\link{equalGADS}}.
#' If differences in the data for specific variables in the two objects occur,
#' these variables can be further inspected using \code{inspectDifferences}. Differences on meta data-level can be inspected via
#' \code{\link{inspectMetaDifferences}}.
#'
#'@param varName A character vector of length 1 containing the variable name.
#'@param GADSdat1 A \code{GADSdat} object.
#'@param GADSdat2 A \code{GADSdat} object.
#'@param id A character vector of length 1 containing the unique identifier column of both \code{GADSdat}.
#'
#'@return A list.
#'
#'@examples
#' # create a second GADS with different data
#' pisa2 <- pisa
#' pisa2$dat$age[400:nrow(pisa$dat)] <- sample(pisa2$dat$age[400:nrow(pisa$dat)])
#'
#' # inspect via equalGADS()
#' equalGADS(pisa, pisa2)
#'
#' # inspect via inspectDifferences()
#' inspectDifferences("age", GADSdat1 = pisa, GADSdat2 = pisa2, id = "idstud")
#'
#'@export
inspectDifferences <- function(varName, GADSdat1, GADSdat2, id) {
  check_GADSdat(GADSdat1)
  check_GADSdat(GADSdat2)
  if(!is.character(varName) || length(varName) != 1) stop("'varName' must be a character of length 1.")
  if(!is.character(id) || length(id) != 1) stop("'id' must be a character of length 1.")
  if(!varName %in% namesGADS(GADSdat1)) stop("'varName' is not a variable in 'GADSdat1'.")
  if(!varName %in% namesGADS(GADSdat2)) stop("'varName' is not a variable in 'GADSdat2'.")
  if(!id %in% namesGADS(GADSdat1)) stop("'id' is not a variable in 'GADSdat1'.")
  if(!id %in% namesGADS(GADSdat2)) stop("'id' is not a variable in 'GADSdat2'.")
  if(nrow(GADSdat1$dat) != nrow(GADSdat2$dat)) stop("'GADSdat1' and 'GADSdat2' have different row numbers.")
  if(any(is.na(GADSdat1$dat[, id]))) stop("Missing values in 'id' column of 'GADSdat1'.")
  if(any(is.na(GADSdat2$dat[, id]))) stop("Missing values in 'id' column of 'GADSdat2'.")
  if(any(GADSdat1$dat[, id] != GADSdat2$dat[, id])) stop("'id' column is not equal for 'GADSdat1' and 'GADSdat2'.")

  if(is.numeric(GADSdat1$dat[, varName]) && !is.numeric(GADSdat2$dat[, varName])) stop("'varName' column is numeric in 'GADSdat1' but not in 'GADSdat2'.")
  if(!is.numeric(GADSdat1$dat[, varName]) && is.numeric(GADSdat2$dat[, varName])) stop("'varName' column is numeric in 'GADSdat2' but not in 'GADSdat1'.")

  if(isTRUE(all.equal(GADSdat2$dat[, varName], GADSdat1$dat[, varName], scale = 1))) return("all.equal")

  unequal_rows <- c(which(GADSdat2$dat[, varName] != GADSdat1$dat[, varName]),
                    which(is.na(GADSdat2$dat[, varName]) & !is.na(GADSdat1$dat[, varName])),
                    which(!is.na(GADSdat2$dat[, varName]) & is.na(GADSdat1$dat[, varName])))
  unequal_case_dat2 <- GADSdat2$dat[unequal_rows, ]
  unequal_case_dat1 <- GADSdat1$dat[unequal_rows, ]

  ncol1 <- ifelse(ncol(GADSdat1$dat) > 8, yes = 8, no = ncol(GADSdat1$dat))
  ncol2 <- ifelse(ncol(GADSdat2$dat) > 8, yes = 8, no = ncol(GADSdat2$dat))
  nrow1 <- ifelse(nrow(unequal_case_dat1) > 5, yes = 5, no = nrow(unequal_case_dat1))
  nrow2 <- ifelse(nrow(unequal_case_dat2) > 5, yes = 5, no = nrow(unequal_case_dat2))

  list(cross_table = table(GADSdat1$dat[, varName], GADSdat2$dat[, varName], useNA = "if",
                           dnn = c("GADSdat1", "GADSdat2")),
       some_unequals_GADSdat1 = unequal_case_dat1[1:nrow1, unique(c(namesGADS(GADSdat1)[1:ncol1], varName))],
       some_unequals_GADSdat2 = unequal_case_dat2[1:nrow2, unique(c(namesGADS(GADSdat2)[1:ncol2], varName))],
       unequal_IDs = unequal_case_dat2[, id]
  )
}
