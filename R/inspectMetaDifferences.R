####
#############################################################################
#' Inspect meta differences in a variable.
#'
#' Inspect meta differences between two \code{GADSdat} objects or \code{GADSdat} data bases regarding a specific variable.
#'
#' Two \code{GADSdat} objects can be compared using \code{\link{equalGADS}}.
#' If differences in the meta data are indicated for specific variables,
#' these variables can be further inspected using \code{inspectMetaDifferences}.
#' For differences on data level for a specific variable, see \code{\link{inspectDifferences}}.
#'
#'@param varName A character vector of length 1 containing the variable name.
#'@param GADSdat1 A \code{GADSdat} object.
#'@param GADSdat2 A \code{GADSdat} object.

#'@return A list.
#'
#'@examples
#' # create a second GADS with different meta data
#' pisa2 <- pisa
#' pisa2 <- changeVarLabels(pisa2, varName = "sameteach", varLabel = "Same math teacher")
#' pisa2 <- recodeGADS(pisa2, varName = "sameteach", oldValues = c(1, 2), newValues = c(0, 1))
#'
#' # inspect via equalGADS()
#' equalGADS(pisa, pisa2)
#'
#' # inspect via inspectMetaDifferences()
#' inspectMetaDifferences("sameteach", GADSdat1 = pisa, GADSdat2 = pisa2)
#'
#'@export
inspectMetaDifferences <- function(varName, GADSdat1, GADSdat2) {
  check_characterArgument(varName, argName = "varName")
  check_vars_in_GADSdat
  if(!varName %in% unlist(namesGADS(GADSdat1))) stop("'varName' is not a variable in 'GADSdat1'.")
  if(!varName %in% unlist(namesGADS(GADSdat2))) stop("'varName' is not a variable in 'GADSdat2'.")

  meta1 <- extractMeta(GADSdat1, varName)
  meta2 <- extractMeta(GADSdat2, varName)

  ## Variable level
  metaVar1 <- meta1[1, c("varName", "varLabel", "format")]
  metaVar2 <- meta2[1, c("varName", "varLabel", "format")]
  row.names(metaVar1) <- row.names(metaVar2) <- NULL

  varDiff <- NULL
  if(!identical(metaVar1, metaVar2)) {
    varDiff <- data.frame(varName = varName,
                          GADS1 = metaVar1[, 2:3],
                          GADS2 = metaVar2[, 2:3])
  }

  ## Value level
  metaVal1 <- meta1[, c("varName", "value", "valLabel", "missings")]
  metaVal2 <- meta2[, c("varName", "value", "valLabel", "missings")]
  row.names(metaVal1) <- row.names(metaVal2) <- NULL

  valDiff <- NULL
  if(!identical(metaVal1, metaVal2)) {
    all_values <- unique(stats::na.omit(c(metaVal1$value, metaVal2$value)))
    for(val in all_values) {
      #browser()
      meta_row1 <- metaVal1[metaVal1$value == val, ]
      meta_row2 <- metaVal2[metaVal2$value == val, ]
      if(nrow(meta_row1) == 0) meta_row1[1, ] <- NA
      if(nrow(meta_row2) == 0) meta_row2[1, ] <- NA
      valDiff_new <- data.frame(varName = varName, value = val, GADS1 = meta_row1[, 3:4], GADS2 = meta_row2[, 3:4])
      if(!identical(meta_row1, meta_row2)) valDiff <- rbind(valDiff, valDiff_new)
    }
    valDiff <- valDiff[order(valDiff$value), ]
  }

  list(varDiff = varDiff, valDiff = valDiff)
}
