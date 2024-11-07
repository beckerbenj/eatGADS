####
#############################################################################
#' Inspect meta data differences in a variable.
#'
#' Inspect meta data differences within a single \code{GADSdat} or between two \code{GADSdat} objects
#' or \code{GADSdat} data bases regarding a specific variable.
#'
#' Two \code{GADSdat} objects can be compared using \code{\link{equalGADS}}.
#' If meta data differences for specific variables in the two objects occur,
#' these variables can be further inspected using \code{inspectMetaDifferences}.
#' For data-level differences for a specific variable, see \code{\link{inspectDifferences}}.
#'
#'@param GADSdat A \code{GADSdat} object.
#'@param varName A character vector of length 1 containing the variable name.
#'@param other_GADSdat A second \code{GADSdat} object. If omitted, it is assumed that both variables are part of the
#'first \code{GADSdat}.
#'@param other_varName A character vector of length 1 containing the other variable name.
#'If omitted, it is assumed that both variables have identical names (as supplied in \code{varName}).
#'
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
#' inspectMetaDifferences(GADSdat = pisa, varName = "sameteach", other_GADSdat = pisa2)
#'
#'@export
inspectMetaDifferences <- function(GADSdat, varName, other_GADSdat = GADSdat, other_varName = varName) {
  check_characterArgument(varName, argName = "varName")
  check_characterArgument(other_varName, argName = "other_varName")
  check_vars_in_GADSdat(GADSdat, vars = varName, argName = "varName", GADSdatName = "GADSdat")
  check_vars_in_GADSdat(other_GADSdat, vars = other_varName, argName = "other_varName", GADSdatName = "other_GADSdat")

  meta1 <- extractMeta(GADSdat, varName)
  meta2 <- extractMeta(other_GADSdat, other_varName)

  # naming for columns in output
  nam_col <- c(varName, other_varName)
  if(identical(varName, other_varName)) {
    nam_col <- c("GADSdat", "other_GADSdat")
  }

  ## Variable level
  metaVar1 <- meta1[1, c("varLabel", "format")]
  metaVar2 <- meta2[1, c("varLabel", "format")]
  row.names(metaVar1) <- row.names(metaVar2) <- NULL

  varDiff <- "all.equal"
  if(!identical(metaVar1, metaVar2)) {
    # only return column with differences
    if(!identical(metaVar1$varLabel, metaVar2$varLabel)) {
      varDiff <- data.frame(metaVar1$varLabel, metaVar2$varLabel)
      names(varDiff) <- paste(nam_col, "varLabel", sep = "_")
      if(!identical(metaVar1$format, metaVar2$format)) {
        varDiff2 <- data.frame(metaVar1$format, metaVar2$format)
        names(varDiff2) <- paste(nam_col, "format", sep = "_")

        varDiff <- cbind(varDiff, varDiff2)[, c(1, 3, 2, 4)]
      }
    } else {
      varDiff <- data.frame(metaVar1$format, metaVar2$format)
      names(varDiff) <- paste(nam_col, "format", sep = "_")
    }
  }

  ## Value level
  metaVal1 <- meta1[, c("varName", "value", "valLabel", "missings")]
  metaVal2 <- meta2[, c("varName", "value", "valLabel", "missings")]
  row.names(metaVal1) <- row.names(metaVal2) <- NULL

  # hotfix, this should be properly fixed someday
  metaVal1$value <- as.numeric(metaVal1$value)
  metaVal2$value <- as.numeric(metaVal2$value)

  valDiff <- "all.equal"
  if(!identical(metaVal1[, c("value", "valLabel", "missings")], metaVal2[, c("value", "valLabel", "missings")])) {
    valDiff <- data.frame(value = integer(),
                          valLabel1 = character(), missings2 = character(),
                          valLabel2 = character(), missings2 = character())

    all_values <- unique(stats::na.omit(c(metaVal1$value, metaVal2$value)))
    for(val in all_values) {
      meta_row1 <- metaVal1[metaVal1$value == val, ]
      meta_row2 <- metaVal2[metaVal2$value == val, ]
      if(nrow(meta_row1) == 0) meta_row1[1, ] <- NA
      if(nrow(meta_row2) == 0) meta_row2[1, ] <- NA
      valDiff_new <- data.frame(value = val, meta_row1[, 3:4], meta_row2[, 3:4])
      row.names(meta_row1) <- row.names(meta_row2) <- NULL
      if(!identical(meta_row1, meta_row2)) valDiff <- rbind(valDiff, valDiff_new)
    }
    valDiff <- valDiff[order(valDiff$value), ]
    names(valDiff) <- c("value", paste0(nam_col[1], "_valLabel"), paste0(nam_col[1], "_missings"),
                        paste0(nam_col[2], "_valLabel"), paste0(nam_col[2], "_missings"))
    rownames(valDiff) <- NULL
  }

  list(varDiff = varDiff, valDiff = valDiff)
}
