####
#############################################################################
#' Compare two GADS.
#'
#' Compare multiple variables of two \code{GADSdat} or \code{all_GADSdat} objects.
#'
#' Returns \code{"all equal"} if the variable is identical across the objects or a \code{data.frame}
#' containing a frequency table with the values which have been changed. Especially useful for checks
#' after recoding.
#'
#'@param GADSdat_old \code{GADSdat} object imported via \code{eatGADS}.
#'@param GADSdat_new \code{GADSdat} object imported via \code{eatGADS}.
#'@param varNames Character string of variable names to be compared.
#'
#'@return Returns a list with \code{"all equal"} or a \code{data.frame}.
#'
#'@examples
#'# Recode a GADS
#' pisa2 <- recodeGADS(pisa, varName = "schtype",
#'                         oldValues = 3, newValues = 9)
#'
#'# Compare
#' compareGADS(pisa, pisa2, varNames = c("ganztag", "schtype"))
#'
#'@export
compareGADS <- function(GADSdat_old, GADSdat_new, varNames) {
  UseMethod("compareGADS")
}
#'@export
compareGADS.GADSdat <- function(GADSdat_old, GADSdat_new, varNames) {
  check_GADSdat(GADSdat_old)
  check_GADSdat(GADSdat_new)
  check_vars_in_GADSdat(GADSdat_old, varNames)
  check_vars_in_GADSdat(GADSdat_new, varNames)

  out_list <- as.list(rep("all equal", length(varNames)))
  names(out_list) <- varNames
  for(nam in varNames) {
    old_unequals <- GADSdat_old$dat[which(GADSdat_old$dat[, nam] != GADSdat_new$dat[, nam]), nam]

    if(length(old_unequals) > 0) {
      out <- as.data.frame(table(old_unequals))
      names(out) <- c("value", "frequency")
      out[, "value"] <- as.character(out[, "value"])
      out[, c("valLabel", "missings")] <- NA

      ## add value labels and missing codes
      for(i in out[, "value"]) {
        #browser()
        i <- eatTools::asNumericIfPossible(i, force.string = FALSE)
        value_meta <- GADSdat_old$labels[GADSdat_old$labels$varName == nam & GADSdat_old$labels$value == i, c("valLabel", "missings")]
        if(nrow(value_meta) > 0) out[out$value == i, c("valLabel", "missings")] <- value_meta
      }
      out_list[[nam]] <- out
    }
  }

  out_list
}
