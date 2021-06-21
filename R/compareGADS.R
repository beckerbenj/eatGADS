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
#'@param output How should the output be structured?
#'
#'@return Returns either a list with \code{"all equal"} and \code{data.frames} or a single \code{data.frame}.
#'
#'@examples
#'# Recode a GADS
#' pisa2 <- recodeGADS(pisa, varName = "schtype",
#'                         oldValues = 3, newValues = 9)
#' pisa2 <- recodeGADS(pisa2, varName = "language",
#'                         oldValues = 1, newValues = 15)
#'
#'# Compare
#' compareGADS(pisa, pisa2,
#'             varNames = c("ganztag", "schtype", "language"), output = "list")
#' compareGADS(pisa, pisa2,
#'             varNames = c("ganztag", "schtype", "language"), output = "data.frame")
#' compareGADS(pisa, pisa2,
#'              varNames = c("ganztag", "schtype", "language"), output = "aggregated")
#'
#'@export
compareGADS <- function(GADSdat_old, GADSdat_new, varNames, output = c("list", "data.frame", "aggregated")) {
  UseMethod("compareGADS")
}
#'@export
compareGADS.GADSdat <- function(GADSdat_old, GADSdat_new, varNames, output = c("list", "data.frame", "aggregated")) {
  check_GADSdat(GADSdat_old)
  check_GADSdat(GADSdat_new)
  check_vars_in_GADSdat(GADSdat_old, varNames)
  check_vars_in_GADSdat(GADSdat_new, varNames)

  output <- match.arg(output)

  out_list <- as.list(rep("all equal", length(varNames)))
  names(out_list) <- varNames
  for(nam in varNames) {
    old_unequals1 <- GADSdat_old$dat[which(GADSdat_old$dat[, nam] != GADSdat_new$dat[, nam]), nam]
    old_unequals2 <- GADSdat_old$dat[is.na(GADSdat_old$dat[, nam]) & !is.na(GADSdat_new$dat[, nam]), nam]
    old_unequals3 <- GADSdat_old$dat[!is.na(GADSdat_old$dat[, nam]) & is.na(GADSdat_new$dat[, nam]), nam]
    old_unequals <- c(old_unequals1, old_unequals2, old_unequals3)

    if(length(old_unequals) > 0) {
      out <- as.data.frame(table(old_unequals, useNA = "ifany"))
      names(out) <- c("value", "frequency")
      out[, "value"] <- as.character(out[, "value"])
      out[, c("valLabel", "missings")] <- NA

      ## add value labels and missing codes
      for(i in out[, "value"]) {
        i <- suppressWarnings(eatTools::asNumericIfPossible(i, force.string = FALSE))
        value_meta <- GADSdat_old$labels[which(GADSdat_old$labels$varName == nam & GADSdat_old$labels$value == i), c("valLabel", "missings")]
        if(is.na(i)) {
          value_meta <- GADSdat_old$labels[GADSdat_old$labels$varName == nam & is.na(GADSdat_old$labels$value), c("valLabel", "missings")]
          if(nrow(value_meta) > 1) stop("Meta information on value level is not unique for variable: ", nam, " and value: NA")
        }

        if(nrow(value_meta) > 0) {
          if(is.na(i)) { out[is.na(out$value), c("valLabel", "missings")] <- value_meta
          } else out[which(out$value == i), c("valLabel", "missings")] <- value_meta
        }
      }
      out_list[[nam]] <- out
    }
  }

  # restructure output according to output argument
  if(!identical(output, "list")) {
    out_list <- out_list[sapply(out_list, function(x) !identical(x, "all equal"))]
    if(length(out_list) < 1) return("all equal")

    out_list <- eatTools::do_call_rbind_withName(out_list, colName = "variable")
    if(identical(output, "aggregated")) {
      out_list <- unique(out_list[, c("value", "valLabel", "missings")])
    }
  }

  out_list
}
