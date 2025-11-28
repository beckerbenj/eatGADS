####
#############################################################################
#' Auto recode a variable in a \code{GADSdat}.
#'
#' Auto recode a variable in a \code{GADSdat}, mirroring the core functionality provided by
#'  \code{AUTORECODE} in \code{SPSS}. A lookup table containing the respective recode pairs can be
#'  applied and/or saved.
#'
#' Existing values are replaced with sequential numbers, and all existing value-level metadata
#'  (\code{valLabel} and \code{missings}) are dropped. This can be useful to remove confidential
#'  information from ID variables. If the original (\code{character}) values are to be preserved as
#'  \code{valLabel}s, \link{multiChar2fac} should be used instead.
#'
#' An existing \code{template} may be used to ensure that identical original values are recoded as
#'  the same new values. The lookup table used to recode \code{var} may also be saved as a
#'  \code{.csv} file, e.g. to be used as a \code{template} later. If both an existing \code{template}
#'  is used and the lookup table is saved, the resulting lookup table will contain the existing
#'  recodes and additional recode pairs required for the data, if any were needed.
#'
#'@param GADSdat A \code{GADSdat} object.
#'@param var Character string of the variable name which should be recoded.
#'@param var_suffix Variable suffix for the newly created \code{GADSdat}. If an empty character, the existing variables are overwritten.
#'@param label_suffix Suffix added to variable label for the newly created variable in the \code{GADSdat}.
#'@param csv_path Path for the \code{.csv} file for the lookup table.
#'@param template Existing lookup table.
#'
#'@return Returns a \code{GADSdat} object.
#'
#'@examples
#' gads <- import_DF(data.frame(v1 = letters))
#'
#' # auto recode without saving lookup table
#' gads2 <- autoRecode(gads, var = "v1", var_suffix = "_num")
#'
#' # auto recode with saving lookup table
#' f <- tempfile(fileext = ".csv")
#' gads2 <- autoRecode(gads, var = "v1", var_suffix = "_num", csv_path = f)
#'
#' # auto recode with applying and expanding a lookup table
#' gads3 <- import_DF(data.frame(v2 = c(letters[1:3], "aa")))
#' gads3 <- autoRecode(gads3, var = "v2", csv_path = f,
#'                     template = read.csv(f))
#'@export
autoRecode <- function(GADSdat, var, var_suffix = "", label_suffix = "", csv_path = NULL, template = NULL) {
  UseMethod("autoRecode")
}
#'@export
autoRecode.GADSdat <- function(GADSdat, var, var_suffix = "", label_suffix = "", csv_path = NULL, template = NULL) {
  check_GADSdat(GADSdat)
  check_single_varName(var)
  check_vars_in_GADSdat(GADSdat, vars = c(var))

  # duplicate
  new_var <- paste0(var, var_suffix)
  if(!identical(new_var, var)) {
    GADSdat_out <- cloneVariable(GADSdat = GADSdat, varName = var, new_varName = new_var, label_suffix = label_suffix)
  } else {
    GADSdat_out <- append_varLabel(GADSdat, varName = var, label_suffix = label_suffix)
  }

  if(is.null(template)) {
    # new variable to character
    GADSdat_out[["dat"]][[new_var]] <- as.character(GADSdat_out[["dat"]][[new_var]])
    GADSdat_out <- changeSPSSformat(GADSdat_out, varName = new_var, format = "A10")

    # new variable to factor
    GADSdat_out <- multiChar2fac(GADSdat_out, vars = new_var, var_suffix = "", label_suffix = "")

    # lookup table
    lookup <- extractMeta(GADSdat_out, vars = new_var)[, c("valLabel", "value")]
    names(lookup) <- c("oldValue", "newValue")
    GADSdat_out <- removeValLabels(GADSdat_out, varName = new_var, value = c(lookup$newValue))
  } else {
    if(!is.data.frame(template) || !identical(names(template), c("oldValue", "newValue"))) {
      stop()
    }

    # recoding the actual data via a lookup table
    new_oldValues <- unique(GADSdat$dat[[var]][!GADSdat$dat[[var]] %in% template$oldValue])
    new_oldValues <- new_oldValues[!is.na(new_oldValues)]
    new_newValues <- seq(from = max(template$newValue) + 1, length.out = length(new_oldValues))
    lookup <- rbind(template, data.frame(oldValue = new_oldValues, newValue = new_newValues))
    lookup_table <- data.frame(variable = new_var, lookup)
    names(lookup_table) <- c("variable", "value", "value_new")
    suppressMessages(GADSdat_out <- applyLookup(GADSdat_out, lookup = lookup_table))
  }

  if(!is.null(csv_path)) utils::write.csv(lookup, file = csv_path, row.names = FALSE)

  #browser()
  GADSdat_out
}
