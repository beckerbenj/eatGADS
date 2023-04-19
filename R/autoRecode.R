####
#############################################################################
#' Auto recode a variable in a \code{GADSdat}.
#'
#' Auto recode a variable in a \code{GADSdat}. A look up table is created containing the respective recode pairs.
#' An existing look up table can be utilized via \code{template}. This function somewhat mirrors the functionality provided
#' by the \code{SPSS} function \code{autorecode}.
#'
#' If an existing \code{template} is used and a look up table is saved as a \code{.csv} file, the resulting look up
#' table will contain the existing recodes plus additional recode pairs required for the data.
#'
#'@param GADSdat A \code{GADSdat} object.
#'@param var Character string of the variable name which should be sorted.
#'@param var_suffix Variable suffix for the newly created \code{GADSdat}. If an empty character, the existing variables are overwritten.
#'@param label_suffix Suffix added to variable label for the newly created variable in the \code{GADSdat}.
#'@param csv_path Path for the \code{.csv} file for the look up table.
#'@param template Existing look up table.
#'
#'@return Returns a \code{GADSdat} object.
#'
#'@examples
#' gads <- import_DF(data.frame(v1 = letters))
#'
#' # auto recode without saving look up table
#' gads2 <- autoRecode(gads, var = "v1", var_suffix = "_num")
#'
#' # auto recode with saving look up table
#' f <- tempfile(fileext = ".csv")
#' gads2 <- autoRecode(gads, var = "v1", var_suffix = "_num", csv_path = f)
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
  GADSdat_out <- cloneVariable(GADSdat = GADSdat, varName = var, new_varName = new_var, label_suffix = label_suffix)

  if(is.null(template)) {
    # to character
    GADSdat_out[["dat"]][[new_var]] <- as.character(GADSdat_out[["dat"]][[new_var]])
    GADSdat_out <- changeSPSSformat(GADSdat_out, varName = new_var, format = "A10")

    # to factor
    GADSdat_out <- multiChar2fac(GADSdat_out, vars = new_var, var_suffix = "", label_suffix = "")

    # look up table
    lookup <- extractMeta(GADSdat_out, vars = new_var)[, c("valLabel", "value")]
    names(lookup) <- c("oldValue", "newValue")
    GADSdat_out <- removeValLabels(GADSdat_out, varName = new_var, value = c(lookup$newValue))
  } else {
    if(!is.data.frame(template) || !identical(names(template), c("oldValue", "newValue"))) {
      stop()
    }

    new_oldValues <- GADSdat$dat[[var]][!GADSdat$dat[[var]] %in% template$oldValue]
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
