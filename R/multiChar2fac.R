#### Multiple Strings to Labeled Variables
#############################################################################
#' Transform multiple character variables to factors with identical levels.
#'
#' Convert multiple character variables to factors, while creating a common set of value labels, which is identical across variables.
#'
#' If a set of variables has the same possible values, it is desirable that these variables share the same
#' value labels, even if some of the values do not occur on the individual variables. This function allows
#' the transformation of multiple character variables to factors while assimilating the value labels.
#' The SPSS format of the newly created variables is set to \code{F10.0}.
#'
#' If necessary, missing codes can be set after transformation via \code{\link{checkMissings}} for setting missing codes
#' depending on value labels for all variables or
#' \code{\link{changeMissings}} for setting missing codes for specific values in a specific variable.
#'
#'@param GADSdat A \code{data.frame} or \code{GADSdat} object.
#'@param vars A character vector with all variables that should be transformed to factor.
#'@param var_suffix Variable suffix for the newly created \code{GADSdat}. If an empty character, the existing variables are overwritten.
#'@param label_suffix Suffix added to variable label for the newly created variable in the \code{GADSdat}.
#'
#'@return Returns a \code{GADSdat} containing the newly computed variable.
#'
#'@examples
#'## create an example GADSdat
#'example_df <- data.frame(ID = 1:4,
#'                         citizenship1 = c("German", "English", "missing by design", "Chinese"),
#'                         citizenship2 = c("missing", "German", "missing by design", "Polish"),
#'                         stringsAsFactors = FALSE)
#'gads <- import_DF(example_df)
#'
#'## transform multiple strings
#'gads2 <- multiChar2fac(gads, vars = c("citizenship1", "citizenship2"))
#'
#'## set values to missings
#'gads3 <- checkMissings(gads2, missingLabel = "missing")
#'
#'@export
multiChar2fac <- function(GADSdat, vars, var_suffix = "_r", label_suffix = "(recoded)") {
  UseMethod("multiChar2fac")
}

#'@export
multiChar2fac.GADSdat <- function(GADSdat, vars, var_suffix = "_r", label_suffix = "(recoded)") {
  check_GADSdat(GADSdat)
  if(!is.character(vars) && length(vars) > 0) stop("vars needs to be a character vector of at least length 1.")

  suppressMessages(only_vars_gads <- removeVars(GADSdat, namesGADS(GADSdat)[!namesGADS(GADSdat) %in% vars]))

  # potential problem: existing (non-missing) value labels
  for(var in namesGADS(only_vars_gads)) {
    existing_meta <- extractMeta(only_vars_gads, var)
    existing_labels <- existing_meta[which(existing_meta$missings != "miss"), "value"]
    existing_labels <- na_omit(existing_labels)
    only_vars_gads$dat[only_vars_gads$dat[, var] %in% existing_labels, var] <- NA
  }

  suppressWarnings(df_no_miss <- extractData(only_vars_gads))
  all_levels <- unique(unlist(lapply(df_no_miss, function(x) x)))
  all_levels_fac <- data.frame("all_levels" = as.factor(all_levels))
  all_levels_gads <- import_DF(all_levels_fac)
  all_levels_lookup <- all_levels_gads$labels[, c("valLabel", "value")]
  names(all_levels_lookup) <- c("value", "value_new")

  for(var in vars) {
    old_nam <- var
    var <- paste0(var, var_suffix)
    specific_lookup <- data.frame(variable = old_nam, all_levels_lookup, stringsAsFactors = FALSE)

    #browser()
    GADSdat <- suppressMessages(suppressWarnings(applyLookup(GADSdat, lookup = specific_lookup, suffix = var_suffix)))
    GADSdat$dat[, var] <- as.numeric(GADSdat$dat[, var])

    GADSdat <- reuseMeta(GADSdat, varName = var, other_GADSdat = all_levels_gads, other_varName = "all_levels",
                         addValueLabels = TRUE)
    GADSdat <- append_varLabel(GADSdat, varName = var, label_suffix = label_suffix)
    GADSdat$labels[GADSdat$labels$varName == var, "format"] <- "F10.0"
  }

  GADSdat
}
