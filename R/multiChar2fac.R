#### Multiple Strings to Labeled Variables
#############################################################################
#' Transform one or multiple character variables to factor.
#'
#' Convert one or multiple character variables to factors. If multiple variables are converted, a common set of value labels is created,
#'  which is identical across variables. Existing value labels are preserved.
#'
#' If a set of variables has the same possible values, it is desirable that these variables share the same
#' value labels, even if some of the values do not occur on the individual variables. This function allows
#' the transformation of multiple character variables to factors while assimilating the value labels.
#' The SPSS format of the newly created variables is set to \code{F10.0}.
#'
#' A current limitation of the function is that prior to the conversion, all variables specified in \code{vars} must have identical
#' meta data on value level (value labels and missing tags).
#'
#' If necessary, missing codes can be set after transformation via \code{\link{checkMissings}} for setting missing codes
#' depending on value labels for all variables or
#' \code{\link{changeMissings}} for setting missing codes for specific values in a specific variable.
#'
#' The argument \code{convertCases} uses the function \code{\link{convertCase}} internally. See the respective documentation for more details.
#'
#'@param GADSdat A \code{data.frame} or \code{GADSdat} object.
#'@param vars A character vector with all variables that should be transformed to factor.
#'@param var_suffix Variable suffix for the newly created \code{GADSdat}. If an empty character, the existing variables are overwritten.
#'@param label_suffix Suffix added to variable label for the newly created variable in the \code{GADSdat}.
#'@param convertCases Should cases be transformed for all variables? Default \code{NULL} leaves cases as they are.
#'Available options for converting cases are all lower case (\code{'lower'}), all upper case (\code{'upper'}),
#'or first letter upper case, everything else lower case (\code{'upperFirst'}).
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
#'## transform one character variable
#'gads2 <- multiChar2fac(gads, vars = "citizenship1")
#'
#'## transform multiple character variables
#'gads2 <- multiChar2fac(gads, vars = c("citizenship1", "citizenship2"))
#'
#'## set values to missings
#'gads3 <- checkMissings(gads2, missingLabel = "missing")
#'
#'@export
multiChar2fac <- function(GADSdat, vars, var_suffix = "_r", label_suffix = "(recoded)", convertCases = NULL) {
  UseMethod("multiChar2fac")
}

#'@export
multiChar2fac.GADSdat <- function(GADSdat, vars, var_suffix = "_r", label_suffix = "(recoded)", convertCases = NULL) {
  check_GADSdat(GADSdat)
  if(!is.character(vars) && length(vars) > 0) stop("vars needs to be a character vector of at least length 1.")
  check_vars_in_GADSdat(GADSdat, vars)
  all_content <- unlist(GADSdat$dat[, vars])
  if(all(is.na(all_content))) {
    stop("Variables in 'vars' contain only NAs. Transforming to factor is not meaningful.")
  }

  ## current limitation: for multiple vars, meta data must be equal (maybe extend later?)
  if(length(vars) > 1) {
    for(nam in vars[-1]) {
      variables_names_in_error <- paste0("variables '", vars[1], "' and '", nam, "'")
      compare_meta_value_level(GADSdat, varName1 = vars[1], varName2 = nam, argumentName = variables_names_in_error)
    }
  }

  # convert case if specified
  if(!is.null(convertCases)) {
    if(!is.character(convertCases) || length(convertCases) != 1) stop("'convertCases' must be a character of length 1.")
    if(!convertCases %in% c("lower", "upper", "upperFirst")) stop("'convertCases' must one of c('lower', 'upper', 'upperFirst').")
    GADSdat <- convertCase(GADSdat, case = convertCases, vars = vars)
  }

  # circumvent existing value labels & missing tags: remove these values so no new value labels are used for them
  suppressMessages(only_vars_gads <- extractVars(GADSdat, vars = vars))
  existing_meta <- extractMeta(only_vars_gads, vars[1])
  existing_meta <- existing_meta[!is.na(existing_meta$value), ]
  existing_labels <- existing_meta[, "value"]
  for(var in namesGADS(only_vars_gads)) {
    only_vars_gads$dat[only_vars_gads$dat[, var] %in% existing_labels, var] <- NA
  }

  # create common lookup table as combination of existing meta data and new meta data derived from data entries
  suppressWarnings(df_no_meta <- extractData(only_vars_gads))
  all_levels <- unique(unlist(lapply(df_no_meta, function(x) x)))
  all_levels_sorted <- sort(all_levels[!is.na(all_levels)])
  all_levels_lookup <- data.frame(value = all_levels_sorted,
                       value_new = seq_but_skip(to = length(all_levels_sorted), skip = c(unique(existing_labels))))
  #existing_meta2 <- existing_meta[, c("valLabel", "value")]
  #names(existing_meta2) <- c("value", "value_new")
  #all_levels_lookup_with_existing_meta <- rbind(all_levels_lookup, existing_meta2)
  #all_levels_lookup_with_existing_meta <- all_levels_lookup_with_existing_meta[order(all_levels_lookup_with_existing_meta$value_new), ]

  for(nam in vars) {
    old_nam <- nam
    new_nam <- paste0(nam, var_suffix)
    specific_lookup <- data.frame(variable = old_nam, all_levels_lookup, stringsAsFactors = FALSE)

    # applyLookUp automatically transfers the existing meta data!
    GADSdat <- suppressMessages(suppressWarnings(applyLookup(GADSdat, lookup = specific_lookup, suffix = var_suffix)))
    GADSdat$dat[, new_nam] <- as.numeric(GADSdat$dat[, new_nam])

    GADSdat <- changeValLabels(GADSdat, varName = new_nam, value = specific_lookup$value_new, valLabel = specific_lookup$value)
    GADSdat <- append_varLabel(GADSdat, varName = new_nam, label_suffix = label_suffix)
    GADSdat$labels[GADSdat$labels$varName == new_nam, "format"] <- "F10.0"
  }

  GADSdat
}
