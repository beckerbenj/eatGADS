####
#############################################################################
#' Assimilate value labels.
#'
#' Assimilate all value labels of multiple variables as part of a \code{GADSdat}
#'  or \code{all_GADSdat} object.
#'
#' Assimilation can be performed using all existing value labels or a lookup
#'  table containing at least all existing value labels. Missing tags are
#'  reused based on the meta data of the first variable in \code{varNames}.
#'  If a \code{lookup} is supplied, its order of value labels as well as its
#'  \code{missings} tags are prioritized over the meta data of the variables.
#'
#' @param GADSdat A \code{GADSdat} object.
#' @param varNames Character string of the variable name(s).
#' @param lookup Optional lookup \code{data.frame} with columns \code{value},
#'  \code{valLabel}, and \code{missings}.
#'
#' @return Returns the \code{GADSdat} object with changed meta data and recoded values.
#'
#' @examples
#' # Example data set
#' facs_df <- data.frame(id = 1:3, fac1 = c("Eng", "Aus", "Ger"),
#'                       fac2 = c("Ger", "Franz", "Ita"),
#'                       fac3 = c("Kor", "Chi", "Alg"),
#'                       stringsAsFactors = TRUE)
#' facs_gads <- import_DF(facs_df)
#'
#' assimilateValLabels(facs_gads, varNames = paste0("fac", 1:3))
#'
#' @export
assimilateValLabels <- function(GADSdat, varNames, lookup = NULL) {
  UseMethod("assimilateValLabels")
}

#' @export
assimilateValLabels.GADSdat <- function(GADSdat, varNames, lookup = NULL) {
  check_GADSdat(GADSdat)
  check_vars_in_GADSdat(GADSdat, vars = varNames)

  # extract data of variables in form of their value labels
  suppressMessages(fac_gads <- extractVars(GADSdat, vars = varNames))
  suppressWarnings(fac_df <- extractData(fac_gads, convertMiss = FALSE,
                                         convertLabels = "character"))

  ## use lookup table if supplied ##
  master_lookup <- data.frame(value = NULL, valLabel = NULL, missings = NULL)
  master_colnames <- c("value", "valLabel", "missings")
  if (!is.null(lookup)) {
    if (!is.data.frame(lookup) || !all(master_colnames %in% names(lookup))) {
      stop("'lookup' needs to be a data.frame with columns 'value', 'valLabel', and 'missings'.")
    }
    master_lookup <- select_occurring(lookup[, master_colnames], fac_df)
  }

  ## add missings of first variable ##
  meta_first_var <- extractMeta(GADSdat, varNames[[1]])
  missings_first_var <- !is.na(meta_first_var$valLabel) & meta_first_var$missings == "miss"
  if (any(missings_first_var)) {
    lookup_miss_first_var <- data.frame(value = meta_first_var$value[missings_first_var],
                                        valLabel = meta_first_var$valLabel[missings_first_var],
                                        missings = "miss")
    lookup_miss_first_var <- select_occurring(lookup_miss_first_var, fac_df)
  } else {
    lookup_miss_first_var <- data.frame(value = NULL, valLabel = NULL, missings = NULL)
  }
  # check for conflicts between first var missings and user-supplied lookup
  lookup_conflicts <- master_lookup$valLabel %in% lookup_miss_first_var$valLabel &
    master_lookup$missings == "valid"
  if (any(lookup_conflicts)) {
    warning("Some values marked as valid in 'lookup' are marked as missing in ",
            varNames[[1]], ". The tag in 'lookup' is being prioritized for these values: ",
            paste0(master_lookup$value[lookup_conflicts], collapse = ", "))
    # discard conflicting rows from first var missings
    conflicting_missings <- lookup_miss_first_var$valLabel %in% master_lookup$valLabel[lookup_conflicts]
    lookup_miss_first_var <- lookup_miss_first_var[!conflicting_missings, ]
  }
  master_lookup <- rbind(master_lookup, lookup_miss_first_var)

  ## add other values ##
  meta_all_vars <- select_occurring(extractMeta(GADSdat, varNames)[, master_colnames], fac_df)
  labels_all_vars <- unique(meta_all_vars$valLabel)
  labels_all_vars <- labels_all_vars[!is.na(labels_all_vars)]
  labels_not_yet_in_lookup <- labels_all_vars[!labels_all_vars %in% master_lookup$valLabel]
  additional_lookup <- data.frame(value = seq_along(labels_not_yet_in_lookup),
                                  valLabel = sort(labels_not_yet_in_lookup),
                                  missings = "valid")
  # increment values if positions in the value range are already taken
  if (any(additional_lookup$value %in% lookup_miss_first_var$value)) {
    warning("Some missing values are now inside the assimilated value range.")
  }
  while (any(additional_lookup$value %in% master_lookup$value)) {
    # pointers
    p_affected <- additional_lookup$value %in% master_lookup$value
    p_first_affected <- which(p_affected)[[1]]
    p_nonaffected <- which(!p_affected)[which(!p_affected) > p_first_affected]
    p_move <- p_first_affected:length(additional_lookup$value)
    # values
    if (length(p_nonaffected) > 0) {
      v_next_nonaffected <- additional_lookup$value[min(p_nonaffected)]
    } else {
      v_next_nonaffected <- max(additional_lookup$value) + 1
    }
    additional_lookup$value[p_move] <- seq(from = v_next_nonaffected,
                                           length.out = length(p_move))
  }
  master_lookup <- rbind(master_lookup, additional_lookup)

  ## recode labels to common values ##
  # remove unnecessary labels to avoid warning from import_DF
  for (this_var in varNames) {
    attr(fac_df[, this_var], "label") <- NULL
  }
  char_gads <- import_DF(fac_df)
  newVars_gads <- multiChar2fac(char_gads, vars = namesGADS(char_gads),
                                var_suffix = "", label_suffix = "")

  ## apply master lookup to recoded value ##
  multiChar_change_table <- getChangeMeta(newVars_gads, level = "value")
  names(master_lookup) <- c("value_new", "valLabel", "missings_new")
  assimilation_change_table <- merge(x = multiChar_change_table[, 1:4],
                                     y = master_lookup,
                                     by = "valLabel",
                                     all.x = TRUE,
                                     all.y = FALSE)
  assimilation_change_table$valLabel_new <- NA
  assimilation_change_table <- assimilation_change_table[, names(multiChar_change_table)]

  newVars_gads_assimilated <- applyChangeMeta(assimilation_change_table, newVars_gads)

  # merge data into original GADSdat
  suppressMessages(GADSdat_removed <- removeVars(GADSdat, vars = varNames))
  GADSdat_unorderd <- cbind(GADSdat_removed, newVars_gads_assimilated)
  GADSdat_out <- orderLike(GADSdat_unorderd, newOrder = namesGADS(GADSdat))

  return(GADSdat_out)
}

#' @export
assimilateValLabels.all_GADSdat <- function(GADSdat, varNames, lookup = NULL) {
  stop("This method has not been implemented yet")
}


select_occurring <- function(lookup, data) {
  valLabel_in_data <- lookup$valLabel %in% unlist(data)
  out <- lookup[valLabel_in_data, ]
}
