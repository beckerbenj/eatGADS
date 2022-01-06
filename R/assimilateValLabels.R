####
#############################################################################
#' Assimilate value labels.
#'
#' Assimilate all value labels of multiple variables as part of a \code{GADSdat} or \code{all_GADSdat} object.
#'
#' Assimilation can be performed using all existing value labels or a look up table containing at least all existing value labels.
#' Missing codes are reused based on the meta data of the first variable in \code{varNames}.
#'
#'@param GADSdat \code{GADSdat} object imported via \code{eatGADS}.
#'@param varNames Character string of a variable name.
#'@param lookup Look up \code{data.frame}.
#'
#'@return Returns the \code{GADSdat} object with changed meta data and recoded values.
#'
#'@examples
#'# Example data set
#' facs_df <- data.frame(id = 1:3, fac1 = c("Eng", "Aus", "Ger"),
#'                       fac2 = c("Ger", "Franz", "Ita"),
#' fac3 = c("Kor", "Chi", "Alg"), stringsAsFactors = TRUE)
#' facs_gads <- import_DF(facs_df)
#'
#' assimilateValLabels(facs_gads, varNames = paste0("fac", 1:3))
#'
#'@export
assimilateValLabels <- function(GADSdat, varNames, lookup = NULL) {
  UseMethod("assimilateValLabels")
}
#'@export
assimilateValLabels.GADSdat <- function(GADSdat, varNames, lookup = NULL) {
  check_vars_in_GADSdat(GADSdat, vars = varNames)

  # extract GADSdat only including variables
  suppressMessages(fac_GADS <- extractVars(GADSdat, vars = varNames))

  # apply value labels
  fac_df <- extractData(fac_GADS, convertMiss = FALSE, convertLabels = "character")

  # (if lookup does not exist, create it from all variables)
  # (if lookup exists, check whether all values are mentioned in lookup)
  if(!is.null(lookup)) stop("Lookup argument is currently not supported.")
  if(is.null(lookup)) {
    lookup_vec <- extractMeta(GADSdat, varNames[1])[, "valLabel"]
    lookup <- data.frame(valLabel = lookup_vec, value = seq_along(lookup_vec))
  }

  lookup_valLabels <- lookup$valLabel[!is.na(lookup$valLabel)]

  #if(length(lookup_valLabels) > nrow(fac_df)) stop("'lookups' with longer length than rows in the data are currently not supported.")
  lookup_helper <- resize_vector(lookup_valLabels, max_nrow = nrow(GADSdat$dat))
  fac_df2 <- cbind(fac_df, lookup_helper)

  # multiChar2fac on all resulting variables
  char_gads <- import_DF(fac_df2)
  newVars_gads <- multiChar2fac(char_gads, vars = namesGADS(char_gads), var_suffix = "", label_suffix = "")
  suppressMessages(newVars_gads_red <- extractVars(newVars_gads, vars = varNames))

  # merge data and meta data into old GADSdat
  suppressMessages(GADSdat_removed <- removeVars(GADSdat, vars = varNames))
  #browser()
  # cbind Method, orderlike
  GADSdat_unorderd <- cbind(GADSdat_removed, newVars_gads_red)
  GADSdat_out <- orderLike(GADSdat_unorderd, newOrder = namesGADS(GADSdat))

  # restore missing value labels?
  # maybe using recodeGADSdat and the first variable in varNames?
  miss_old <- extractMeta(GADSdat, varNames[1])
  miss_old <- miss_old[which(miss_old$missings == "miss"), c("value", "valLabel")]

  #browser()

  if(nrow(miss_old) > 0){
    if(any(is.na(miss_old$valLabel))) stop("Missing values in 'valLabel' for declared missings.")
    miss_new <- extractMeta(GADSdat_out, varNames[1])
    miss_new <- miss_new[which(miss_new$valLabel %in% miss_new$valLabel), c("value", "valLabel")]

    miss_rec <- merge(miss_old, miss_new, by = "valLabel")

    for(nam in varNames) {
      GADSdat_out <- recodeGADS(GADSdat_out, varName = nam, oldValues = miss_rec$value.y, newValues = miss_rec$value.x)
    }
    #browser()
  }

  ## tbd:
  # is lookup a specific lookup or just a list of character entries
  # restore specific value label order?

  GADSdat_out
}

#'@export
changeValLabels.all_GADSdat <- function(GADSdat, varName, value, valLabel) {
  stop("This method has not been implemented yet")
}

# restructure a vector into a data.frame with maximum row number (and possibly more columns)
resize_vector <- function(vec, max_nrow) {
  stopifnot(is.numeric(max_nrow) && length(max_nrow) == 1)
  stopifnot(is.vector(vec))

  suppressWarnings(mat <- matrix(vec, nrow = max_nrow))
  as.data.frame(mat)
}

# resize_vector2 <- function(vec, max_nrow) {
#   stopifnot(is.numeric(max_nrow) && length(max_nrow) == 1)
#   stopifnot(is.vector(vec))
#
#   vec_len <- length(vec)
#   if(vec_len <= max_nrow) return(data.frame(vec, stringsAsFactors = FALSE))
#
#   out <- data.frame(pre_length = numeric(max_nrow))
#   while(vec_len > 0) {
#     # add NAs if necessary
#     if(vec_len < max_nrow) vec <- c(vec, rep(NA, max_nrow - vec_len))
#
#     pick_ind <- seq(max_nrow)
#     out <- data.frame(out, vec[pick_ind])
#     vec <- vec[-pick_ind]
#     vec_len <- length(vec)
#   }
#   out[, -1]
# }
