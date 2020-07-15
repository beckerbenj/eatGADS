#### Apply recode lookup table with input 1 variable output multiple variables
#############################################################################
#' Recode via lookup table into multiple variables.
#'
#' Recode one or multiple variables based on a lookup table created via \code{\link{createLookup}}. In contrast to \code{\link{applyLookup}},
#' this function allows the creation of multiple resulting variables from a single input variable.
#'
#' tbd
#'
#'@param GADSdat A \code{GADSdat} object.
#'@param lookup Lookup table created by \code{\link{createLookup}}.
#'
#'@return Returns a recoded \code{GADSdat}.
#'
#'@examples
#'#to be done
#'
#'@export
applyLookup_expandVar <- function(GADSdat, lookup) {
  UseMethod("applyLookup_expandVar")
}

#'@export
applyLookup_expandVar.GADSdat <- function(GADSdat, lookup) {
  check_GADSdat(GADSdat)

  GADSdat_new <- GADSdat
  recode_colnames <- names(lookup)[3:ncol(lookup)]
  for(i in seq(recode_colnames)) {
    # 1) divide lookup table
    single_lookup <- lookup[, c(1, 2, 2 + i)]
    names(single_lookup)[3] <- "value_new"
    if(i == 1) check_lookup(single_lookup, GADSdat = GADSdat)

    # 2) apply recode; new variable with number as suffix?
    suppressWarnings(GADSdat_new <- applyLookup(GADSdat_new, lookup = single_lookup, suffix = paste0("_", i)))
  }

  new_varname_order <- sapply(unique(lookup$variable), function(x) paste(x, seq(recode_colnames), sep = "_"))
  GADSdat_new <- orderLike(GADSdat_new, newOrder = c(namesGADS(GADSdat), new_varname_order))

  check_GADSdat(GADSdat_new)
  GADSdat_new
}
