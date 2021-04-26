#### Apply recode lookup table with input 1 variable output multiple variables
#############################################################################
#' Recode via lookup table into multiple variables.
#'
#' Recode one or multiple variables based on a lookup table created via \code{\link{createLookup}}.
#' In contrast to \code{\link{applyLookup}}, this function allows the creation of multiple resulting
#' variables from a single input variable. All variables in \code{lookup} except
#' \code{variable} and \code{value} are treated as recode columns.
#'
#' If a variable contains information that should be split into multiple variables via manual recoding,
#' \code{applyLookup_expandVar} can be used. If there are missing values in any recode column,
#' \code{NAs} are inserted as new values. A \code{warning} is issued only for the first column.
#'
#' The complete work flow when using a lookup table to expand variables in a \code{GADSdat} based on manual recoding could be:
#' (1) create a lookup table with \code{\link{createLookup}}.
#' (2) Save the lookup table to \code{.xlsx} with \code{write_xlsx} from \code{eatAnalysis}.
#' (3) fill out the lookup table via \code{Excel}.
#' (4) Import the lookup table back to \code{R} via \code{read_excel} from \code{readxl}.
#' (5) Apply the final lookup table with \code{applyLookup_expandVar}.
#'
#' See \code{\link{applyLookup}} for simply recoding variables in a \code{GADSdat}.
#'
#'@param GADSdat A \code{GADSdat} object.
#'@param lookup Lookup table created by \code{\link{createLookup}}.
#'
#'@return Returns a recoded \code{GADSdat}.
#'
#'@examples
#'## create an example GADSdat
#'example_df <- data.frame(ID = 1:6,
#'                         citizenship = c("germ", "engl", "germ, usa", "china",
#'                                         "austral, morocco", "nothin"),
#'                         stringsAsFactors = FALSE)
#'gads <- import_DF(example_df)
#'
#'## create Lookup
#'lu <- createLookup(gads, recodeVars = "citizenship", addCol = c("cit_1", "cit_2"))
#'lu$cit_1 <- c("German", "English", "German", "Chinese", "Australian", NA)
#'lu$cit_2 <- c(NA, NA, "USA", NA, "Morocco", NA)
#'
#'## apply lookup table
#'gads2 <- applyLookup_expandVar(gads, lookup = lu)
#'
#'@export
applyLookup_expandVar <- function(GADSdat, lookup) {
  UseMethod("applyLookup_expandVar")
}

#'@export
applyLookup_expandVar.GADSdat <- function(GADSdat, lookup) {
  check_GADSdat(GADSdat)
  #if("new_value1" %in% names(lookup) && -99 %in% lookup$new_value1) browser()

  GADSdat_new <- GADSdat
  recode_colnames <- names(lookup)[3:ncol(lookup)]
  for(i in seq(recode_colnames)) {
    # 1) divide lookup table
    single_lookup <- lookup[, c(1, 2, 2 + i)]
    names(single_lookup)[3] <- "value_new"

    # 2) apply recode; new variable with number as suffix?
    # check first recode column more thoroughly than later columns (same warning as in check_lookup)
    #if(i == 1) check_lookup(single_lookup, GADSdat = GADSdat)
    if(i == 1) {
      GADSdat_new <- applyLookup(GADSdat_new, lookup = single_lookup, suffix = paste0("_", i))
    }
    else {suppressWarnings(GADSdat_new <- applyLookup(GADSdat_new, lookup = single_lookup, suffix = paste0("_", i)))}
  }

  new_varname_order <- sapply(unique(lookup$variable), function(x) paste(x, seq(recode_colnames), sep = "_"))
  GADSdat_new <- orderLike(GADSdat_new, newOrder = c(namesGADS(GADSdat), new_varname_order))

  check_GADSdat(GADSdat_new)
  GADSdat_new
}

######### deprecated, check_lookup is used for first column ###################
check_lookup_expandVar <- function(lookup, GADSdat) {
  # checks as in check_lookup
  if(!all(lookup$variable %in% namesGADS(GADSdat))) stop("Some of the variables are not variables in the GADSdat.")
  if(!identical(names(lookup)[1:2], c("variable", "value"))) stop("'lookup' table has to be formatted correctly.")
  if(sum(is.na(lookup$value)) > 1) stop("In more than 1 row value is missing.")
  if(any(is.na(lookup[, 3]))) warning("Not all values have a recode value assigned in new value column 1 (missings in new values).")
}
