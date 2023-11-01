#### Apply recode lookup table
#############################################################################
#' Recode via lookup table.
#'
#' Recode one or multiple variables based on a lookup table created via \code{\link{createLookup}}
#' (and potentially formatted by \code{\link{collapseColumns}}).
#'
#' If there are missing values in the column \code{value_new}, \code{NAs} are inserted as new values
#' and a \code{warning} is issued.
#'
#' The complete work flow when using a lookup table to recode multiple variables in a \code{GADSdat} could be:
#' (0) optional: Recode empty strings to \code{NA} (necessary, if the look up table is written to excel).
#' (1) create a lookup table with \code{\link{createLookup}}.
#' (2) Save the lookup table to \code{.xlsx} with \code{write_xlsx} from \code{eatAnalysis}.
#' (3) fill out the lookup table via \code{Excel}.
#' (4) Import the lookup table back to \code{R} via \code{read_excel} from \code{readxl}.
#' (5) Apply the final lookup table with \code{applyLookup}.
#'
#' See \code{\link{applyLookup_expandVar}} for recoding a single variable into multiple variables.
#'
#'@param GADSdat A \code{GADSdat} object.
#'@param lookup Lookup table created by \code{\link{createLookup}} and - if necessary -  collapsed by \code{\link{collapseColumns}}.
#'Column names must be \code{c("variable", "value", "value_new")}.
#'@param suffix Suffix to add to the existing variable names. If \code{NULL}, the old variables will be overwritten.
#'
#'@return Returns a recoded \code{GADSdat}.
#'
#'@examples
#'## create an example GADSdat
#'iris2 <- iris
#'iris2$Species <- as.character(iris2$Species)
#'gads <- import_DF(iris2)
#'
#'## create Lookup
#'lu <- createLookup(gads, recodeVars = "Species")
#'lu$value_new <- c("plant 1", "plant 2", "plant 3")
#'
#'## apply lookup table
#'gads2 <- applyLookup(gads, lookup = lu, suffix = "_r")
#'
#'## only recode some values
#'lu2 <- createLookup(gads, recodeVars = "Species")
#'lu2$value_new <- c("plant 1", "plant 2", NA)
#'gads3 <- applyLookup(gads, lookup = lu2, suffix = "_r")
#'
#'@export
applyLookup <- function(GADSdat, lookup, suffix = NULL) {
  UseMethod("applyLookup")
}

#'@import data.table

#'@export
applyLookup.GADSdat <- function(GADSdat, lookup, suffix = NULL) {
  check_GADSdat(GADSdat)
  check_lookup(lookup, GADSdat)

  rec_vars <- unique(lookup[["variable"]])

  GADSdat2 <- GADSdat

  ## recode via data.table
  for(nam in rec_vars) {
    #rec_df <- data.table::as.data.table(GADSdat2$dat)
    rec_df <- GADSdat2$dat
    lookup <- as.data.frame(lookup)

    sub_lu <- lookup[lookup$variable == nam, c("value", "value_new")]
    names(sub_lu) <- c(nam, "value_new")
    if(is.numeric(GADSdat$dat[, nam])) {
      suppressWarnings(sub_lu <- eatTools::asNumericIfPossible(sub_lu, force.string = FALSE))
    }

    suppressWarnings(test <- compare_and_order(rec_df[[nam]], set2 = sub_lu[[nam]]))
    if(length(test$not_in_set1) != 0) {
      warning("For variable ", nam, " the following values are in the lookup table but not in the data: ",
              paste(test$not_in_set1, collapse = ", "))
    }
    if(length(test$not_in_set2) != 0) {
      warning("For variable ", nam, " the following values are in the data but not in the lookup table: ",
              paste(test$not_in_set2, collapse = ", "))
    }
    if(length(test$not_in_set2) != 0 && "" %in% test$not_in_set2) {
      warning("Empty strings are values in the data but not in the look up table. Using recodeString2NA() is recommended.")
    }

    old_nam <- nam
    if(!is.null(suffix)) {
      nam <- paste0(nam, suffix)
      rec_df[[nam]] <- rec_df[[old_nam]] # initialize variable to allow incomplete recodes
      names(sub_lu)[1] <- nam
    }
    rec_dt <- data.table::as.data.table(rec_df)
    rec_dt[sub_lu, on = nam, (nam) := i.value_new]

    if(all(is.na(rec_dt[[nam]]))) {
      warning("In the new variable ", nam, " all values are missing, therefore the variable is dropped. If this behaviour is not desired, contact the package author.")
      rec_dt[[nam]] <- NULL
      GADSdat2 <- updateMeta(GADSdat2, newDat = as.data.frame(rec_dt, stringsAsFactor = FALSE))
    } else{
      GADSdat2 <- updateMeta(GADSdat2, newDat = as.data.frame(rec_dt, stringsAsFactor = FALSE))
      GADSdat2 <- reuseMeta(GADSdat = GADSdat2, varName = nam, other_GADSdat = GADSdat, other_varName = old_nam)
    }
  }

  check_GADSdat(GADSdat2)
  GADSdat2
}


check_lookup <- function(lookup, GADSdat) {
  if(!all(lookup$variable %in% namesGADS(GADSdat))) {
    stop("Some of the variables are not variables in the GADSdat.")
  }
  if(!identical(names(lookup), c("variable", "value", "value_new"))) {
    stop("'lookup' table has to be formatted correctly.")
  }
  if(all(is.na(lookup$value_new))) {
    stop("No values have a recode value assigned (missings in value_new).")
  }
  by(lookup, lookup$variable, function(sub_lookup) {
    dup_values <- sub_lookup$value[duplicated(sub_lookup$value)]
    if(length(dup_values) > 0) {
      varName <- sub_lookup[1, "variable"]
      stop("There are duplicate values in the lookup for variable '", varName,"': ",
           paste(dup_values, collapse = ", "))
    }
  })
  if(any(is.na(lookup$value_new))) {
    warning("Not all values have a recode value assigned (missings in value_new).")
  }
  NULL
}
