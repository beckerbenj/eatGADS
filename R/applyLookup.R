#### Apply recode lookup table
#############################################################################
#' Recode via lookup table.
#'
#' Recode one or multiple variables based on a lookup table created via \code{\link{createLookup}}.
#'
#' tbd
#'
#'@param GADSdat A \code{GADSdat} object.
#'@param lookup Lookup table created by \code{\link{createLookup}} and - if necessary -  collapsed by \code{\link{collapseColumns}}. Column names should be \code{c("variable", "value", "value_new")}.
#'@param suffix Suffix to add to the existing variable names. If \code{NULL} the old variables will be overwritten.
#'
#'@return Returns a recoded \code{GADSdat}.
#'
#'@examples
#'#to be done
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
    if(length(test$not_in_set1) != 0) warning("For variable ", nam, " the following values are in the lookup table but not in the data: ", paste(test$not_in_set1, collapse = ", "))
    if(length(test$not_in_set2) != 0) warning("For variable ", nam, " the following values are in the data but not in the lookup table: ", paste(test$not_in_set2, collapse = ", "))

    old_nam <- nam
    if(!is.null(suffix)) {
      nam <- paste0(nam, suffix)
      rec_df[[nam]] <- rec_df[[old_nam]] # initialise variable to allow incomplete recodes
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
  if(!all(lookup$variable %in% namesGADS(GADSdat))) stop("Some of the variables are not variables in the GADSdat.")
  if(!identical(names(lookup), c("variable", "value", "value_new"))) stop("LookUp table has to be formatted correctly.")
  if(sum(is.na(lookup$value)) > 1) stop("In more than 1 row value is missing.")
  if(all(is.na(lookup$value_new))) stop("All values have no recode value assigned (missings in value_new).")
  if(any(is.na(lookup$value_new))) warning("Some values have no recode value assigned (missings in value_new).")
}
