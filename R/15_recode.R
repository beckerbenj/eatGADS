#### Write xlsx for recoding
#############################################################################
#' Extract values for recoding.
#'
#' Extract values from variables of a \code{GADSdat} object for recoding.
#'
#' If recoding of one or multiple variables is more complex, a lookup table can be created for later importing via \code{eatGADS}. The function allows the extraction of the values of multiple variables and sorting of these unique values via variable or values.
#'
#'@param GADSdat A \code{GADSdat} object.
#'@param recodeVars Character vector of variable names which should be recoded.
#'@param sort_by By which column should the long format data.frame be sorted? If \code{NULL}, no sorting is performed.
#'@param addCols Character vector of additional column names for recoding purposes.
#'
#'@return Returns a data frame in long format including all unique values of the variables in \code{recodeVars}.
#'
#'@examples
#'#to be done
#'
#'@export
createLookup <- function(GADSdat, recodeVars, sort_by = NULL, addCols = c("value_new")) {
  UseMethod("createLookup")
}

#'@export
createLookup.GADSdat <- function(GADSdat, recodeVars, sort_by = NULL, addCols = c("value_new")) {
  check_GADSdat(GADSdat)
  if(!is.character(recodeVars) && length(recodeVars) > 0) stop("recodeVars needs to be a character vector of at least length 1.")
  if(!all(recodeVars %in% namesGADS(GADSdat))) stop("Some of the variables are not variables in the GADSdat.")
  vars_w <- data.table::as.data.table(GADSdat$dat[, recodeVars, drop = FALSE])
  dt_l <- unique(data.table::melt(vars_w, measure.vars = recodeVars, variable.factor = FALSE, value.factor = FALSE))

  if(!all(sort_by %in% names(dt_l))) stop("data.frame can only be sorted by 'variable' or 'value' or both.")
  if(!is.null(sort_by)) {
    data.table::setorderv(dt_l, cols = sort_by, order=1L, na.last=FALSE)
  }

  vars_l <- data.frame(dt_l, stringsAsFactors = FALSE)
  stopifnot(length(addCols) >= 1)
  for(i in addCols) vars_l[, i] <- NA

  vars_l
}


#### Collapse multiple recodings
#############################################################################
#' Collapse columns.
#'
#' Collapse columns of a lookup table created by \code{\link{createLookup}}.
#'
#' tbd
#'
#'@param lookup A lookup table \code{data.frame} as created via \code{\link{createLookup}}.
#'@param recodeVars Character vector of variable names which should be collapsed.
#'@param prioritize Which of the variables should be prioritized, if multiple values are available?
#'
#'@return Returns a data frame that can be used for \code{\link{applyLookup}}.
#'
#'@examples
#'#to be done
#'
#'@export
collapseColumns <- function(lookup, recodeVars, prioritize) {
  if(length(recodeVars) != 2) stop("More recode variables than 2 are currently not supported.")
  if(length(prioritize) != 1) stop("Prioritize must be of length = length(recodeVars) - 1.")
  lookup[, "value_new"] <- ifelse(is.na(lookup[[prioritize]]),
                                 yes = lookup[[recodeVars[!recodeVars %in% prioritize]]],
                                 no = lookup[[prioritize]])
  lookup[, names(lookup)[!names(lookup) %in% recodeVars]]
}


#### Apply recode lookup table
#############################################################################
#' Recoade via lookup table.
#'
#' Recode one or multiple variables based on a lookup table created via \code{\link{createLookup}}.
#'
#' tbd
#'
#'@param GADSdat A \code{GADSdat} object.
#'@param lookup Lookup table created by \code{\link{createLookup}} and - if necessary -  collapsed by \code{\link{collapseColumns}}.
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
    rec_dt <- data.table::as.data.table(GADSdat2$dat)

    sub_lu <- lookup[lookup$variable == nam, c("value", "value_new")]
    names(sub_lu) <- c(nam, "value_new")
    suppressWarnings(sub_lu <- eatTools::asNumericIfPossible(sub_lu, force.string = FALSE))

    old_nam <- nam
    if(!is.null(suffix)) {
      nam <- paste0(nam, suffix)
      rec_dt[sub_lu, on = old_nam, (nam) := i.value_new]
    } else {
      rec_dt[sub_lu, on = nam, (nam) := i.value_new]
    }

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
  if(any(is.na(lookup$value))) stop("In some rows there are missings in column value.")
  if(all(is.na(lookup$value_new))) stop("All values have no recode value assigned (missings in value_new).")
  if(any(is.na(lookup$value_new))) warning("Some values have no recode value assigned (missings in value_new).")
}


#### Collapse an MC and a text variable.
#############################################################################
#' Recode MC variable based on text.
#'
#' Use an additional text variable to recode an existing MC variable.
#'
#' to be written
#'
#'@param GADSdat A \code{GADSdat} object.
#'@param mc_var A single variable name of the multiple choice variable.
#'@param text_var A single variable name of the text variable.
#'@param mc_code4text The value in the MC variable that signals that information from the text variable should be used.
#'@param var_suffix Variable suffix for the newly created \code{GADSdat}.
#'@param label_suffix Suffix added to variable label for the newly created variable in the \code{GADSdat}.
#'
#'@return Returns a \code{GADSdat} containing the newly computed variable.
#'
#'@examples
#'#to be done
#'
#'@export
collapseMC_Text <- function(GADSdat, mc_var, text_var, mc_code4text, var_suffix = "_r", label_suffix = "(recoded)") {
  UseMethod("collapseMC_Text")
}

#'@export
collapseMC_Text.GADSdat <- function(GADSdat, mc_var, text_var, mc_code4text, var_suffix = "_r", label_suffix = "(recoded)") {
  if(!mc_var %in% namesGADS(GADSdat)) stop("mc_var is not a variable in the GADSdat.")
  if(!text_var %in% namesGADS(GADSdat)) stop("text_var is not a variable in the GADSdat.")

  mc_var_new <- paste0(mc_var, var_suffix)
  MC_dat <- GADSdat$dat[, mc_var, drop = FALSE]

  ## important for recoding: difference, is text variable has (a) missing code or (b) the missing code as string
  # if (a), other on mc stays, if (b), other on mc becomes missing
  suppressMessages(MC_gads <- updateMeta(GADSdat, MC_dat))
  MC <- extractData(MC_gads, convertMiss = TRUE, convertLabels = "character")[, 1]
  tex <- extractData(GADSdat)[[text_var]]
  MC_new <- ifelse(MC == mc_code4text | is.na(MC),
                   yes = ifelse(is.na(tex), yes = GADSdat$dat[[mc_var]], no = tex),
                   no = GADSdat$dat[[mc_var]])
  #cbind(MC, tex, MC_new)
  #if(all.equal(dim(GADSdat$dat), c(7, 2))) browser()

  ## work with lookup tables!
  # recode values from old variable
  lookup_oldValues <- data.frame(variable = mc_var_new,
                                 value = GADSdat$labels[GADSdat$labels$varName == mc_var, "valLabel"],
                                 value_new = GADSdat$labels[GADSdat$labels$varName == mc_var, "value"],
                                 stringsAsFactors = FALSE)

  GADSdat_dat <- cbind(GADSdat$dat, MC_new, stringsAsFactors = FALSE)
  names(GADSdat_dat)[ncol(GADSdat_dat)] <- mc_var_new
  GADSdat_dat2 <- updateMeta(GADSdat, GADSdat_dat)

  # use lookup tables
  suppressMessages(GADSdat_dat3 <- applyLookup(GADSdat_dat2, lookup = lookup_oldValues))

  # create and use lookup tables for new value levels
  add_values <- GADSdat_dat3$dat[!GADSdat_dat3$dat[, mc_var_new] %in% lookup_oldValues$value_new, mc_var_new]
  add_values_df <- data.frame(add_values, stringsAsFactors = TRUE)
  names(add_values_df) <- mc_var_new
  add_values_gads <- import_DF(add_values_df)
  max_old_value <- max(GADSdat$labels[GADSdat$labels$varName == mc_var & GADSdat$labels$missings == "valid", "value"])
  add_values_gads$labels$value <- add_values_gads$labels$value + max_old_value

  # recode new values from text variable
  lookup_newValues <- data.frame(variable = mc_var_new,
                                 value = add_values_gads$labels[, "valLabel"],
                                 value_new = add_values_gads$labels[, "value"],
                                 stringsAsFactors = FALSE)
  suppressMessages(GADSdat_dat4 <- applyLookup(GADSdat_dat3, lookup = lookup_newValues))
  GADSdat_dat4$dat[, mc_var_new] <- as.numeric(GADSdat_dat4$dat[, mc_var_new])

  ### insert right meta data (combine)
  GADSdat_out <- reuseMeta(GADSdat_dat4, other_GADSdat = GADSdat, varName = mc_var_new, other_varName = mc_var)
  GADSdat_out2 <- reuseMeta(GADSdat_out, other_GADSdat = add_values_gads, varName = mc_var_new, addValueLabels = TRUE)
  GADSdat_out3 <- append_varLabel(GADSdat = GADSdat_out2, varName = mc_var_new, label_suffix = label_suffix)

  check_GADSdat(GADSdat_out3)
  GADSdat_out3
}

## append a suffix to a variable label safely
append_varLabel <- function(GADSdat, varName, label_suffix) {
  stopifnot(length(label_suffix) == 1)
  if(nchar(label_suffix) == 0) return(GADSdat)
  old_varLabel <- extractMeta(GADSdat, varName)[1, "varLabel"]
  new_varLabel <- ifelse(is.na(old_varLabel), yes = label_suffix,
                         no = paste(old_varLabel, label_suffix, sep = " "))
  GADSdat_out <- changeVarLabels(GADSdat, varName = varName, varLabel =
                                    new_varLabel)
  GADSdat_out
}


