#### Write xlsx for recoding
#############################################################################
#' Recode values.
#'
#' Extract values form variables of \code{GADSdat} object and write to \code{xlsx} for manuel recoding.
#'
#' If recoding of one or multiple variables is more complex, a lookup table can be created for later importing via \code{eatGADS}.
#'
#'@param GADSdat A \code{GADSdat} object.
#'@param recodeVars Character vector of variable names which should be recoded.
#'@param addCols Character vector of additional column names for recoding purposes.
#'@param sort_by By which column should the long format data.frame be sorted? If \code{NULL}, no sorting is performed.
#'
#'@return Returns a data frame in long format including all unique values of the variables in \code{recodeVars}.
#'
#'@examples
#'\dontrun{
#'#to be done
#'
#'@export
createLookup <- function(GADSdat, recodeVars, sort_by = NULL, addCols = c("newValue")) {
  UseMethod("createLookup")
}

#'@export
createLookup.GADSdat <- function(GADSdat, recodeVars, sort_by = NULL, addCols = c("newValue")) {
  check_GADSdat(GADSdat)
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
#' Extract values form variables of \code{GADSdat} object and write to \code{xlsx} for manuel recoding.
#'
#' If recoding of one or multiple variables is more complex, a lookup table can be created for later importing via \code{eatGADS}.
#'
#'@param GADSdat A \code{GADSdat} object.
#'@param recodeVars Character vector of variable names which should be recoded.
#'@param addCols Character vector of additional column names for recoding purposes.
#'@param sort_by By which column should the long format data.frame be sorted? If \code{NULL}, no sorting is performed.
#'
#'@return Returns a data frame that can be used for \code{\link{applyLookup}}.
#'
#'@examples
#'\dontrun{
#'#to be done
#'
#'@export
collapseColumns <- function(lookUp, recodeVars, prioritize) {
  if(length(recodeVars) != 2) stop("More recode variables than 2 are currently not supported.")
  if(length(prioritize) != 1) stop("Prioritize must be of length = length(recodeVars) - 1.")
  lookUp[, "valueNew"] <- ifelse(is.na(lookUp[[prioritize]]),
                                 yes = lookUp[[recodeVars[!recodeVars %in% prioritize]]],
                                 no = lookUp[[prioritize]])
  lookUp[, names(lookUp)[!names(lookUp) %in% recodeVars]]
}


#### Apply recode lookup table
#############################################################################
#' Recoade via lookup table.
#'
#' Extract values form variables of \code{GADSdat} object and write to \code{xlsx} for manuel recoding.
#'
#' If recoding of one or multiple variables is more complex, a lookup table can be created for later importing via \code{eatGADS}.
#'
#'@param GADSdat A \code{GADSdat} object.
#'@param lookUp LookUp table created by \code{\link{createLookup}} and - if necessary -  collapsed by \code{\link{collapseColumns}}.
#'
#'@return Returns a recoded \code{GADSdat}.
#'
#'@examples
#'\dontrun{
#'#to be done
#'
#'@export
applyLookup <- function(GADSdat, lookup, suffix = "_r") {
  UseMethod("applyLookup")
}

#'@export
applyLookup.GADSdat <- function(GADSdat, lookup, suffix = "_r") {
  check_GADSdat(GADSdat)
  check_lookup(lookup, GADSdat)

  rec_vars <- unique(lookup[["variable"]])

  GADSdat2 <- GADSdat
  ## recode via data.table
  for(nam in rec_vars) {
    rec_dt <- data.table::as.data.table(GADSdat2$dat)

    sub_lu <- lookup[lookup$variable == nam, c("value", "newValue")]
    names(sub_lu) <- c(nam, "valueNew")
    sub_lu <- eatTools::asNumericIfPossible(sub_lu, force.string = FALSE)
    new_nam <- paste0(nam, suffix)
    rec_dt[sub_lu, on = nam, (new_nam) := i.valueNew]

    GADSdat2 <- updateMeta(GADSdat2, newDat = as.data.frame(rec_dt, stringsAsFactor = FALSE))
    GADSdat2 <- reuseMeta(GADSdat = GADSdat2, varName = new_nam, other_GADSdat = GADSdat, other_varName = nam)
  }

  check_GADSdat(GADSdat2)
  GADSdat2
}

check_lookup <- function(lookup, GADSdat) {
  if(!all(lookup$variable %in% namesGADS(GADSdat))) stop("Some of the variables are not variables in the GADSdat.")
  if(!identical(names(lookup), c("variable", "value", "newValue"))) stop("LookUp table has to be formatted correctly.")

  # tbd!!!!!!!!!!
}


#### Apply recode lookup table
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
#'@param suffix Variable suffix for the newly created GADSdat.
#'
#'@return Returns a \code{GADSdat} containing the newly computed variable.
#'
#'@examples
#'\dontrun{
#'#to be done
#'
#'@export
collapseMC_Text <- function(GADSdat, mc_var, text_var, mc_code4text, suffix = "_r") {
  UseMethod("collapseMC_Text")
}

#'@export
collapseMC_Text.GADSdat <- function(GADSdat, mc_var, text_var, mc_code4text, suffix = "_r") {
  MC_dat <- GADSdat$dat[, mc_var, drop = FALSE]
  #suppressMessages(MC_gads <- updateMeta(GADSdat, MC_dat))
  MC_gads <- updateMeta(GADSdat, MC_dat)
  MC <- extractData(MC_gads, convertMiss = FALSE, convertLabels = "character")[, 1]
  MC[which(MC == mc_code4text)] <- NA

  MC_new <- ifelse(is.na(MC), yes = GADSdat$dat[[text_var]],
                                 no = MC)
  df_MC_new <- data.frame(MC_new, stringsAsFactors = TRUE)
  names(df_MC_new) <- paste0(mc_var, suffix)
  MC_gads2 <- import_DF(df_MC_new)

  GADSdat_dat <- cbind(GADSdat$dat, MC_gads2$dat)
  GADSdat_out <- updateMeta(GADSdat, GADSdat_dat)
  GADSdat_out <- reuseMeta(GADSdat_out, other_GADSdat = MC_gads2, varName = paste0(mc_var, suffix))

  GADSdat_out
}





