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
#' Extract values form variables of \code{GADSdat} object and write to \code{xlsx} for manuel recoding.
#'
#' If recoding of one or multiple variables is more complex, a lookup table can be created for later importing via \code{eatGADS}.
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

    if(!is.null(suffix)) {
      old_nam <- nam
      nam <- paste0(nam, suffix)
      rec_dt[sub_lu, on = old_nam, (nam) := i.value_new]
    } else {
      old_nam <- nam
      rec_dt[sub_lu, on = nam, (nam) := i.value_new]
    }

    GADSdat2 <- updateMeta(GADSdat2, newDat = as.data.frame(rec_dt, stringsAsFactor = FALSE))
    GADSdat2 <- reuseMeta(GADSdat = GADSdat2, varName = nam, other_GADSdat = GADSdat, other_varName = old_nam)
  }

  check_GADSdat(GADSdat2)
  GADSdat2
}

check_lookup <- function(lookup, GADSdat) {
  if(!all(lookup$variable %in% namesGADS(GADSdat))) stop("Some of the variables are not variables in the GADSdat.")
  if(!identical(names(lookup), c("variable", "value", "value_new"))) stop("LookUp table has to be formatted correctly.")
  if(any(is.na(lookup$value_new))) warning("Some values have no recode value assigned (missings in value_new).")
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


#### Match value and varLabels
#############################################################################
#' Match regular expresssions and variable names.
#'
#' Using variable labels, the function matches a vector of regular expressions to a set of variable names.
#'
#' Note that all variables in \code{mc_vars} have to be assigned a value. If a variable name is missing in the output, an error will be thrown. In this case, the \code{label_by_hand} argument should be used to specifiy the regular expression variable name pair manually.
#'
#'@param GADSdat A \code{GADSdat} object.
#'@param mc_vars A vector containing the names of the variables, which should be matched accoring to their variable labels.
#'@param values A character vector containing the regular expressions for which the \code{varLabel} column should be searched.
#'@param label_by_hand Additional value - mc_var pairs. Necessary, if for some mc_vars no value exists.
#'
#'@return Returns a named character vector. Values of the vector are the variable names in the \code{GADSdat}, names of the vector are the regular expressions.
#'
#'@examples
#' # Prepare example data
#' mt2 <- data.frame(ID = 1:4, mc1 = c(1, 0, 0, 0), mc2 = c(0, 0, 0, 0), mc3 = c(0, 1, 1, 0), text1 = c(NA, "Eng", "Aus", "Aus2"), text2 = c(NA, "Franz", NA, NA),stringsAsFactors = FALSE)
#' mt2_gads <- import_DF(mt2)
#' mt3_gads <- changeVarLabels(mt2_gads, varName = c("mc1", "mc2", "mc3"), varLabel = c("Lang: Eng", "Aus spoken", "other"))
#'
#' out <- matchValues_varLabels(mt3_gads, mc_vars = c("mc1", "mc2", "mc3"), values = c("Aus", "Eng", "Eng"), label_by_hand = c("other" = "mc3"))
#'
#'@export
matchValues_varLabels <- function(GADSdat, mc_vars, values, label_by_hand = character(0)) {
  check_GADSdat(GADSdat)
  if(!is.vector(values) & length(values) > 0) stop("values needs to be a character vector of at least length 1.")

  values <- unique(values)
  labels <- unique(extractMeta(GADSdat, mc_vars)[, c("varName", "varLabel")])

  ## test label_by_hand (all in names, all in varLabel)
  if(!all(label_by_hand %in% mc_vars)) stop("All variable names in label_by_hand must be variables in mc_vars.")

  names(values) <- values
  matches <- lapply(values, function(value) labels[grep(value, labels$varLabel), "varName"])
  matches <- unlist(matches[sapply(matches, function(x) length(x) > 0)])
  matches <- c(matches, label_by_hand)

  unassigned_mcs <- mc_vars[!mc_vars %in% matches]
  if(length(unassigned_mcs) > 0) stop("The following mc_vars have not been assigned a value: ", paste(unassigned_mcs, collapse = ", "))
  names(mc_vars) <- names(matches)[match(mc_vars, matches)]
  mc_vars
}



#### Apply recode lookup table for Multi-MC
#############################################################################
#' Recode MC variable with multiple variables based on text.
#'
#' Recode a multiple variable multiplce choice item based on a multiple variable test field..
#'
#' If a multiple choice item can be answered with crossing multiple boxes, multiple variables in the data set are necessary to represent this item. In this case, an additional text field for further answers can also contain multiple values at once. This function allows to recode multiple MC items of this kind based on multiple text variables.
#'
#'@param GADSdat A \code{GADSdat} object.
#'@param mc_vars A single variable name of the multiple choice variable.
#'@param text_vars A single variable name of the text variable.
#'@param mc_var_4text The value in the MC variable that signals that information from the text variable should be used.
#'@param var_suffix Variable suffix for the newly created \code{GADSdat}. If an empty character, the existing variables are overwritten.
#'@param label_suffix Suffix added to variable label for the newly created variable in the \code{GADSdat}.
#'
#'@return Returns a \code{GADSdat} containing the newly computed variable.
#'
#'@examples
#' # Prepare example data
#' mt2 <- data.frame(ID = 1:4, mc1 = c(1, 0, 0, 0), mc2 = c(0, 0, 0, 0), mc3 = c(0, 1, 1, 0),
#' text1 = c(NA, "Eng", "Aus", "Aus2"), text2 = c(NA, "Franz", NA, NA),stringsAsFactors = FALSE)
#' mt2_gads <- import_DF(mt2)
#' mt3_gads <- changeVarLabels(mt2_gads, varName = c("mc1", "mc2", "mc3"), varLabel = c("Lang: Eng", "Aus spoken", "other"))
#'
#' ## All operations (see also respective help pages of functions for further explanations)
#' mc_vars <- matchValues_varLabels(mt3_gads, mc_vars = c("mc1", "mc2", "mc3"), values = c("Aus", "Eng", "Eng"), label_by_hand = c("other" = "mc3"))
#' out_gads <- collapseMultiMC_Text(mt3_gads, mc_vars = mc_vars, text_vars = c("text1", "text2"), mc_var_4text = "mc3")
#' out_gads2 <- multiChar2fac(out_gads, vars = c("text1_r", "text2_r"))
#' final_gads <- remove_2NA_char(out_gads2, vars = c("text1", "text2"), max_num = 1, na_value = -99)
#'
#'@export
collapseMultiMC_Text <- function(GADSdat, mc_vars, text_vars, mc_var_4text, var_suffix = "_r", label_suffix = "(recoded)") {
  UseMethod("collapseMultiMC_Text")
}

#'@export
collapseMultiMC_Text.GADSdat <- function(GADSdat, mc_vars, text_vars, mc_var_4text, var_suffix = "_r", label_suffix = "(recoded)") {
  if(!all(mc_vars %in% namesGADS(GADSdat))) stop("Not all mc_vars are variables in the GADSdat.")
  if(!all(text_vars %in% namesGADS(GADSdat))) stop("Not all text_vars are variables in the GADSdat.")
  if(!mc_var_4text %in% mc_vars) stop("mc_var_4text is not part of mc_vars.")

  dat <- GADSdat$dat
  ## check if the the value has been given multiple times in the text fields?
  for(r in seq(nrow(dat))) {
    dups_in_row <- duplicated(as.character(dat[r, text_vars])[!is.na(as.character(dat[r, text_vars]))])
    if(any(dups_in_row)) stop("Duplicate values in row ", r, ".")
  }

  # create new variables
  new_mc_vars <- paste0(mc_vars, var_suffix)
  names(new_mc_vars) <- names(mc_vars)
  for(i in seq(mc_vars)) dat[, new_mc_vars[i]] <- dat[, mc_vars[i]]
  new_text_vars <- paste0(text_vars, var_suffix)
  for(i in seq(text_vars)) dat[, new_text_vars[i]] <- dat[, text_vars[i]]

  # loop over all text variables, recode all MCs according to each
  new_mc_vars <- new_mc_vars[!new_mc_vars == paste0(mc_var_4text, var_suffix)]
  for(text_var in new_text_vars) {
    for(mc_value in names(new_mc_vars)) {
      new_mc_var <- new_mc_vars[[mc_value]]
      dat[, new_mc_var] <- ifelse(!is.na(dat[[text_var]]) & dat[[text_var]] == mc_value, yes = 1, no = dat[[new_mc_var]])
    }
  }

  dat <- remove_values(dat, vars = new_text_vars, values = names(mc_vars))
  dat <- left_fill(dat, vars = new_text_vars)

  GADSdat2 <- updateMeta(GADSdat, dat)
  # fix meta data for newly created variables
  for(old_varName in c(mc_vars, text_vars)) {
    new_varName <- paste0(old_varName, var_suffix)
    GADSdat2 <- reuseMeta(GADSdat = GADSdat2, varName = new_varName, other_GADSdat = GADSdat2, other_varName = old_varName)
    GADSdat2 <- append_varLabel(GADSdat2, new_varName, label_suffix = label_suffix)
  }

  GADSdat2
}

# remove all text values that occur in labels (own function)
remove_values <- function(dat, vars = names(dat), values) {
  for(value in values) {
    dat[, vars][dat[, vars] == value] <- NA
  }
  dat
}

# "refill" text variables (move up values) (own function)
left_fill <- function(dat, vars = names(dat)) {
  for(var in vars[-1]) {
    var_left <- names(dat)[which(names(dat) == var) - 1]
    var_left_ori <- dat[, var_left]
    dat[, var_left] <- ifelse(is.na(dat[[var_left]]) & !is.na(dat[[var]]), yes = dat[[var]], no = dat[[var_left]])
    dat[, var] <- ifelse(is.na(var_left_ori) & !is.na(dat[[var]]), yes = NA, no = dat[[var]])
  }
  dat
}


#### Multiple Strings to Labeled Variables
#############################################################################
#' Multiple character variables to factors with identical levels.
#'
#' Convert multiple character variables to factors, while creating a common set of value labels, which is identical across variables.
#'
#' If a set of variables has the same possible values, it is desirable that these variables share the same value labels, even if some of the values do not occur on the individual variables. This function allows the transformation of multiple character variables to factors while assimilating the value labels.
#'
#'@param GADSdat A \code{data.frame} or \code{GADSdat} object.
#'@param vars A single variable name of the multiple choice variable.
#'@param var_suffix Variable suffix for the newly created \code{GADSdat}. If an empty character, the existing variables are overwritten.
#'@param label_suffix Suffix added to variable label for the newly created variable in the \code{GADSdat}.
#'
#'@return Returns a \code{GADSdat} containing the newly computed variable.
#'
#'@examples
#'#to be done
#'
#'@export
multiChar2fac <- function(GADSdat, vars, var_suffix = "_r", label_suffix = "(recoded)") {
  UseMethod("multiChar2fac")
}

#'@export
multiChar2fac.GADSdat <- function(GADSdat, vars, var_suffix = "_r", label_suffix = "(recoded)") {
  check_GADSdat(GADSdat)
  if(!is.character(vars) && length(vars) > 0) stop("vars needs to be a character vector of at least length 1.")

  all_levels <- unique(unlist(lapply(GADSdat$dat[vars], function(x) x)))
  all_levels_fac <- as.data.frame(all_levels, stringsAsFactor = TRUE)
  all_levels_gads <- import_DF(all_levels_fac)
  all_levels_lookup <- all_levels_gads$labels[, c("valLabel", "value")]
  names(all_levels_lookup) <- c("value", "value_new")

  for(var in vars) {
    old_nam <- var
    var <- paste0(var, var_suffix)
    specific_lookup <- data.frame(variable = old_nam, all_levels_lookup, stringsAsFactors = FALSE)
    GADSdat <- applyLookup(GADSdat, lookup = specific_lookup, suffix = var_suffix)
    GADSdat$dat[, var] <- as.numeric(GADSdat$dat[, var])

    GADSdat <- reuseMeta(GADSdat, varName = var, other_GADSdat = all_levels_gads, other_varName = "all_levels")
    GADSdat <- append_varLabel(GADSdat, varName = var, label_suffix = label_suffix)
  }

  GADSdat
}



#### Shorten multiple text variables
#############################################################################
#' Shorten multiple text variables while giving NA codes.
#'
#' Remove text variables from a certain number from \code{GADSdat} while coding overflowing answers as complete missings.
#'
#' In some cases, multiple text variables contain the information of one variable (e.g. mutliple answers to an open item).
#'
#'@param GADSdat A \code{GADSdat} object.
#'@param vars A character vector with the names of the text variables.
#'@param max_num Maximum number of text variables. Additional text variables will be removed and NA codes given accordingly.
#'@param na_value Which NA value should be given in cases of too many values on text variables.
#'
#'@return Returns the modified \code{GADSdat}.
#'
#'@examples
#'#to be done
#'
#'@export
remove_2NA_char <- function(GADSdat, vars, max_num = 2, na_value) {
  UseMethod("remove_2NA_char")
}

#'@export
remove_2NA_char.GADSdat <- function(GADSdat, vars, max_num = 2, na_value) {
  check_GADSdat(GADSdat)
  if(!is.numeric(max_num) && length(max_num) == 1 && max_num > 0) stop("max_num needs to be a single numeric value greater than 0.")

  dat <- max_num_strings2NA(GADSdat$dat, vars = vars, max_num = max_num, na_value = na_value)
  # cut text variables
  remove_vars <- vars[-(1:max_num)]
  dat2 <- dat[, !names(dat) %in% remove_vars, drop = FALSE]

  updateMeta(GADSdat, dat2)
}

# count text variables, give missings if more than x left
max_num_strings2NA <- function(dat, vars, max_num, na_value) {
  #dat[, vars] <- ifelse(!is.na(dat[, max_num]), yes = NA, no = dat[, vars])
  stopifnot(is.numeric(max_num) && length(max_num) == 1)
  stopifnot(is.character(vars) && length(vars) > 1)

  for(i in seq(nrow(dat))) {
    if(!is.na(dat[i, max_num + 1])) {
      dat[i, vars] <- na_value
    }
  }
  dat
}
