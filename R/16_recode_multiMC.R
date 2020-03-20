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
#' mt2 <- data.frame(ID = 1:4, mc1 = c(1, 0, 0, 0), mc2 = c(0, 0, 0, 0), mc3 = c(0, 1, 1, 0),
#'                   text1 = c(NA, "Eng", "Aus", "Aus2"), text2 = c(NA, "Franz", NA, NA),
#'                   stringsAsFactors = FALSE)
#'
#' mt2_gads <- import_DF(mt2)
#'
#' mt3_gads <- changeVarLabels(mt2_gads, varName = c("mc1", "mc2", "mc3"),
#'                            varLabel = c("Lang: Eng", "Aus spoken", "other"))
#'
#' out <- matchValues_varLabels(mt3_gads, mc_vars = c("mc1", "mc2", "mc3"),
#'                             values = c("Aus", "Eng", "Eng"),
#'                             label_by_hand = c("other" = "mc3"))
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


#### Apply recode lookup table with input 1 variable output multiple variables
#############################################################################
#' Recoade via lookup table into multiple variables.
#'
#' Recode one or multiple variables based on a lookup table created via \code{\link{createLookup}}. In contrast to \code{\link{applyLookup}}, this function allows the creation of multiple resulting variables from a single input variable.
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


#### Collapse multi MC and multi text variables.
#############################################################################
#' Recode MC variable with multiple variables based on text.
#'
#' Recode a multiple variable multiplce choice item based on a multiple variable test field..
#'
#' If a multiple choice item can be answered with crossing multiple boxes, multiple variables in the data set are necessary to represent this item. In this case, an additional text field for further answers can also contain multiple values at once. This function allows to recode multiple MC items of this kind based on multiple text variables.
#'
#'@param GADSdat A \code{GADSdat} object.
#'@param mc_vars A character vector with the variable names of the multiple choice variable. Names of the character vector are the corresponding values that are represented by the individual variables.
#'@param text_vars A character vector with the names of the text variables which should be collapsed.
#'@param mc_var_4text The names of the multiple choice variable that signals that information from the text variable should be used.
#'@param var_suffix Variable suffix for the newly created \code{GADSdat}. If an empty character, the existing variables are overwritten.
#'@param label_suffix Suffix added to variable label for the newly created or modified variables in the \code{GADSdat}.
#'
#'@return Returns a \code{GADSdat} containing the newly computed variable.
#'
#'@examples
#' # Prepare example data
#' mt2 <- data.frame(ID = 1:4, mc1 = c(1, 0, 0, 0), mc2 = c(0, 0, 0, 0), mc3 = c(0, 1, 1, 0),
#'                   text1 = c(NA, "Eng", "Aus", "Aus2"), text2 = c(NA, "Franz", NA, NA),
#'                   stringsAsFactors = FALSE)
#' mt2_gads <- import_DF(mt2)
#' mt3_gads <- changeVarLabels(mt2_gads, varName = c("mc1", "mc2", "mc3"),
#'                             varLabel = c("Lang: Eng", "Aus spoken", "other"))
#'
#' ## All operations (see also respective help pages of functions for further explanations)
#' mc_vars <- matchValues_varLabels(mt3_gads, mc_vars = c("mc1", "mc2", "mc3"),
#'             values = c("Aus", "Eng", "Eng"), label_by_hand = c("other" = "mc3"))
#'
#' out_gads <- collapseMultiMC_Text(mt3_gads, mc_vars = mc_vars,
#' text_vars = c("text1", "text2"), mc_var_4text = "mc3")
#'
#' out_gads2 <- multiChar2fac(out_gads, vars = c("text1_r", "text2_r"))
#'
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
  if(!is.character(mc_var_4text) || length(mc_var_4text) != 1) stop("mc_var_4text needs to be a character of lenth one.")
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
  all_levels_fac <- data.frame("all_levels" = as.factor(all_levels))
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
