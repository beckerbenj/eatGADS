#### Collapse multi MC and multi text variables.
#############################################################################
#' Recode multiple choice variable with multiple variables.
#'
#' Recode multiple variables (representing a single multiple choice item) based on multiple character variables
#' (representing a text field).
#'
#' If a multiple choice item can be answered with ticking multiple boxes, multiple variables in the data
#' set are necessary to represent this item. In this case, an additional text field for further answers can also
#' contain multiple values at once. However, some of the answers in the text field might be redundant to
#' the dummy variables. \code{collapseMultiMC_Text} allows to recode multiple MC items of this
#' kind based on multiple text variables. The recoding can be prepared by expanding the single text variable
#' (\code{\link{createLookup}} and \code{\link{applyLookup_expandVar}}) and by matching the dummy variables
#' to its underlying values stored in variable labels (\code{\link{matchValues_varLabels}}).
#'
#' The function recodes the dummy variables according to the character variables. Additionally, the \code{mc_var_4text}
#' variable is recoded according to the final status of the \code{text_vars} (exception: if the text variables were
#' originally \code{NA}, \code{mc_var_4text} is left as it was).
#'
#' Missing values in the character variables can be represented either by \code{NAs} or by empty characters.
#' The multiple choice variables specified with \code{mc_vars} can only contain the values \code{0},
#' \code{1} and missing codes. The value \code{1} must always represent "this category applies".
#' If necessary, use \code{\link{recodeGADS}} for recoding.
#'
#' For cases for which the \code{text_vars} contain only values that can be recoded into the \code{mc_vars},
#' all new \code{text_vars} are given specific missing codes (see \code{invalid_miss_code} and \code{invalid_miss_label}).
#' All remaining \code{NAs} on the character variables are given a specific missing code (\code{notext_miss_code}).
#'
#'@param GADSdat A \code{GADSdat} object.
#'@param mc_vars A character vector with the variable names of the multiple choice variable. Names of the character
#' vector are the corresponding values that are represented by the individual variables.
#' Creation by \code{\link{matchValues_varLabels}} is recommended.
#'@param text_vars A character vector with the names of the text variables which should be collapsed.
#'@param mc_var_4text The name of the multiple choice variable that signals that information from the text variable should be used. This variable is recoded according to the final status of the text variables.
#'@param var_suffix Variable suffix for the newly created \code{GADSdat}. If an empty character, the existing variables are overwritten.
#'@param label_suffix Suffix added to variable label for the newly created or modified variables in the \code{GADSdat}.
#'@param invalid_miss_code Missing code which is given to new character variables if all text entries where recoded into the dichotomous variables.
#'@param invalid_miss_label Value label for \code{invalid_miss_code}.
#'@param notext_miss_code Missing code which is given to empty character variables.
#'@param notext_miss_label Value label for \code{notext_miss_code}.
#'
#'@return Returns a \code{GADSdat} containing the newly computed variables.
#'
#'@examples
#' # Prepare example data
#' mt2 <- data.frame(ID = 1:4, mc1 = c(1, 0, 0, 0), mc2 = c(0, 0, 0, 0), mc3 = c(0, 1, 1, 0),
#'                   text1 = c(NA, "Eng", "Aus", "Aus2"), text2 = c(NA, "Franz", NA, "Ger"),
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
#'              text_vars = c("text1", "text2"), mc_var_4text = "mc3")
#'
#' out_gads2 <- multiChar2fac(out_gads, vars = c("text1_r", "text2_r"))
#'
#' final_gads <- remove2NAchar(out_gads2, vars = c("text1_r_r", "text2_r_r"),
#'                               max_num = 1, na_value = -99, na_label = "missing: excessive answers")
#'
#'@export
collapseMultiMC_Text <- function(GADSdat, mc_vars, text_vars, mc_var_4text, var_suffix = "_r", label_suffix = "(recoded)",
                                 invalid_miss_code = -98, invalid_miss_label = "Missing: Invalid response",
                                 notext_miss_code = -99, notext_miss_label = "Missing: By intention") {
  UseMethod("collapseMultiMC_Text")
}

#'@export
collapseMultiMC_Text.GADSdat <- function(GADSdat, mc_vars, text_vars, mc_var_4text, var_suffix = "_r", label_suffix = "(recoded)",
                                         invalid_miss_code = -98, invalid_miss_label = "Missing: Invalid response",
                                         notext_miss_code = -99, notext_miss_label = "Missing: By intention") {
  if(!all(mc_vars %in% namesGADS(GADSdat))) stop("Not all mc_vars are variables in the GADSdat.")
  if(!all(text_vars %in% namesGADS(GADSdat))) stop("Not all text_vars are variables in the GADSdat.")
  if(!is.character(mc_var_4text) || length(mc_var_4text) != 1) stop("mc_var_4text needs to be a character of lenth one.")
  if(!mc_var_4text %in% mc_vars) stop("mc_var_4text is not part of mc_vars.")
  check_01_mc_in_gadsdat(GADSdat, mcs = mc_vars)

  dat <- GADSdat$dat
  ## check if the the value has been given multiple times in the text fields?
  #browser()
  miss_codes <- unique(GADSdat$labels[GADSdat$labels$varName %in% text_vars & GADSdat$labels$missings == "miss", "value"])
  for(r in seq(nrow(dat))) {
    values_in_row <- as.character(dat[r, text_vars])[!is.na(as.character(dat[r, text_vars]))]
    values_in_row <- values_in_row[!values_in_row %in% miss_codes]
    dups_in_row <- duplicated(values_in_row[values_in_row != ""])
    #if(mc_var_4text == "Pfluhl_k") browser()
    if(any(dups_in_row)) stop("Duplicate values in row ", r, ".")
  }

  # create new variables
  new_mc_var_4text <- paste0(mc_var_4text, var_suffix)
  new_mc_vars <- paste0(mc_vars, var_suffix)
  names(new_mc_vars) <- names(mc_vars)
  for(i in seq(mc_vars)) dat[, new_mc_vars[i]] <- dat[, mc_vars[i]]
  new_text_vars <- paste0(text_vars, var_suffix)
  for(i in seq(text_vars)) dat[, new_text_vars[i]] <- dat[, text_vars[i]]

  # loop over all text variables, recode all MCs according to each
  new_mc_vars <- new_mc_vars[!new_mc_vars == new_mc_var_4text]
  for(text_var in new_text_vars) {
    for(mc_value in names(new_mc_vars)) {
      new_mc_var <- new_mc_vars[[mc_value]]
      dat[, new_mc_var] <- ifelse(!is.na(dat[[text_var]]) & dat[[text_var]] == mc_value, yes = 1, no = dat[[new_mc_var]])
    }
  }

  dat <- remove_values(dat, vars = new_text_vars, values = names(mc_vars))
  dat <- left_fill(dat, vars = new_text_vars)
  dat <- drop_empty(dat, vars = new_text_vars, miss_codes = miss_codes)

  GADSdat2 <- updateMeta(GADSdat, dat)
  # fix meta data for newly created variables
  for(old_varName in c(mc_vars, text_vars)) {
    new_varName <- paste0(old_varName, var_suffix)
    if(new_varName %in% namesGADS(GADSdat2)) {
      GADSdat2 <- reuseMeta(GADSdat = GADSdat2, varName = new_varName, other_GADSdat = GADSdat2, other_varName = old_varName)
      GADSdat2 <- append_varLabel(GADSdat2, new_varName, label_suffix = label_suffix)
    }
  }

  ## recode 'other' mc
  GADSdat2$dat[, new_mc_var_4text] <- ifelse(is.na(GADSdat2$dat[[new_text_vars[1]]]) | GADSdat$dat[[text_vars[1]]] %in% miss_codes,
                                             yes = 0, no = 1)
  ## special case: empty/missing text -> other stays as is
  GADSdat2$dat[, new_mc_var_4text] <- ifelse(is.na(GADSdat$dat[[text_vars[1]]]) | GADSdat$dat[[text_vars[1]]] %in% miss_codes,
                                             yes = GADSdat$dat[[mc_var_4text]], no = GADSdat2$dat[, new_mc_var_4text])
  ## special case 2: originally other = yes, now other = no: give special missing
  ## additionally: recode all remaining NA to missing code
  for(new_text_var in new_text_vars[new_text_vars %in% namesGADS(GADSdat2)]) {
    GADSdat2$dat[, new_text_var] <- ifelse(!is.na(GADSdat$dat[[text_vars[1]]]) & !GADSdat$dat[[text_vars[1]]] %in% miss_codes &
                                             GADSdat2$dat[[new_mc_var_4text]] == 0,
                                               yes = invalid_miss_code, no = GADSdat2$dat[, new_text_var])
    GADSdat2$dat[is.na(GADSdat2$dat[, new_text_var]), new_text_var] <- notext_miss_code

    # create corresponding missing labels for new text variables
    GADSdat2 <- changeValLabels(GADSdat2, varName = new_text_var, value = invalid_miss_code, valLabel = invalid_miss_label)
    GADSdat2 <- changeMissings(GADSdat2, varName = new_text_var, value = invalid_miss_code, missings = "miss")
    GADSdat2 <- changeValLabels(GADSdat2, varName = new_text_var, value = notext_miss_code, valLabel = notext_miss_label)
    GADSdat2 <- changeMissings(GADSdat2, varName = new_text_var, value = notext_miss_code, missings = "miss")
  }


  GADSdat2
}

check_01_mc_in_gadsdat <- function(GADSdat, mcs) {
  violating_vars <- character()
  for(mc in mcs) {
    suppressMessages(one_GADSdat <- removeVars(GADSdat, namesGADS(GADSdat)[namesGADS(GADSdat) != mc]))
    dat <- extractData(one_GADSdat, convertMiss = TRUE, convertLabels = "numeric")
    unique_values <- sort(unique(dat[[mc]]))
    if(!all(unique_values %in% c(0, 1))) {
      violating_vars <- c(violating_vars, paste0(mc, " contains values: ", paste(unique_values, collapse = ", ")))
    }
  }
  violating_list <- paste(violating_vars, collapse = "\n")
  if(length(violating_vars) > 0) stop(paste0("MC variables must be coded 0 (no) and 1 (yes):\n", violating_list))
  return()
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

drop_empty <- function(dat, vars = names(dat), miss_codes) {
  for(nam in names(dat)) {
    if(all(is.na(dat[[nam]]) | dat[[nam]] %in% miss_codes)) {
      warning("In the new variable ", nam, " all values are missing, therefore the variable is dropped. If this behaviour is not desired, contact the package author.")
      dat[[nam]] <- NULL
    }
  }
  dat
}

