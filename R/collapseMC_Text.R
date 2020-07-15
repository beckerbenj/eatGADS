
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
  suppressWarnings(suppressMessages(GADSdat_dat3 <- applyLookup(GADSdat_dat2, lookup = lookup_oldValues)))

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
  suppressWarnings(suppressMessages(GADSdat_dat4 <- applyLookup(GADSdat_dat3, lookup = lookup_newValues)))
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
