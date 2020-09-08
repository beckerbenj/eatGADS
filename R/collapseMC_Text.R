
#### Collapse an MC and a text variable.
#############################################################################
#' Recode a multiple choice variable according to a character variable.
#'
#' Recode an labeled integer variable (based on an multiple choice item), according to a character variable (e.g. an open answer item).
#'
#' Multiple choice variables can be represented as labeled integer variables in a \code{GADSdat}. Multiple choice items with a forced choice
#' frequently contain an open answer category. However, sometimes open answers overlap with the existing categories in the multiple choice
#' item. \code{collapseMC_Text} allows recoding the multiple choice variable based on the open answer variable.
#'
#' \code{mc_code4text} indicates when entries in the \code{text_var} should be used. Additionally, entries in the \code{text_var} are also
#' used when there are missings on the \code{mc_var}. New values for the \code{mc_var} are added in the meta data, while preserving the initial
#' ordering of the value labels. Newly added value labels are sorted alphabetically.
#'
#' For more details see the help vignette:
#' \code{vignette("recoding_forcedChoice", package = "eatGADS")}.
#'
#'
#'@param GADSdat A \code{GADSdat} object.
#'@param mc_var The variable name of the multiple choice variable.
#'@param text_var The variable name of the text variable.
#'@param mc_code4text The value label in \code{mc_var} that indicates that information from the text variable should be used.
#'@param var_suffix Variable name suffix for the newly created variables. If \code{NULL}, variables are overwritten.
#'@param label_suffix Variable label suffix for the newly created variable (only added in the meta data). If \code{NULL} no suffix is added.
#'
#'@return Returns a \code{GADSdat} containing the newly computed variable.
#'
#'@examples
#'# Example gads
#'example_df <- data.frame(ID = 1:5, mc = c("blue", "blue", "green", "other", "other"),
#'                         open = c(NA, NA, NA, "yellow", "blue"),
#'                         stringsAsFactors = FALSE)
#'example_df$mc <- as.factor(example_df$mc)
#'gads <- import_DF(example_df)
#'
#'# recode
#'gads2 <- collapseMC_Text(gads, mc_var = "mc", text_var = "open",
#'                         mc_code4text = "other")
#'
#'@export
collapseMC_Text <- function(GADSdat, mc_var, text_var, mc_code4text, var_suffix = "_r", label_suffix = "(recoded)") {
  UseMethod("collapseMC_Text")
}

#'@export
collapseMC_Text.GADSdat <- function(GADSdat, mc_var, text_var, mc_code4text, var_suffix = "_r", label_suffix = "(recoded)") {
  if(!mc_var %in% namesGADS(GADSdat)) stop("'mc_var' is not a variable in the GADSdat.")
  if(!text_var %in% namesGADS(GADSdat)) stop("'text_var' is not a variable in the GADSdat.")
  if(!is.numeric(GADSdat$dat[, mc_var]) || GADSdat$labels[GADSdat$labels$varName == mc_var, "labeled"][1] != "yes"){
    stop("'mc_var' must be a labeled integer.")
  }
  if(!is.character(mc_code4text) || length(mc_code4text) != 1) stop("'mc_code4text' must be a character of length 1.")
  if(!mc_code4text %in% GADSdat$labels[GADSdat$labels$varName == mc_var, "valLabel"]) stop("'mc_code4text' must be a 'valLabel' entry for 'mc_var'.")

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

  GADSdat_dat <- GADSdat$dat
  GADSdat_dat[, mc_var_new] <- MC_new
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
  if(is.null(label_suffix) || nchar(label_suffix) == 0) return(GADSdat)
  old_varLabel <- extractMeta(GADSdat, varName)[1, "varLabel"]
  new_varLabel <- ifelse(is.na(old_varLabel), yes = label_suffix,
                         no = paste(old_varLabel, label_suffix, sep = " "))
  GADSdat_out <- changeVarLabels(GADSdat, varName = varName, varLabel =
                                   new_varLabel)
  GADSdat_out
}
