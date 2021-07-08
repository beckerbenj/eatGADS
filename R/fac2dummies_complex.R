
#############################################################################
#' Transform a complex factor variable to dummy variables.
#'
#' Convert a factor variable with complex factor levels (factor levels contain combinations of other factor levels) to dummy variables.
#' Dummy variables are coded \code{1} (\code{"yes"}) and \code{0} (\code{"no"}).
#'
#' The basic functionality of this function is analogous to \code{\link{fac2dummies}}. However, the function expects factor levels to only go
#' to \code{9}. Higher numbers are treated as combinations of factor levels, for example \code{"13"} as \code{"1"} and \code{"3"}.
#'
#'@param GADSdat A \code{data.frame} or \code{GADSdat} object.
#'@param var A character vector with the name of the factor variable.
#'
#'@return Returns a \code{GADSdat} containing the newly computed variables.
#'
#'@examples
#'## create an example GADSdat
#' df_fac <- data.frame(id = 1:6, fac = c("Opt a", "Opt c, Opt b", "Opt c",
#' "Opt b", "Opt a, Opt b", "Opt a, Opt b, Opt c"), stringsAsFactors = TRUE)
#' g_fac <- import_DF(df_fac)
#' g_fac <- recodeGADS(g_fac, varName = "fac", oldValues = c(1, 2, 3, 4, 5, 6),
#'                      newValues = c(1, 12, 123, 2, 3, 23))
#'
#'## transform factor variable
#' fac2dummies_complex(g_fac, "fac")
#'
#'
#'@export
fac2dummies_complex <- function(GADSdat, var) {
  UseMethod("fac2dummies_complex")
}

#'@export
fac2dummies_complex.GADSdat <- function(GADSdat, var) {
  check_GADSdat(GADSdat)
  if(!is.character(var) || length(var) != 1) stop("'var' needs to be a character vector of length 1.")
  check_vars_in_GADSdat(GADSdat, var)

  var_labels <- GADSdat$labels[GADSdat$labels$varName == var, ]
  all_levels <- unique(var_labels[is.na(var_labels$missings) | var_labels$missings != "miss", "value"])
  all_distinct_levels <- all_levels[all_levels < 10]
  all_miss_levels <- var_labels[which(var_labels$missings == "miss"), "value"]
  var_suffix <- letters[seq_along(all_distinct_levels)]
  new_dummies <- paste(var, var_suffix, sep = "_")
  names(new_dummies) <- all_distinct_levels

  illegal_dummies <- new_dummies[new_dummies %in% namesGADS(GADSdat)]
  if(length(illegal_dummies) > 0) stop("The following variables are already in the 'GADSdat' and conflict with dummy variables you are trying to create: ", paste(illegal_dummies, collapse = ", "))


  valLabel_prefix <- var_labels[1, "varLabel"]
  if(is.na(valLabel_prefix)) valLabel_prefix <- var

  for(single_level in all_distinct_levels) {
    new_dat <- GADSdat$dat
    single_dummie <- new_dummies[as.character(single_level)]
    #browser()
    new_dat[, single_dummie] <- ifelse(new_dat[, var] == single_level, yes = 1,
                                       no = ifelse(new_dat[, var] %in% all_miss_levels, yes = new_dat[, var], no = 0))

    suppressMessages(GADSdat <- updateMeta(GADSdat, newDat = new_dat))
    new_val_label <- paste(valLabel_prefix, var_labels[var_labels$value == single_level, "valLabel"], sep = ": ")

    GADSdat <- changeVarLabels(GADSdat, varName = single_dummie,
                               varLabel = new_val_label)
    GADSdat <- reuseMeta(GADSdat, varName = single_dummie, other_GADSdat = GADSdat, other_varName = var,
                         missingLabels = "only", addValueLabels = TRUE)
    GADSdat <- changeValLabels(GADSdat, varName = single_dummie, value = c(1, 0), valLabel = c("yes", "no"))
    GADSdat <- changeMissings(GADSdat, varName = single_dummie, value = c(1, 0), missings = c("valid", "valid"))
  }

  for(combi_level in all_levels[!all_levels %in% all_distinct_levels]) {
    combi_level_char <- as.character(combi_level)
    single_levels <- substring(combi_level_char, seq(nchar(combi_level_char)), seq(nchar(combi_level_char)))

    for(single_level2 in single_levels) {
      single_dummy2 <- new_dummies[single_level2]
      GADSdat$dat[, single_dummy2] <-  ifelse(new_dat[, var] == combi_level, yes = 1, no = GADSdat$dat[, single_dummy2])
    }
  }

  for(i in new_dummies) {
    GADSdat <- changeSPSSformat(GADSdat, varName = i, format = "F2.0")
  }

  message("The following dummy variables have been created: ", paste(new_dummies, collapse = (", ")))
  GADSdat
}





