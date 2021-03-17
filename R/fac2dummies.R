
#############################################################################
#' Transform a factor variable to dummy variables.
#'
#' Convert a factor variable with n levels to n dummy variables.
#'
#' Newly created variables are named as the original variable with the suffix \code{"_a"}, \code{"_b"} and so on. Variable labels
#' are created by using the original variable label (if available) and adding the value label of the corresponding level.
#' All missing codes are forwarded to all dummy variables.
#'
#'@param GADSdat A \code{data.frame} or \code{GADSdat} object.
#'@param var A character vector with the name of the factor variable.
#'
#'@return Returns a \code{GADSdat} containing the newly computed variables.
#'
#'@examples
#'## create an example GADSdat
#'suppressMessages(gads <- import_DF(iris))
#'
#'## transform factor variable
#'gads2 <- fac2dummies(gads, var = "Species")
#'
#'
#'@export
fac2dummies <- function(GADSdat, var) {
  UseMethod("fac2dummies")
}

#'@export
fac2dummies.GADSdat <- function(GADSdat, var) {
  check_GADSdat(GADSdat)
  if(!is.character(var) || length(var) != 1) stop("'var' needs to be a character vector of length 1.")
  check_vars_in_GADSdat(GADSdat, var)

  var_labels <- GADSdat$labels[GADSdat$labels$varName == var, ]
  all_levels <- unique(var_labels[is.na(var_labels$missings) | var_labels$missings != "miss", "value"])
  all_miss_levels <- var_labels[which(var_labels$missings == "miss"), "value"]
  var_suffix <- letters[seq_along(all_levels)]
  new_dummies <- paste(var, var_suffix, sep = "_")
  names(new_dummies) <- all_levels

  illegal_dummies <- new_dummies[new_dummies %in% namesGADS(GADSdat)]
  if(length(illegal_dummies) > 0) stop("The following variables are already in the 'GADSdat' and conflict with dummy variables you are trying to create: ", paste(illegal_dummies, collapse = ", "))


  valLabel_prefix <- var_labels[1, "varLabel"]
  if(is.na(valLabel_prefix)) valLabel_prefix <- var

  for(single_level in all_levels) {
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
  }

  for(i in new_dummies) {
    GADSdat <- changeSPSSformat(GADSdat, varName = i, format = "F2.0")
  }

  message("The following dummy variables have been created: ", paste(new_dummies, collapse = (", ")))
  GADSdat
}
