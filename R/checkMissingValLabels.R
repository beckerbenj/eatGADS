
#############################################################################
#' @describeIn checkEmptyValLabels check for missing value labels
#'@export
checkMissingValLabels <- function(GADSdat, vars = namesGADS(GADSdat), classes = c("integer"), valueRange = NULL, output = c("list", "data.frame")) {
  UseMethod("checkMissingValLabels")
}

#'@export
checkMissingValLabels.GADSdat <- function(GADSdat, vars = namesGADS(GADSdat), classes = c("integer"), valueRange = NULL, output = c("list", "data.frame")) {
  check_GADSdat(GADSdat)
  check_vars_in_GADSdat(GADSdat, vars = vars)
  output <- match.arg(output)

  ## select only specific var types for check
  var_classes <- give_GADSdat_classes(GADSdat, vars = vars)
  vars <- vars[var_classes %in% classes]
  if(length(vars) == 0) stop("None of the specified 'vars' have the specified 'classes'.")

  #browser()

  not_labeled <- vector("list", length = length(vars))
  names(not_labeled) <- vars
  for(i in vars) {
    i_meta <- GADSdat$labels[GADSdat$labels$varName == i, ]
    i_labeled_values <- unique(i_meta[, "value"])[!is.na(unique(i_meta[, "value"]))]
    i_real_values <- unique(GADSdat$dat[, i])[!is.na(unique(GADSdat$dat[, i]))]
    missing_values <- setdiff(i_real_values, i_labeled_values)

    if(length(missing_values) > 0) {
      not_labeled[[i]] <- list()
      not_labeled[[i]]$varLabel <- i_meta[1, "varLabel"]
      not_labeled[[i]]$missing_labels <- sort(missing_values)
    }
  }

  if(!is.null(valueRange)) {
    if(!is.numeric(valueRange) || length(valueRange) != 2) stop("'valueRange' needs to be a numeric vector of length 2.")
    not_labeled <- lapply(not_labeled, function(not_labeled_single) {
      not_labeled_single$missing_labels <- not_labeled_single$missing_labels[between(not_labeled_single$missing_labels,
                                                                                     range(valueRange)[1], range(valueRange)[2])]
      if(length(not_labeled_single$missing_labels) == 0) return(NULL)
      not_labeled_single
    })

  }

  # reshape/restructure data.frame output as in checkEmptyValLabels()
  if(identical(output, "data.frame")) {
    #browser()
    not_labeled2 <- lapply(not_labeled, function(single_not_labeled) {
      if(is.null(single_not_labeled)) return(data.frame(varLabel = character(),
                                                        number_of_missing_labels = numeric(), values_with_missing_labels = character()))
      length_not_labeled <- length(single_not_labeled[["missing_labels"]])

      # print only first ten missing value labels
      if(length_not_labeled <= 10) {
        not_labeled_value_list <- paste(single_not_labeled[["missing_labels"]], collapse = ", ")
      } else {
        #browser()
         not_labeled_value_list_pre <- paste(single_not_labeled[["missing_labels"]][1:10], collapse = ", ")
         not_labeled_value_list <- paste0(not_labeled_value_list_pre, ", ...")
      }
      data.frame(varLabel = single_not_labeled[["varLabel"]],
                 number_of_missing_labels = length_not_labeled,
                 values_with_missing_labels = not_labeled_value_list)
    })
    out <- eatTools::do_call_rbind_withName(not_labeled2, colName = "variable")
  } else out <- not_labeled
  out
}

give_GADSdat_classes <- function(GADSdat, vars = namesGADS(GADSdat)) {
  out_list <- lapply(GADSdat$dat[, vars, drop = FALSE], function(single_var) {
    if(is.character(single_var)) {
      return("character")
    }
    tryCatch({
      single_var_retransformed <- as.double(as.integer(single_var))
    }, warning = {
      ## edge case for cases in which any(single_var > .Machine$integer.max)
      if(identical(single_var, round(single_var))) {
        return("integer")
      } else {
        return("double")
      }
    })
    if(identical(single_var, single_var_retransformed)) {
      return("integer")
    }
    return("double")
  })
  out <- unlist(out_list)
  names(out) <- vars
  out
}



