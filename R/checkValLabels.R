
#############################################################################
#' Check Value Labels
#'
#' Check value labels for (a) value labels with no occurrence in the data (\code{checkEmptyValLabels}) and
#' (b) values with no value labels (\code{checkMissingValLabels}).
#'
#' \code{NAs} are excluded from this check. Designated missing codes are reported normally.
#'
#'@param GADSdat A \code{GADSdat} object.
#'@param vars Character vector with the variable names to which \code{checkValLabels()} should be applied.
#'@param classes Character vector with the classes to which \code{checkMissingLabels()} should be applied. Valid options are \code{"integer"}, \code{"double"}, and \code{"character"}.
#'@param valueRange [optional] Numeric vector of length 2: In which range should numeric values be checked?
#'If specified, only numeric values are returned and strings are omitted.
#'@param output Should the output be structured as a \code{"list"} or a \code{"data.frame"}?
#'
#'@return Returns a list of length \code{vars} or a \code{data.frame}.
#'
#'@examples
#'# Check a categorical and a metric variable
#'checkMissingValLabels(pisa, vars = c("g8g9", "age"))
#'checkEmptyValLabels(pisa, vars = c("g8g9", "age"))
#'
#' # Check while defining a specific value range
#'checkMissingValLabels(pisa, vars = c("g8g9", "age", "idschool"),
#'               valueRange = c(0, 5))
#'checkEmptyValLabels(pisa, vars = c("g8g9", "age", "idschool"),
#'               valueRange = c(0, 5))
#'
#' @describeIn checkEmptyValLabels check for superfluous value labels
#'@export
checkEmptyValLabels <- function(GADSdat, vars = namesGADS(GADSdat), valueRange = NULL, output = c("list", "data.frame")) {
  UseMethod("checkEmptyValLabels")
}

#'@export
checkEmptyValLabels.GADSdat <- function(GADSdat, vars = namesGADS(GADSdat), valueRange = NULL, output = c("list", "data.frame")) {
  check_GADSdat(GADSdat)
  check_vars_in_GADSdat(GADSdat, vars = vars)
  output <- match.arg(output)

  label_no_values <- vector("list", length = length(vars))
  names(label_no_values) <- vars
  for(i in vars) {
    i_meta <- GADSdat$labels[GADSdat$labels$varName == i, ]
    i_labeled_values <- unique(i_meta[, "value"])[!is.na(unique(i_meta[, "value"]))]
    i_real_values <- unique(GADSdat$dat[, i])[!is.na(unique(GADSdat$dat[, i]))]
    empty_values <- setdiff(i_labeled_values, i_real_values)
    label_no_values[[i]] <- i_meta[i_meta$value %in% empty_values, c("value", "valLabel", "missings")]
    label_no_values[[i]] <- label_no_values[[i]][order(label_no_values[[i]][, "value"]), ]
  }

  if(!is.null(valueRange)) {
    if(!is.numeric(valueRange) || length(valueRange) != 2) stop("'valueRange' needs to be a numeric vector of length 2.")

    label_no_values <- lapply(label_no_values, function(label_no_values_single) {
      label_no_values_single[between(label_no_values_single$value, range(valueRange)[1], range(valueRange)[2]), ]
    })

  }

  if(identical(output, "data.frame")) {
    out <- eatTools::do_call_rbind_withName(label_no_values, colName = "variable")
  } else out <- lapply(label_no_values, function(x) if(nrow(x) == 0) NULL else x)
  out
}

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
    if(is.character(single_var)) return("character")
    if(identical(single_var, as.double(as.integer(single_var)))) return("integer")
    return("double")
  })
  out <- unlist(out_list)
  names(out) <- vars
  out
}



