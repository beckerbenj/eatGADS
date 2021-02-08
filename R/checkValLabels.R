
#############################################################################
#' Check Value Labels
#'
#' Check value labels for (a) value labels with no occurrence in the data and (b) values with no value labels.
#'
#' \code{NAs} are excluded from this check. Designated missing codes are reported normally.
#'
#'@param GADSdat A \code{GADSdat} object.
#'@param vars Character vector with the variable names to which \code{checkValLabels()} should be applied.
#'@param valueRange [optional] Numeric vector of length 2: In which range should numeric values be checked? If specified, only numeric values
#'are returned and strings are omitted.
#'
#'@return Returns a list of length 2. Each contains a list of the length of \code{vars}.
#'\item{labels with no values}{Value labels with no occurrence in the data}
#'\item{not labeled values}{Values in the data with no value labels}
#'
#'@examples
#'# Check a categorical and a metric variable
#'checkValLabels(pisa, vars = c("g8g9", "age"))
#'
#' # Check while defining a specific value range
#'checkValLabels(pisa, vars = c("g8g9", "age", "idschool"),
#'               valueRange = c(0, 5))
#'
#'@export
checkValLabels <- function(GADSdat, vars = namesGADS(GADSdat), valueRange = NULL) {
  UseMethod("checkValLabels")
}

#'@export
checkValLabels.GADSdat <- function(GADSdat, vars = namesGADS(GADSdat), valueRange = NULL) {
  check_GADSdat(GADSdat)
  check_vars_in_GADSdat(GADSdat, vars = vars)

  label_no_values <- not_labeled <- vector("list", length = length(vars))
  names(label_no_values) <- names(not_labeled) <- vars
  for(i in vars) {
    i_meta <- GADSdat$labels[GADSdat$labels$varName == i, ]
    i_labeled_values <- unique(i_meta[, "value"])[!is.na(unique(i_meta[, "value"]))]
    i_real_values <- unique(GADSdat$dat[, i])[!is.na(unique(GADSdat$dat[, i]))]
    label_no_values[[i]] <- setdiff(i_labeled_values, i_real_values)
    not_labeled[[i]] <- setdiff(i_real_values, i_labeled_values)
  }
  out <- list("labels with no values" = label_no_values, "not labeled values" = not_labeled)

  if(!is.null(valueRange)) {
    if(!is.numeric(valueRange) || length(valueRange) != 2) stop("'valueRange' needs to be a numeric vector of length 2.")

    out <- lapply(out, function(x) {
      lapply(x, function(single_vector){
        single_vector <- suppressWarnings(eatTools::asNumericIfPossible(single_vector))
        single_vector <- na_omit(single_vector)
        single_vector[between(single_vector, range(valueRange)[1], range(valueRange)[2])]
      })
    })
  }

  out
}

