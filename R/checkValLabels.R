
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
#'
#'@export
  check_GADSdat(GADSdat)
  check_vars_in_GADSdat(GADSdat, vars = vars)

}
