#### Match value and varLabels
#############################################################################
#' Match regular expressions and variable names.
#'
#' Using variable labels, the function matches a vector of regular expressions to a set of variable names.
#'
#' Note that all variables in \code{mc_vars} have to be assigned a value. If a variable name is missing in the output,
#' an error will be thrown. In this case, the \code{label_by_hand} argument should be used to specify the regular expression
#' variable name pair manually.
#'
#'@param GADSdat A \code{GADSdat} object.
#'@param mc_vars A vector containing the names of the variables, which should be matched according to their variable labels.
#'@param values A character vector containing the regular expressions for which the \code{varLabel} column should be searched.
#'@param label_by_hand Additional value - \code{mc_var} pairs. Necessary, if for some \code{mc_vars} no value exists.
#'
#'@return Returns a named character vector. Values of the vector are the variable names in the \code{GADSdat}, names of the vector
#'are the regular expressions.
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
