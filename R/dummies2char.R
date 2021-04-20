
#############################################################################
#' Transform dummy variables to character variables.
#'
#' Convert a set of dummy variables into a set of character variables.
#'
#' A set of dummy variables is transformed to an equal number of character variables.
#' The character variables are aligned to the left and the remaining character variables are set to \code{NA}.
#' For each new variable the missing codes of the respective dummy variable are reused.
#'
#'
#'@param GADSdat A \code{GADSdat} object.
#'@param dummies A character vector with the names of the dummy variables.
#'@param dummyValues A vector with the values which the dummy variables represent.
#'@param charNames A character vector containing the new variable names.
#'
#'@return Returns a \code{GADSdat}.
#'
#'@examples
#'## create an example GADSdat
#'dummy_df <- data.frame(d1 = c("eng", "no eng", "eng"),
#'                       d2 = c("french", "french", "no french"),
#'                       d3 = c("no ger", "ger", "no ger"),
#'                       stringsAsFactors = TRUE)
#'dummy_g <- import_DF(dummy_df)
#'
#'## transform dummy variables
#'dummy_g2 <- dummies2char(dummy_g, dummies = c("d1", "d2", "d3"),
#'                         dummyValues = c("english", "french", "german"),
#'                         charNames = c("char1", "char2", "char3"))
#'
#'
#'@export
dummies2char <- function(GADSdat, dummies, dummyValues, charNames) {
  UseMethod("dummies2char")
}

#'@export
dummies2char.GADSdat <- function(GADSdat, dummies, dummyValues, charNames) {
  check_GADSdat(GADSdat)
  if(!is.character(dummies)) stop("'dummies' needs to be a character vector.")
  if(length(dummies) != length(dummyValues)) stop("'dummyValues' needs to be the same length as 'dummies'.")
  if(length(dummies) != length(charNames)) stop("'charNames' needs to be the same length as 'dummies'.")
  check_vars_in_GADSdat(GADSdat, dummies)

  names(dummyValues) <- names(charNames) <- dummies
  for(dummy in dummies) {
    charName <- charNames[names(charNames) == dummy]
    dummyValue <- dummyValues[names(dummyValues) == dummy]

    all_meta <- extractMeta(GADSdat, dummy)
    valid_values <- all_meta[which(all_meta$missings != "miss"), "value"]
    valid_values <- valid_values[!is.na(valid_values)]

    dat <- GADSdat$dat
    dat[, charName] <- ifelse(dat[, dummy] == 1, yes = dummyValue, no = NA)
    suppressMessages(GADSdat <- updateMeta(GADSdat, dat))
    GADSdat <- reuseMeta(GADSdat, charName, other_GADSdat = GADSdat, other_varName = dummy,
                         missingLabels =  "only", addValueLabels = TRUE)
  }

  GADSdat$dat <- left_fill(GADSdat$dat, vars = charNames)

  ## remarks JB:
  # automatically delete empty variables after left_fill? maybe report this?
  # missing consistent instead of variable by variable?

  GADSdat
}
