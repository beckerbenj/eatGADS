#### extractData
#############################################################################
#' Extract Data
#'
#' Extract \code{data.frame} from a \code{GADSdat} object for analyses in \code{R}. For extracting meta data see \code{\link{extractMeta}}.
#'
#' A \code{GADSdat} object includes actual data (\code{GADSdat$dat}) and the corresponding meta data information
#' (\code{GADSdat$labels}). \code{extractData} extracts the data and applies relevant meta data (missing conversion, value labels),
#' so the data can be used for analyses in \code{R}.
#'
#' If \code{factor} are extracted via \code{convertLabels == "factor"}, the underlying integers will
#' be tried to preserved. If this is not possible, a warning is issued. As \code{SPSS} has almost no limitations regarding the underlying values of labeled
#' integers and \code{R}'s \code{factor} format is very strict (no \code{0}, only integers increasing by \code{+ 1}), this procedure can lead to
#' frequent problems.
#'
#'@param GADSdat A \code{GADSdat} object.
#'@param convertMiss Should values coded as missing values be recoded to \code{NA}?
#'@param convertLabels If \code{"numeric"}, values remain as numerics. If \code{"factor"} or \code{"character"}, values are recoded to their labels. Corresponding variable type is applied.
#'@param dropPartialLabels Should value labels for partially labeled variables be dropped? If \code{TRUE}, the partial labels will be dropped. If \code{FALSE}, the variable will be converted to the class specified in \code{convertLabels}.
#'@param convertVariables Character vector of variables names, which labels should be applied to. If not specified (default), value labels are applied to all variables for which labels are available. Variable names not in the actual GADS are silently dropped.
#'
#'@return Returns a data frame.
#'
#'@examples
#'# Extract Data for Analysis
#'dat <- extractData(pisa)
#'
#'# convert labeled variables to factors
#'dat <- extractData(pisa, convertLabels = "factor")
#'
#'# convert only some variables to factor
#'dat <- extractData(pisa, convertLabels = "factor", convertVariables = c("schtype", "ganztag"))
#'
#'@export
extractData <- function(GADSdat, convertMiss = TRUE, convertLabels = "character", dropPartialLabels = TRUE, convertVariables) {
  UseMethod("extractData")
}

#'@export
extractData.GADSdat <- function(GADSdat, convertMiss = TRUE, convertLabels = "character", dropPartialLabels = TRUE, convertVariables) {
  check_GADSdat(GADSdat)
  if(length(convertLabels) != 1 || !convertLabels %in% c("character", "factor", "numeric")) stop("Argument convertLabels incorrectly specified.")
  dat <- GADSdat$dat
  labels <- GADSdat$labels
  ## missings
  if(identical(convertMiss, TRUE)) dat <- miss2NA(GADSdat)
  ## labels
  dat <- labels2values(dat = dat, labels = labels, convertLabels = convertLabels, convertMiss = convertMiss,
                       dropPartialLabels = dropPartialLabels, convertVariables = convertVariables)
  dat
}

#'@export
extractData.trend_GADSdat <- function(GADSdat, convertMiss = TRUE, convertLabels = "character", dropPartialLabels = TRUE, convertVariables) {
  check_trend_GADSdat(GADSdat)

  all_dat <- extract_data_only(GADSdat = GADSdat, convertMiss = convertMiss, convertLabels = convertLabels,
                               dropPartialLabels = dropPartialLabels, convertVariables = convertVariables)

  ## if available, merge also linking errors; merge picks by automatically, keep variable order as in original data frames
  if(!is.null(GADSdat$datList[["LEs"]])) {
    gads_le <- extractGADSdat(all_GADSdat = GADSdat, name = "LEs")
    le <- extractData(gads_le, convertMiss = convertMiss, convertLabels = "character")

    # performance relevant: merge (data.table seems to be fastest)
    all_dat <- data.table::setDT(all_dat)
    le <- data.table::setDT(le)
    all_dat_withLEs <- merge(all_dat, le)
    all_dat_withLEs <- as.data.frame(all_dat_withLEs)

    all_dat <- all_dat_withLEs[, c(names(all_dat), setdiff(names(le), names(all_dat)))]
  }

  all_dat <- all_dat[, c(names(all_dat)[names(all_dat) != "year"], "year")]
  all_dat
}

# function for extracting the data and rbinding it (extra function for prevention of memory allocation problems)
extract_data_only <- function(GADSdat, convertMiss, convertLabels, dropPartialLabels, convertVariables) {
  gads1 <- extractGADSdat(all_GADSdat = GADSdat, name = names(GADSdat$datList)[1])
  dat1 <- extractData(gads1, convertMiss = convertMiss, convertLabels = convertLabels,
                      dropPartialLabels = dropPartialLabels, convertVariables)
  gads2 <- extractGADSdat(all_GADSdat = GADSdat, name = names(GADSdat$datList)[2])
  dat2 <- extractData(gads2, convertMiss = convertMiss, convertLabels = convertLabels,
                      dropPartialLabels = dropPartialLabels, convertVariables)
  # test_names <- compare_and_order(set1 = names(dat1), set2 = names(dat2), name1 = "GADS 1", name2 = "GADS 2")
  # oder year mit reinnehmen?
  plyr::rbind.fill(dat1, dat2)
}

# converts labels to values
labels2values <- function(dat, labels, convertLabels, convertMiss, dropPartialLabels, convertVariables) {
  if(identical(convertLabels, "numeric")) return(dat)
  # Which variables should their value labels be applied to?
  if(missing(convertVariables)) convertVariables <- names(dat)
  stopifnot(is.character(convertVariables) && length(convertVariables) > 0)
  change_labels <- labels[labels[, "varName"] %in% convertVariables, ]    # careful, from here use only change_labels!
  # check value labels, remove incomplete labels from insertion to protect variables
  if(identical(dropPartialLabels, TRUE)) {
    drop_labels <- unlist(lapply(unique(labels$varName), check_labels, dat = dat, labels = labels,
                                 convertMiss = convertMiss))
    change_labels <- change_labels[!change_labels$varName %in% drop_labels, ]
  }
  # convert labels into values
  changed_variables <- character(0)
  # early return, if no values are to be recoded
  if(nrow(change_labels) == 0) return(dat)
  # recode values
  for(i in seq(nrow(change_labels))) {
    curRow <- change_labels[i, , drop = FALSE]
    #browser()
    if(!is.na(curRow$valLabel)) {
      ## preserve numeric type of variable if possible (although not sure whether this could realistically be the case...)
      curRow$valLabel <- suppressWarnings(eatTools::asNumericIfPossible(curRow$valLabel, force.string = FALSE))
      # so far fastest: maybe car? mh...
      dat[which(dat[, curRow$varName] == curRow$value), curRow$varName] <- curRow$valLabel
      # dat[, curRow$varName] <- ifelse(dat[, curRow$varName] == curRow$value, curRow$valLabel, dat[, curRow$varName])
      changed_variables <- unique(c(curRow$varName, changed_variables))
    }
  }

  # convert characters to factor if specified (keep ordering if possible)
  if(identical(convertLabels, "factor")) {
    dat <- char2fac(dat = dat, labels = labels, vars = changed_variables, convertMiss = convertMiss)
  }
  dat
}

# check if variable is correctly labeled, issues warning
check_labels <- function(varName, dat, labels, convertMiss) {
  # if(varName == "VAR3") browser()
  real_values <- na_omit(unique(dat[[varName]]))
  labeled_values <- na_omit(labels[labels$varName == varName, "value"])
  ## either all labeled
  if(all(real_values %in% labeled_values)) return()
  ## or no labels except missings (if missings are recoded, else this is irrelevant)
  if(identical(convertMiss, TRUE)) {
    labeled_values <- na_omit(labels[labels$varName == varName & labels$missings == "valid", "value"])
    if(length(labeled_values) == 0) return(varName)
  }
  warning("Variable ", varName, " is partially labeled. Value labels will be dropped for this variable.\n",
          "Labeled values are: ", paste(labeled_values, collapse = ", "), call. = FALSE)

  varName
  #warning("Variable ", varName, " is partially labeled. Value labels will be dropped for this variable variable.\nExisting values are: ",
  #        paste(real_values, collapse = ", "), "\n", "Labeled values are: ", paste(labeled_values_noMiss, collapse = ", "), call. = FALSE)
}

na_omit <- function(vec) {
  vec[!is.na(vec)]
}

# convert characters to factor if specified (keep ordering if possible)
char2fac <- function(dat, labels, vars, convertMiss) {
  partially_labeled <- unordered_facs <- vars
  for(i in vars) {
    fac_meta <- labels[labels$varName == i & (is.na(labels$missings) | labels$missings != "miss")  , c("value", "valLabel")]
    ## additionalcolumns relevant, if missings are not converted
    if(convertMiss == FALSE) fac_meta <- labels[labels$varName == i, c("value", "valLabel")]
    fac_meta <- fac_meta[order(fac_meta$value), ]

    ## 3 scenarios: a) ordering possible, b) ordering impossible because no strictly integers from 1 rising,
    # c) Ordering impossible because partially labelled
    if(nrow(fac_meta) < length(unique(dat[!is.na(dat[, i]), i]))) {
      dat[, i] <- factor(dat[, i])
      unordered_facs <- unordered_facs[unordered_facs != i]
    } else{
      partially_labeled <- partially_labeled[partially_labeled != i]
      if(all(fac_meta$value == seq(nrow(fac_meta)))) unordered_facs <- unordered_facs[unordered_facs != i]

      dat[, i] <- factor(dat[, i], levels = fac_meta$valLabel)
    }
  }

  if(length(partially_labeled) > 0) warning("For the following factor variables only incomplete value labels are available, rendering the underlying integers meaningless: ",
                                            paste(partially_labeled, collapse = ", "))
  if(length(unordered_facs) > 0) warning("For the following factor variables the underlying integers can not be preserved due to R-incompatible ordering of numeric values: ",
                                         paste(unordered_facs, collapse = ", "))
  dat
}
