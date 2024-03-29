#### extractData
#############################################################################
#' Extract Data
#'
#' Extract \code{data.frame} from a \code{GADSdat} object for analyses in \code{R}. Value labels can be
#'  selectively applied via defining \code{convertLabels} and \code{covertVariables}.
#'  For extracting meta data see \code{\link{extractMeta}}.
#'
#' A \code{GADSdat} object includes actual data (\code{GADSdat$dat}) and the corresponding meta data information
#' (\code{GADSdat$labels}). \code{extractData} extracts the data and applies relevant meta data on value level (missing conversion, value labels),
#' so the data can be used for analyses in \code{R}. Variable labels are retained as \code{label} attributes on column level.
#'
#' If \code{factor} are extracted via \code{convertLabels == "factor"}, an attempt is made to preserve the underlying integers.
#' If this is not possible, a warning is issued.
#' As \code{SPSS} has almost no limitations regarding the underlying values of labeled
#' integers and \code{R}'s \code{factor} format is very strict (no \code{0}, only integers increasing by \code{+ 1}),
#' this procedure can lead to frequent problems.
#'
#'@param GADSdat A \code{GADSdat} object.
#'@param convertMiss Should values tagged as missing values be recoded to \code{NA}?
#'@param convertLabels If \code{"numeric"}, values remain as numerics. If \code{"factor"} or
#' \code{"character"}, values are recoded to their labels. Corresponding variable type is applied.
#'@param convertVariables Character vector of variables names, which labels should be applied to.
#' All other variables remain as numeric variables in the data.
#'If not specified [default], value labels are applied to all variables for which labels are available.
#' Variable names not in the actual \code{GADS} are silently dropped.
#'@param dropPartialLabels Should value labels for partially labeled variables be dropped?
#'If \code{TRUE}, the partial labels will be dropped. If \code{FALSE}, the variable will be converted
#'to the class specified in \code{convertLabels}.
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
#'# convert only some variables to factor, all others remain numeric
#'dat <- extractData(pisa, convertLabels = "factor", convertVariables = c("schtype", "ganztag"))
#'
#'# convert only some variables to character, all others remain numeric
#'dat <- extractData(pisa, convertLabels = "factor", convertVariables = c("schtype", "ganztag"))
#'# schtype is now character
#'table(dat$schtype)
#'# schtype remains numeric
#'table(dat$gender)
#'
#'@export
extractData <- function(GADSdat, convertMiss = TRUE, convertLabels = "character", convertVariables = NULL, dropPartialLabels = TRUE) {
  UseMethod("extractData")
}

#'@export
extractData.GADSdat <- function(GADSdat, convertMiss = TRUE, convertLabels = "character", convertVariables = NULL, dropPartialLabels = TRUE) {
  check_GADSdat(GADSdat)
  if(length(convertLabels) != 1 || !convertLabels %in% c("character", "factor", "numeric")) stop("Argument convertLabels incorrectly specified.")
  dat <- GADSdat$dat
  labels <- GADSdat$labels
  ## missings
  if(identical(convertMiss, TRUE)) dat <- miss2NA(GADSdat)
  ## labels
  dat <- labels2values(dat = dat, labels = labels, convertLabels = convertLabels, convertMiss = convertMiss,
                       dropPartialLabels = dropPartialLabels, convertVariables = convertVariables)
  ## varLabels
  dat <- varLabels_as_labels(dat = dat, labels = labels)
  dat
}

#'@export
extractData.trend_GADSdat <- function(GADSdat, convertMiss = TRUE, convertLabels = "character", convertVariables = NULL, dropPartialLabels = TRUE) {
  check_trend_GADSdat(GADSdat)
  if("LEs" %in% names(GADSdat$datList)) stop("Linking errors are no longer supported by extractData. Use extractDataOld() instead.")

  all_dat <- extract_data_only(GADSdat = GADSdat, convertMiss = convertMiss, convertLabels = convertLabels,
                               dropPartialLabels = dropPartialLabels, convertVariables = convertVariables)

  all_dat <- all_dat[, c(names(all_dat)[names(all_dat) != "year"], "year")]
  all_dat
}

# function for extracting the data and rbinding it (extra function for prevention of memory allocation problems)
extract_data_only <- function(GADSdat, convertMiss, convertLabels, dropPartialLabels, convertVariables) {
  #browser()
  old_class <- class(GADSdat)
  GADSdat$datList <- GADSdat$datList[names(GADSdat$datList) != "LEs"]
  class(GADSdat) <- old_class

  dat_list <- lapply(names(GADSdat$datList), function(nam) {
    gads <- extractGADSdat(all_GADSdat = GADSdat, name = nam)
    dat <- extractData(gads, convertMiss = convertMiss, convertLabels = convertLabels,
                        dropPartialLabels = dropPartialLabels, convertVariables = convertVariables)
    dat
  })

  do.call(plyr::rbind.fill, dat_list)
  # gads1 <- extractGADSdat(all_GADSdat = GADSdat, name = names(GADSdat$datList)[1])
  # dat1 <- extractData(gads1, convertMiss = convertMiss, convertLabels = convertLabels,
  #                     dropPartialLabels = dropPartialLabels, convertVariables)
  # gads2 <- extractGADSdat(all_GADSdat = GADSdat, name = names(GADSdat$datList)[2])
  # dat2 <- extractData(gads2, convertMiss = convertMiss, convertLabels = convertLabels,
  #                     dropPartialLabels = dropPartialLabels, convertVariables)
  # test_names <- compare_and_order(set1 = names(dat1), set2 = names(dat2), name1 = "GADS 1", name2 = "GADS 2")
  # oder year mit reinnehmen?
  #plyr::rbind.fill(dat1, dat2)
}

# converts labels to values
labels2values <- function(dat, labels, convertLabels, convertMiss, dropPartialLabels, convertVariables) {
  if(identical(convertLabels, "numeric")) return(dat)
  # Which variables should their value labels be applied to?
  if(is.null(convertVariables)) convertVariables <- names(dat)
  stopifnot(is.character(convertVariables) && length(convertVariables) > 0)
  change_labels <- labels[labels[, "varName"] %in% convertVariables, ]    # careful, from here use only change_labels!
  # check value labels, remove incomplete labels from insertion to protect variables
  if(identical(dropPartialLabels, TRUE)) {
    drop_labels <- unlist(lapply(unique(labels$varName), check_labels, dat = dat, labels = labels,
                                 convertMiss = convertMiss))
    change_labels <- change_labels[!change_labels$varName %in% drop_labels, ]
  }
  # early return, if no values are to be recoded
  if(nrow(change_labels) == 0) return(dat)

  # convert labels into values (use recode function from applyChangeMeta)
  change_table <- change_labels[, c("varName", "value", "valLabel")]
  names(change_table) <- c("varName", "value", "value_new")
  dat2 <- recode_dat(dat, changeTable = change_table)

  # identify modified variables
  is_character_old <- unlist(lapply(dat, function(var) is.character(var)))
  is_character_new <- unlist(lapply(dat2, function(var) is.character(var)))
  changed_variables <- names(dat2)[is_character_new & !is_character_old]

  # convert characters to factor if specified (keep ordering if possible)
  if(identical(convertLabels, "factor")) {
    dat2 <- char2fac(dat = dat2, labels = labels, vars = changed_variables, convertMiss = convertMiss)
  }
  dat2
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
char2fac <- function(dat, labels, vars, convertMiss, ordered = FALSE) {
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

      dat[, i] <- factor(dat[, i], levels = fac_meta$valLabel, ordered = ordered)
    }
  }

  if(length(partially_labeled) > 0) warning("For the following factor variables only incomplete value labels are available, rendering the underlying integers meaningless: ",
                                            paste(partially_labeled, collapse = ", "))
  if(length(unordered_facs) > 0) warning("For the following factor variables the underlying integers can not be preserved due to R-incompatible ordering of numeric values: ",
                                         paste(unordered_facs, collapse = ", "))
  dat
}

varLabels_as_labels <- function(dat, labels) {
  for(i in names(dat)) {
    varLabel <- labels[match(i, labels$varName), "varLabel"]
    if(!is.na(varLabel)) attr(dat[[i]], "label") <- varLabel
  }
  dat
}
