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
#'@param labels2character For which variables should values be recoded to their labels? The resulting variables
#'are of type \code{character}.
#'@param labels2factor For which variables should values be recoded to their labels? The resulting variables
#'are of type \code{factor}.
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
extractData2 <- function(GADSdat, convertMiss = TRUE, labels2character = namesGADS(GADSdat), labels2factor = NULL, dropPartialLabels = TRUE) {
  UseMethod("extractData2")
}

#'@export
extractData2.GADSdat <- function(GADSdat, convertMiss = TRUE, labels2character = namesGADS(GADSdat), labels2factor = NULL, dropPartialLabels = TRUE) {
  check_GADSdat(GADSdat)
  # input validation
  if(!is.null(labels2character)) check_vars_in_GADSdat(GADSdat, labels2character)
  if(!is.null(labels2factor)) check_vars_in_GADSdat(GADSdat, labels2factor)
  if(!is.null(labels2character) && !is.null(labels2factor)) {
    dups <- c(labels2character, labels2factor)[duplicated(c(labels2character, labels2factor))]
    if(length(dups) > 0) stop("The following variables are both in 'labels2character' and 'labels2factor': ",
                              paste(dups, collapse = ", "))
  }

  dat <- GADSdat$dat
  labels <- GADSdat$labels
  ## missings
  if(identical(convertMiss, TRUE)) dat <- miss2NA(GADSdat)
  ## labels
  dat <- labels2values2(dat = dat, labels = labels, convertMiss = convertMiss, dropPartialLabels = dropPartialLabels,
                       labels2character = labels2character, labels2factor = labels2factor)
  ## varLabels
  dat <- varLabels_as_labels(dat = dat, labels = labels)
  dat
}

#'@export
extractData2.trend_GADSdat <- function(GADSdat, convertMiss = TRUE, labels2character = namesGADS(GADSdat), labels2factor = NULL, dropPartialLabels = TRUE) {
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
labels2values2 <- function(dat, labels, convertMiss, dropPartialLabels, labels2character, labels2factor) {
  if(is.null(labels2character) && is.null(labels2factor)) return(dat)
  # Which variables should their value labels be applied to?
  convertVariables <- c(labels2character, labels2factor)
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
  changed_variables_labels2factor <- intersect(labels2factor, changed_variables)
  if(length(changed_variables_labels2factor) > 0) {
    dat <- char2fac(dat = dat, labels = labels, vars = changed_variables_labels2factor, convertMiss = convertMiss)
  }
  dat
}
