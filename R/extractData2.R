#### extractData
#############################################################################
#' Extract Data 2
#'
#' Extract \code{data.frame} from a \code{GADSdat} object for analyses in \code{R}. Per default, missing codes are applied but
#' value labels are dropped. Alternatively, value labels can be selectively applied via
#' \code{labels2character}, \code{labels2factor}, and \code{labels2ordered}.
#' For extracting meta data see \code{\link{extractMeta}}.
#'
#' A \code{GADSdat} object includes actual data (\code{GADSdat$dat}) and the corresponding meta data information
#' (\code{GADSdat$labels}). \code{extractData2} extracts the data and applies relevant meta data on value level
#' (missing tags, value labels),
#' so the data can be used for analyses in \code{R}. Variable labels are retained as \code{label} attributes on column level.
#'
#' If \code{factor} are extracted via \code{labels2factor} or \code{labels2ordered}, an attempt is made to preserve the underlying integers.
#' If this is not possible, a warning is issued.
#' As \code{SPSS} has almost no limitations regarding the underlying values of labeled
#' integers and \code{R}'s \code{factor} format is very strict (no \code{0}, only integers increasing by \code{+ 1}),
#' this procedure can lead to frequent problems.
#'
#' If multiple values of the same variable are assigned the same value label and the variable should be transformed to
#' \code{character}, \code{factor}, or \code{ordered}, a warning is issued and the transformation is correctly performed.
#'
#'@param GADSdat A \code{GADSdat} object.
#'@param convertMiss Should values tagged as missing values be recoded to \code{NA}?
#'@param labels2character For which variables should values be recoded to their labels? The resulting variables
#'are of type \code{character}.
#'@param labels2factor For which variables should values be recoded to their labels? The resulting variables
#'are of type \code{factor}.
#'@param labels2ordered For which variables should values be recoded to their labels? The resulting variables
#'are of type \code{ordered}.
#'@param dropPartialLabels Should value labels for partially labeled variables be dropped?
#'If \code{TRUE}, the partial labels will be dropped. If \code{FALSE}, the variable will be converted
#'to the class specified in \code{labels2character}, \code{labels2factor}, or \code{labels2ordered}.
#'
#'@return Returns a data frame.
#'
#'@examples
#'# Extract Data for Analysis
#'dat <- extractData2(pisa)
#'
#'# convert only some variables to character, all others remain numeric
#'dat <- extractData2(pisa, labels2character = c("schtype", "ganztag"))
#'
#'# convert only some variables to factor, all others remain numeric
#'dat <- extractData2(pisa, labels2factor = c("schtype", "ganztag"))
#'
#'# convert all labeled variables to factors
#'dat <- extractData2(pisa, labels2factor = namesGADS(pisa))
#'
#'# convert somme variables to factor, some to character
#'dat <- extractData2(pisa, labels2character = c("schtype", "ganztag"),
#'                           labels2factor = c("migration"))
#'
#'@export
extractData2 <- function(GADSdat,
                         convertMiss = TRUE,
                         labels2character = NULL,
                         labels2factor = NULL,
                         labels2ordered = NULL,
                         dropPartialLabels = TRUE) {
  UseMethod("extractData2")
}

#'@export
extractData2.GADSdat <- function(GADSdat,
                                 convertMiss = TRUE,
                                 labels2character = NULL,
                                 labels2factor = NULL,
                                 labels2ordered = NULL,
                                 dropPartialLabels = TRUE) {
  check_GADSdat(GADSdat)
  # input validation
  if(!is.null(labels2character)) check_vars_in_GADSdat(GADSdat, labels2character)
  if(!is.null(labels2factor)) check_vars_in_GADSdat(GADSdat, labels2factor)
  #browser()
  if(!is.null(labels2character) && !is.null(labels2factor)) {
    dups <- c(labels2character, labels2factor)[duplicated(c(labels2character, labels2factor))]
    if(length(dups) > 0) stop("The following variables are both in 'labels2character' and 'labels2factor': ",
                              paste(dups, collapse = ", "))
  }
  if(!is.null(labels2character) && !is.null(labels2ordered)) {
    dups <- c(labels2character, labels2ordered)[duplicated(c(labels2character, labels2ordered))]
    if(length(dups) > 0) stop("The following variables are both in 'labels2character' and 'labels2ordered': ",
                              paste(dups, collapse = ", "))
  }
  if(!is.null(labels2ordered) && !is.null(labels2factor)) {
    dups <- c(labels2ordered, labels2factor)[duplicated(c(labels2ordered, labels2factor))]
    if(length(dups) > 0) stop("The following variables are both in 'labels2ordered' and 'labels2factor': ",
                              paste(dups, collapse = ", "))
  }

  dat <- GADSdat$dat
  labels <- GADSdat$labels
  ## missings
  if(identical(convertMiss, TRUE)) dat <- miss2NA(GADSdat)
  ## labels
  dat <- labels2values2(dat = dat, labels = labels, convertMiss = convertMiss, dropPartialLabels = dropPartialLabels,
                       labels2character = labels2character, labels2factor = labels2factor, labels2ordered = labels2ordered)
  ## varLabels
  dat <- varLabels_as_labels(dat = dat, labels = labels)
  dat
}

#'@export
extractData2.trend_GADSdat <- function(GADSdat,
                                       convertMiss = TRUE,
                                       labels2character = NULL,
                                       labels2factor = NULL,
                                       labels2ordered = NULL,
                                       dropPartialLabels = TRUE) {
  # Input validation
  check_trend_GADSdat(GADSdat)
  if(!is.null(labels2character) && !is.character(labels2character)) stop("'labels2character' must be a character vector.")
  if(!is.null(labels2factor) && !is.character(labels2factor)) stop("'labels2factor' must be a character vector.")
  if(!is.null(labels2ordered) && !is.character(labels2ordered)) stop("'labels2ordered' must be a character vector.")
  all_GADSdat_names <- unique(unlist(namesGADS(GADSdat)))
  check_vars_in_vector(all_GADSdat_names, vars = labels2character, vec_nam = "GADSdats")
  check_vars_in_vector(all_GADSdat_names, vars = labels2factor, vec_nam = "GADSdats")
  check_vars_in_vector(all_GADSdat_names, vars = labels2ordered, vec_nam = "GADSdats")

  dat_list <- lapply(names(GADSdat$datList), function(nam) {
    gads <- extractGADSdat(all_GADSdat = GADSdat, name = nam)

    single_labels2character <- labels2character[labels2character %in% namesGADS(gads)]
    single_labels2factor <- labels2factor[labels2factor %in% namesGADS(gads)]
    single_labels2ordered <- labels2ordered[labels2ordered %in% namesGADS(gads)]

    dat <- extractData2(gads, convertMiss = convertMiss, labels2character = single_labels2character, labels2factor = single_labels2factor,
                       labels2ordered = single_labels2ordered, dropPartialLabels = dropPartialLabels)
    dat
  })

  dat_out <- do.call(plyr::rbind.fill, dat_list)
  dat_out[, c(setdiff(names(dat_out), "year"), "year")]
}


# converts labels to values
labels2values2 <- function(dat, labels, convertMiss, dropPartialLabels, labels2character, labels2factor, labels2ordered) {
  if(is.null(labels2character) && is.null(labels2factor)) return(dat)
  # Which variables should their value labels be applied to?
  convertVariables <- c(labels2character, labels2factor, labels2ordered)
  #stopifnot(is.character(convertVariables) && length(convertVariables) > 0)

  change_labels <- labels[labels[, "varName"] %in% convertVariables, ]    # careful, from here use only change_labels!
  # check value labels, remove incomplete labels from insertion to protect variables
  if(identical(dropPartialLabels, TRUE)) {
    drop_labels <- unlist(lapply(unique(labels$varName), FUN = check_labels, dat = dat, labels = labels,
                                 convertMiss = convertMiss))
    change_labels <- change_labels[!change_labels$varName %in% drop_labels, ]
  }
  # early return, if no values are to be recoded
  if(nrow(change_labels) == 0) return(dat)

  # check for duplicate value labels (unfortunately possible in SPSS)
  vars_with_duplicate_valLabels <- change_labels[duplicated(change_labels[, c("varName", "valLabel")]), "varName"]
  if(length(vars_with_duplicate_valLabels) > 0) {
    for(nam in unique(vars_with_duplicate_valLabels)) {
      single_change_labels <- change_labels[change_labels$varName == nam, ]
      dup_valLabels <- single_change_labels[duplicated(single_change_labels$valLabel), "valLabel"]
      # exclude NAs. If valLables are NA, no label is applied anyway
      dup_valLabels <- dup_valLabels[!is.na(dup_valLabels)]
      affected_values <- single_change_labels[single_change_labels$valLabel == dup_valLabels, ]
      for(dup_valLabel in dup_valLabels) {
        warning("Duplicate value label in variable ", nam, ". The following values (see value column) will be recoded into the same value label (see valLabel column):\n",
                eatTools::print_and_capture(affected_values))

        # recode actual data to prevent any potential issues with char2fac later
        value_lookup <- data.frame(oldValues = affected_values[, "value"],
                                   newValues = affected_values[1, "value"])
        dat[, nam] <- eatTools::recodeLookup(dat[, nam], value_lookup)

        # remove superfluous meta data
        change_labels <- change_labels[!(change_labels$varName == nam &
                                           change_labels$value %in% affected_values[-(1), "value"]), ]
  }}}


  # convert labels into values (use recode function from applyChangeMeta)
  change_table <- change_labels[, c("varName", "value", "valLabel")]
  names(change_table) <- c("varName", "value", "value_new")
  dat2 <- recode_dat(dat, changeTable = change_table)

  # identify modified variables
  is_character_old <- unlist(lapply(dat, function(var) is.character(var)))
  is_character_new <- unlist(lapply(dat2, function(var) is.character(var)))
  changed_variables <- names(dat2)[is_character_new & !is_character_old]

  # convert characters to factor if specified (keep ordering if possible)
  #if(!is.null(labels2ordered)) browser()
  changed_variables_labels2factor <- intersect(labels2factor, changed_variables)
  changed_variables <- setdiff(changed_variables, changed_variables_labels2factor)
  if(length(changed_variables_labels2factor) > 0) {
    dat2 <- char2fac(dat = dat2, labels = change_labels, vars = changed_variables_labels2factor, convertMiss = convertMiss, ordered = FALSE)
  }
  changed_variables_labels2ordered <- intersect(labels2ordered, changed_variables)
  if(length(changed_variables_labels2ordered) > 0) {
    dat2 <- char2fac(dat = dat2, labels = change_labels, vars = changed_variables_labels2ordered, convertMiss = convertMiss, ordered = TRUE)
  }
  dat2
}

# check if variable is correctly labeled, issues warning
check_labels <- function(varName, dat, labels, convertMiss) {
  # if(varName == "VAR3") browser()
  real_values <- stats::na.omit(unique(dat[[varName]]))
  labeled_values <- stats::na.omit(labels[labels$varName == varName, "value"])
  ## either all labeled
  if(all(real_values %in% labeled_values)) return()
  ## or no labels except missings (if missings are recoded, else this is irrelevant)
  if(identical(convertMiss, TRUE)) {
    labeled_values <- stats::na.omit(labels[labels$varName == varName & labels$missings == "valid", "value"])
    if(length(labeled_values) == 0) return(varName)
  }
  warning("Variable ", varName, " is partially labeled. Value labels will be dropped for this variable.\n",
          "Labeled values are: ", paste(labeled_values, collapse = ", "), call. = FALSE)

  varName
  #warning("Variable ", varName, " is partially labeled. Value labels will be dropped for this variable variable.\nExisting values are: ",
  #        paste(real_values, collapse = ", "), "\n", "Labeled values are: ", paste(labeled_values_noMiss, collapse = ", "), call. = FALSE)
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
