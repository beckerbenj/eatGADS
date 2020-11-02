
#### Shorten multiple text variables
#############################################################################
#' Shorten multiple text variables while giving NA codes.
#'
#' Shorten text variables from a certain number on while coding overflowing answers as complete missings.
#'
#' In some cases, multiple text variables contain the information of one variable (e.g. multiple answers to an open item).
#' If this is a case, sometimes the number text variables displaying this variable should be limited. \code{remove2NAchar}
#' allows shortening multiple character variables, this means character variables after \code{max_num} are removed
#' from the \code{GADSdat}. Cases, which had valid responses on these removed variables are coded as missings (using
#' \code{na_value} and \code{na_label}).
#'
#'@param GADSdat A \code{GADSdat} object.
#'@param vars A character vector with the names of the text variables.
#'@param max_num Maximum number of text variables. Additional text variables will be removed and NA codes given accordingly.
#'@param na_value Which NA value should be given in cases of too many values on text variables.
#'@param na_label Which value label should be given to the \code{na_value}.
#'
#'@return Returns the modified \code{GADSdat}.
#'
#'@examples
#'## create an example GADSdat
#'example_df <- data.frame(ID = 1:4,
#'                         citizenship1 = c("German", "English", "missing by design", "Chinese"),
#'                         citizenship2 = c(NA, "German", "missing by design", "Polish"),
#'                         citizenship3 = c(NA, NA, NA, "German"),
#'                         stringsAsFactors = FALSE)
#'gads <- import_DF(example_df)
#'
#'## shorten character variables
#'gads2 <- remove2NAchar(gads, vars = c("citizenship1", "citizenship2", "citizenship3"),
#'                       na_value = -99, na_label = "missing: too many answers")
#'
#'
#'@export
remove2NAchar <- function(GADSdat, vars, max_num = 2, na_value, na_label) {
  UseMethod("remove2NAchar")
}

#'@export
remove2NAchar.GADSdat <- function(GADSdat, vars, max_num = 2, na_value, na_label) {
  check_GADSdat(GADSdat)
  if(!is.numeric(max_num) || length(max_num) != 1) stop("'max_num' needs to be a single numeric value greater than 0.")
  if(!is.numeric(na_value) || length(na_value) != 1) stop("'na_value' needs to be a single numeric value.")
  if(!is.character(na_label) || length(na_label) != 1) stop("'na_label' needs to be a single character value.")

  suppressMessages(miniGADS <- extractVars(GADSdat, vars = vars))
  mini_dat_ori <- extractData(miniGADS)
  mini_dat <- max_num_strings2NA(mini_dat_ori, max_num = max_num, na_value = na_value)
  # replace old variables (to maintain original column ordering)
  dat <- GADSdat$dat
  for(i in names(mini_dat)) {
    dat[, i] <- mini_dat[, i]
  }

  # cut text variables
  remove_vars <- vars[-(1:max_num)]
  dat2 <- dat[, !names(dat) %in% remove_vars, drop = FALSE]
  # restore specific missing codes in character variables
  missing_values <- unique(GADSdat$labels[which(GADSdat$labels$varName %in% vars & GADSdat$labels$missings == "miss"), "value"])
  for(i in vars[!vars %in% remove_vars]) {
    dat2[, i] <- ifelse(GADSdat$dat[, i] %in% missing_values, yes = GADSdat$dat[, i], no = dat2[, i])
  }

  GADSdat_out <- updateMeta(GADSdat, dat2)
  for(i in vars[!vars %in% remove_vars]) {
    GADSdat_out <- changeValLabels(GADSdat_out, varName = i, value = na_value, valLabel = na_label)
    GADSdat_out <- changeMissings(GADSdat_out, varName = i, value = na_value, missings = "miss")
  }
  GADSdat_out
}

# count text variables, give missings if more than x left
max_num_strings2NA <- function(dat, max_num, na_value) {
  #dat[, vars] <- ifelse(!is.na(dat[, max_num]), yes = NA, no = dat[, vars])
  stopifnot(is.numeric(max_num) && length(max_num) == 1)
  stopifnot(ncol(dat) > 1)

  max_var <- names(dat)[max_num + 1]
  if(max_num >= ncol(dat)) return(dat)

  for(i in seq(nrow(dat))) {
    if(!is.na(dat[i, max_var])) {
      dat[i, ] <- na_value
    }
  }
  dat
}
