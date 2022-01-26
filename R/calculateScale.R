####
#############################################################################
#' Calculate a scale.
#'
#' Calculate a scale variable based on multiple items.
#'
#' Descriptive statistics (including Cronbach's alpha, credit to the \code{psy} package) are calculated and printed to the console.
#' The new scale variable is automatically inserted right after the last item in the original \code{GADSdat}.
#'
#'@param GADSdat A \code{data.frame} or \code{GADSdat} object.
#'@param items A character vector with all item variable names.
#'@param scale A character vector with the scale name.
#'@param maxNA Maximum number of allowed \code{NA} values on the items.
#'@param reportDescr Should descriptive statistics be reported for the calculated scale.
#'
#'@return Returns a \code{GADSdat} containing the newly computed variable.
#'
#'@examples
#'##
#'items <- paste0("norms_", letters[1:6])
#'pisa_new <- calculateScale(pisa, items = items, scale = "norms")
#'
#'@export
calculateScale <- function(GADSdat, items, scale, maxNA = length(items), reportDescr = FALSE) {
  UseMethod("calculateScale")
}

#'@export
calculateScale.GADSdat <- function(GADSdat, items, scale, maxNA = length(items), reportDescr = FALSE) {
  check_GADSdat(GADSdat)
  if(!is.character(items) || length(items) < 2) stop("'items' needs to be a character vector of at least length 2.")
  if(!is.character(scale) || length(scale) != 1) stop("'scale' needs to be a character vector of length 1.")
  if(scale %in% namesGADS(GADSdat)) stop("'scale' is already an existing variable in 'GADSdat'.")
  if(!is.numeric(maxNA) || length(maxNA) != 1) stop("'maxNA' needs to be a numeric vector of length 1.")
  check_vars_in_GADSdat(GADSdat, vars = items)

  suppressMessages(only_items_gads <- extractVars(GADSdat, items))
  only_items <- extractData(only_items_gads, convertLabels = "numeric", convertMiss = TRUE)
  new_scale <- rowMeans(only_items, na.rm = TRUE)

  # count missings
  miss_items <- only_items
  for(cols in seq(ncol(only_items))) {
    miss_items[, cols] <- is.na(miss_items[, cols])
  }
  miss_count <- rowSums(miss_items, na.rm = FALSE)

  new_scale_narm <- ifelse(miss_count > maxNA, yes = NA, no = new_scale)

  # setup new variable in GADS
  dat <- GADSdat$dat
  dat[, scale] <- new_scale_narm
  suppressMessages(GADSdat_out <- updateMeta(GADSdat, newDat = dat))

  # sort data set
  old_nam <- namesGADS(GADSdat)
  last_item_pos <- max(which(old_nam %in% items))
  new_nam <- append(old_nam, values = scale, after = last_item_pos)
  GADSdat_out2 <- orderLike(GADSdat_out, newOrder = new_nam)

  # calculate descriptives
  if(reportDescr) {
    alpha <- cronbach_alpha(only_items)
    descr <- c(summary(new_scale_narm), alpha = alpha)
    descr2 <- round(descr, 2)
    cat("Descriptives for", scale ,"\n")
    print(descr2)
  }

  GADSdat_out2
}

cronbach_alpha <- function(v1) {
  v1 <- stats::na.omit(v1)
  nv1 <- ncol(v1)
  pv1 <- nrow(v1)
  alpha <- (nv1/(nv1 - 1)) * (1 - sum(apply(v1, 2, stats::var))/stats::var(apply(v1,
                                                                   1, sum)))
  resu <- list(sample.size = pv1, number.of.items = nv1, alpha = alpha)
  resu$alpha
}
