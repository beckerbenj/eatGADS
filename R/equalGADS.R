
#############################################################################
#' Test if two \code{GADSdat} objects are (nearly) equal
#'
#' Run tests to check whether two \code{GADSdat} objects are (nearly) equal. Variable names, number of rows in the data,
#' meta data and data differences are checked and reported as a list output.
#'
#' More detailed checks for individual variables can be performed via \code{\link{inspectDifferences}}
#' and \code{\link{inspectMetaDifferences}}.
#'
#'@param target A \code{GADSdat} object.
#'@param current A \code{GADSdat} object.
#'@param id A character vector of length 1 containing the unique identifier column of both \code{GADSdat}.
#'If specified, both \code{GADSdat} are ordered according to \code{ID} before comparing their data.
#'@param metaExceptions Should certain meta data columns be excluded from the comparison?
#'@param tolerance A numeric value greater than or equal to \code{0}. Differences smaller than \code{tolerance} are not reported.
#'The default value is close to \code{1.5e-8}.
#'
#'@return Returns a list.
#'
#'
#'@export
equalGADS <- function(target, current, id = NULL,
                      metaExceptions = c("display_width", "labeled"), tolerance = sqrt(.Machine$double.eps)) {
  UseMethod("equalGADS")
}
#'@export
equalGADS.GADSdat <- function(target, current, id = NULL,
                              metaExceptions = c("display_width", "labeled"), tolerance = sqrt(.Machine$double.eps)) {
  check_GADSdat(target)
  check_GADSdat(current)
  if(!(is.null(metaExceptions) || is.character(metaExceptions))) {
    stop("'metaExceptions' must be NULL or a character vector.")
  }
  if(any(!metaExceptions %in% c("varLabel", 'format', 'display_width', 'valLabel', 'missings', 'labeled'))) {
    stop("Entries in 'metaExceptions' can only be 'varLabel', 'format', 'display_width', 'labeled', 'valLabel', and 'missings'.")
  }
  # sort if possible
  if(!is.null(id)) {
    check_vars_in_GADSdat(target, vars = id, argName = "id", GADSdatName = "target")
    check_vars_in_GADSdat(current, vars = id, argName = "id", GADSdatName = "current")

    target$dat <- target$dat[order(target$dat[, id]), ]
    current$dat <- current$dat[order(current$dat[, id]), ]
  }

  out <- list()

  # variable names
  target_names <- namesGADS(target)
  current_names <- namesGADS(current)
  out[["names_not_in_1"]] <- setdiff(current_names, target_names)
  out[["names_not_in_2"]] <- setdiff(target_names, current_names)

  # data rows
  out[["data_nrow"]] <- ifelse(nrow(target$dat) == nrow(current$dat), yes = "all.equal", no =
                                  paste0("nrow 1: ", nrow(target$dat), "nrow 2: ", nrow(current$dat)))

  # meta data
  out[["meta_data_differences"]] <- out[["data_differences"]] <- character()
  target_labs <- target$labels
  current_labs <- current$labels

  # ignore irrelevant meta differences
  target_labs$format <- gsub("\\.0$", "", target_labs$format)
  current_labs$format <- gsub("\\.0$", "", current_labs$format)

  metaNames <- names(target_labs)
  if(!is.null(metaExceptions)) metaNames <- setdiff(metaNames, metaExceptions)

  for(i in intersect(target_names, current_names)) {
    #browser()
    target_single_labs <- target_labs[target_labs$varName == i, metaNames]
    current_single_labs <- current_labs[current_labs$varName == i, metaNames]
    rownames(target_single_labs) <- rownames(current_single_labs) <- NULL
    if(!identical(all.equal(target_single_labs, current_single_labs), TRUE)) {
      # temporary fix: ignore pure order differences (long term: there should never be such differences)
      target_single_labs2 <- target_single_labs[order(target_single_labs$value), ]
      current_single_labs2 <- current_single_labs[order(current_single_labs$value), ]
      rownames(target_single_labs2) <- rownames(current_single_labs2) <- NULL
      #if(nrow(target_single_labs) > 1) browser()
      if(!identical(all.equal(target_single_labs2, current_single_labs2), TRUE)) {
        out[["meta_data_differences"]] <- c(out[["meta_data_differences"]], i)
      }
    }
    if(!identical(all.equal(target$dat[[i]], current$dat[[i]], scale = 1, tolerance = tolerance), TRUE)) {
      out[["data_differences"]] <- c(out[["data_differences"]], i)
    }
  }
  out
}
