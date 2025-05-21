#' Test if two \code{GADSdat} objects are (nearly) equal
#'
#' Run tests to check whether two \code{GADSdat} objects are (nearly) equal.
#' \code{equalData} compares variable names, number of rows in the data, and data differences.
#' \code{equalMeta} compares variable names and meta data differences.
#' \code{equalGADS} combines both functions. All functions produce a test report in list format.
#'
#' More detailed checks for individual variables can be performed via \code{\link{inspectDifferences}}
#' and \code{\link{inspectMetaDifferences}}.
#'
#'@param target A \code{GADSdat} object.
#'@param current A \code{GADSdat} object.
#'@param id A character vector containing the unique identifier columns of both \code{GADSdat}.
#'If specified, both \code{GADSdat} are ordered according to \code{id} before comparing their data.
#'@param metaExceptions Should certain meta data columns be excluded from the comparison?
#'@param tolerance A numeric value greater than or equal to \code{0}. Differences smaller than \code{tolerance} are not reported.
#'The default value is close to \code{1.5e-8}.
#'
#'@returns
#' Returns a list with the following entries:
#' \item{names_not_in_1}{Which variables are included in \code{current} but not in \code{target}?}
#' \item{names_not_in_2}{Which variables are included in \code{target} but not in \code{current}?}
#' \item{data_nrow}{Do the actual data sets have the same number of rows?}
#' \item{data_differences}{For which variables are the data different?}
#' \item{meta_data_differences}{For which variables are the meta data different?}
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
  equal_data <- equalData(target = target, current = current, id = id, tolerance = tolerance)
  equal_meta <- equalMeta(target = target, current = current, metaExceptions = metaExceptions)
  c(equal_data, equal_meta["meta_data_differences"])
  #list(equal_data$names_not_in_1, equal_data$names_not_in_2,
  #     equal_data$data_differences, equal_data$data_nrow,
  #     equal_meta$meta_data_differences)
}

#'@rdname equalGADS
#'@export
equalData <- function(target, current, id = NULL, tolerance = sqrt(.Machine$double.eps)) {
  check_GADSdat(target)
  check_GADSdat(current)

  if(!is.null(id)) {
    check_vars_in_GADSdat(target, vars = id, argName = "id", GADSdatName = "target")
    check_vars_in_GADSdat(current, vars = id, argName = "id", GADSdatName = "current")

    target$dat <- target$dat[do.call(order, target$dat[id]), , drop = FALSE]
    current$dat <- current$dat[do.call(order, current$dat[id]), , drop = FALSE]
  }

  out <- list()

  # variable names
  target_names <- namesGADS(target)
  current_names <- namesGADS(current)
  out[["names_not_in_1"]] <- setdiff(current_names, target_names)
  out[["names_not_in_2"]] <- setdiff(target_names, current_names)

  # data rows
  out[["data_differences"]] <- character()
  out[["data_nrow"]] <- ifelse(nrow(target$dat) == nrow(current$dat), yes = "all.equal", no =
                                 paste0("nrow 1: ", nrow(target$dat), "; nrow 2: ", nrow(current$dat)))

  for(i in intersect(target_names, current_names)) {
    if(!identical(all.equal(target$dat[[i]], current$dat[[i]], scale = 1, tolerance = tolerance), TRUE)) {
      out[["data_differences"]] <- c(out[["data_differences"]], i)
    }
  }
  out
}

#'@rdname equalGADS
#'@export
equalMeta <- function(target, current, metaExceptions = c("display_width", "labeled")) {
  check_GADSdat(target)
  check_GADSdat(current)
  if(!(is.null(metaExceptions) || is.character(metaExceptions))) {
    stop("'metaExceptions' must be NULL or a character vector.")
  }
  if(any(!metaExceptions %in% c("varLabel", 'format', 'display_width', 'valLabel', 'missings', 'labeled'))) {
    stop("Entries in 'metaExceptions' can only be 'varLabel', 'format', 'display_width', 'labeled', 'valLabel', and 'missings'.")
  }

  out <- list()

  # variable names
  target_names <- namesGADS(target)
  current_names <- namesGADS(current)
  out[["names_not_in_1"]] <- setdiff(current_names, target_names)
  out[["names_not_in_2"]] <- setdiff(target_names, current_names)

  # meta data
  out[["meta_data_differences"]] <- character()
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
  }
  out
}
