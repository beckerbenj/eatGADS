
#### Get data from two Gads and Linking Errors
#############################################################################
#' Get data for trend reports.
#'
#' Extracts variables from multiple \code{eatGADS} data bases and a linking error data base.
#' Data can then be extracted from the \code{GADSdat} object via
#' \code{\link{extractData}}. For extracting meta data from a data base or a \code{GADSdat} object see \code{\link{extractMeta}}. To speed
#' up the data loading, \code{\link{getGADS_fast}} is used per default.
#'
#' This function extracts data from multiple GADS data bases and a linking error data base. All data bases have to be created via
#' \code{\link{createGADS}}. The two GADS are joined via \code{rbind()} and a variable \code{year} is added, corresponding to the
#' argument \code{years}. If \code{lePath} is specified, linking errors are also extracted and then merged to the GADS data. Make
#' sure to also extract the key variables necessary for merging the linking errors (the domain variable for all linking errors,
#' additionally the competence level variable for linking errors for competence levels). The \code{GADSdat} object can then further
#' be used via \code{\link{extractData}}. See \code{\link[eatDB]{createDB}} and \code{\link[eatDB]{dbPull}} for further explanation
#' of the querying and merging processes.
#'
#'@param filePaths Character vectors with paths to the \code{eatGADS} db files.
#'@param lePath Path of the linking error db file. If \code{NULL}, no linking errors are added to the data.
#'@param vSelect Variables from all GADS to be selected (as character vector).
#'@param years A numeric vector with identical length as \code{filePaths}.
#'@param fast Should \code{\link{getGADS_fast}} be used for data loading instead of \code{\link{getGADS}}? Using the default is heavily recommended.
#'@param tempPath The directory, in which both GADS will be temporarily stored. Using the default is heavily recommended.
#'
#'@return Returns a \code{GADSdat} object.
#'
#'@examples
#' # See getGADS vignette
#'
#'
#'@export
getTrendsGADS <- function(filePaths, lePath = NULL, vSelect = NULL, years, fast = TRUE, tempPath = tempdir()) {
  # Check for uniqueness of data bases used
  if(is.null(lePath)) {
    if(length(unique(filePaths)) != length(filePaths)) stop("All file arguments have to point to different files.")
  } else {
    all_filePaths <- c(filePaths, lePath)
    if(length(unique(all_filePaths)) != length(all_filePaths)) stop("All file arguments have to point to different files.")
  }
  if(!(length(years) == length(filePaths) && is.numeric(years))) stop("'years' has to be a numeric vector of the same length as 'filePaths'.")
  # check if vSelect in both GADS
  check_keyStrcuture_TrendsGADS(filePaths = filePaths)

  #browser()
  # prepare vSelect for GADS (unique variables are allowed!)
  if(is.null(vSelect)) {
    vSelect_list <- lapply(filePaths, function(filePath) unlist(namesGADS(filePath)))
    vSelect <- unique(unlist(vSelect_list))
  }
  vSelect_checked <- lapply(filePaths, function(filePath) check_vSelect(filePath, vSelect = vSelect))

  # checks for vSelect
  not_in_gads_list <- lapply(vSelect_checked, function(x) x$not_in_gads)
  not_in_any_gads <- Reduce(intersect, not_in_gads_list)
  if(length(not_in_any_gads) > 0) stop("The following selected variables are not in any of the data bases: ",
                                       paste(not_in_any_gads, collapse = ", "))

  lapply(seq_along(vSelect_checked), function(i) {
    vSelect_checked_single <- vSelect_checked[[i]]
    if(length(vSelect_checked_single$in_gads) == 0) stop("No variables from data base ", years[i], " selected.")
    if(length(vSelect_checked_single$not_in_gads) > 0) warning(paste0("The following variables are not in GADS ", years[i],": ",
                                                                      vSelect_checked_single$not_in_gads,
                                                                      ". NAs will be inserted if data is extracted."))
  })

  gList <- lapply(seq_along(filePaths), function(i) {
    filePath <- filePaths[i]
    vSelect_in_gads <- vSelect_checked[[i]][["in_gads"]]
    year <- years[i]

    cat(" -----  Loading GADS", year, "----- \n")
    if(identical(fast, TRUE)) {
      g <- getGADS_fast(vSelect = vSelect_in_gads, filePath = filePath, tempPath = tempPath)
    } else{
      g <- getGADS(vSelect = vSelect_in_gads, filePath = filePath)
    }

    g2 <- add_year(g, year)
  })

  # add linking errors (automatic variable selection)
  gList["LEs"] <- list(NULL)
  if(!is.null(lePath)) {
    leSelect <- make_leSelect(lePath = lePath, vSelect = vSelect)
    if(is.null(leSelect) || length(leSelect) > 0) LEs <- getGADS(filePath = lePath, vSelect = leSelect)
    else message("No linking errors for chosen variables available.")

    # select correct years
    LEs2 <- LEs
    LEs2$dat <- LEs$dat[LEs$dat$year1 %in% years & LEs$dat$year2 %in% years, ]

    gList[[length(gList)]] <- LEs2
  }

  names(gList) <- c(paste0("gads", years), "LEs")

  gads_trend <- do.call(mergeLabels, gList)
  class(gads_trend) <- c("trend_GADSdat", "all_GADSdat", "list")

  gads_trend
}


check_trend_GADSdat <- function(trend_GADSdat) {
  if(is.null(trend_GADSdat$datList[["LEs"]])) {
    trend_GADSdat$datList <- trend_GADSdat$datList[1:2]
  }
  check_all_GADSdat(trend_GADSdat)
}


## Check compatability of trend data bases
check_keyStrcuture_TrendsGADS <- function(filePaths) {
  # check levels and keys
  keys <- lapply(filePaths, eatDB::dbKeys)
  pKeys <- lapply(keys, function(x) x$pkList)
  fKeys <- lapply(keys, function(x) x$fkList)
  lapply(names(pKeys[[1]]), function(dataTable_name) {
    first_db_dt <- pKeys[[1]][[dataTable_name]]
    lapply(pKeys, function(other_db) {
      other_db_dt <- other_db[[dataTable_name]]
      if(!identical(first_db_dt, other_db_dt)) stop("Trend data bases must have the same primary key structure:")
    })
  })
  lapply(names(fKeys[[1]]), function(dataTable_name) {
    first_db_dt <- fKeys[[1]][[dataTable_name]]
    lapply(fKeys, function(other_db) {
      other_db_dt <- other_db[[dataTable_name]]
      if(!identical(first_db_dt, other_db_dt)) stop("Trend data bases must have the same foreign key structure:")
    })
  })
  return()
}


