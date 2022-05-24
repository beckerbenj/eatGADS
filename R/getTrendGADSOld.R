
#### Get data from two Gads and Linking Errors
#############################################################################
#' Get data for trend reports.
#'
#' Support for linking error data bases has been removed from \code{eatGADS}.
#' \code{getGADSold} provides (for the time being) backwards compatibility, so linking errors can still be extracted automatically.
#'
#' See \code{\link{getGADS}} for the current functionality.
#'
#'@param filePath1 Path of the first \code{eatGADS} db file.
#'@param filePath2 Path of the second \code{eatGADS} db file.
#'@param lePath Path of the linking error db file. If \code{NULL}, no linking errors are added to the data.
#'@param vSelect Variables from both GADS to be selected (as character vector).
#'@param years A numeric vector of length 2. The first elements corresponds to \code{filePath1}, the second element to \code{filePath2}.
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
getTrendGADSOld <- function(filePath1, filePath2, lePath = NULL, vSelect = NULL, years, fast = TRUE, tempPath = tempdir()) {
  # Check for uniqueness of data bases used
  if(is.null(lePath)) {
    if(length(unique(c(filePath1, filePath2))) != 2) stop("All file arguments have to point to different files.")
  } else {
    if(length(unique(c(filePath1, filePath2, lePath))) != 3) stop("All file arguments have to point to different files.")
  }
  if(!(length(years) == 2 && is.numeric(years))) stop("years has to be a numeric vector of length 2.")
  # check if vSelect in both GADS
  check_keyStrcuture_TrendGADS(filePath1 = filePath1, filePath2 = filePath2)

  # prepare vSelect for GADS (unique variables are allowed!)
  if(is.null(vSelect)) {
    vSelect <- unique(c(unlist(namesGADS(filePath1)), unlist(namesGADS(filePath2))))
  }
  vSelect1 <- list(in_gads = vSelect)
  vSelect2 <- list(in_gads = vSelect)

  vSelect1 <- check_vSelect(namesGADS(filePath1), vSelect = vSelect)
  vSelect2 <- check_vSelect(namesGADS(filePath2), vSelect = vSelect)
  not_in_both_gads <- intersect(vSelect1$not_in_gads, vSelect2$not_in_gads)
  if(length(not_in_both_gads) > 0) stop("Variables ", not_in_both_gads, " are in neither of both data bases.")
  if(!length(vSelect1$in_gads) > 0) stop("No variables from first data base selected.")
  if(!length(vSelect2$in_gads) > 0) stop("No variables from second data base selected.")
  # warn about added missings
  if(length(vSelect1$not_in_gads) > 0) warning(paste0("The following variables are not in GADS ", years[1],": ", vSelect1$not_in_gads,". NAs will be inserted if data is extracted."))
  if(length(vSelect2$not_in_gads) > 0) warning(paste0("The following variables are not in GADS ", years[2],": ", vSelect2$not_in_gads,". NAs will be inserted if data is extracted."))

  if(!identical(fast, TRUE)) {
    g1 <- getGADS(vSelect = vSelect1$in_gads, filePath = filePath1)
    g2 <- getGADS(vSelect = vSelect2$in_gads, filePath = filePath2)
  } else {
    cat(" -----  Loading first GADS ----- \n")
    g1 <- getGADS_fast(vSelect = vSelect1$in_gads, filePath = filePath1, tempPath = tempPath)
    cat(" -----  Loading second GADS ----- \n")
    g2 <- getGADS_fast(vSelect = vSelect2$in_gads, filePath = filePath2, tempPath = tempPath)
  }

  # add year
  g1 <- add_year(g1, years[1])
  g2 <- add_year(g2, years[2])

  # add linking errors (automatic variable selection)
  LEs <- NULL
  if(!is.null(lePath)) {
    leSelect <- make_leSelect(lePath = lePath, vSelect = vSelect)
    if(is.null(leSelect) || length(leSelect) > 0) LEs <- getGADS(filePath = lePath, vSelect = leSelect)
    else message("No linking errors for chosen variables available.")
  }

  gList <- list(g1, g2, LEs)
  names(gList) <- c(paste0("gads", years), "LEs")

  #browser()
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
check_keyStrcuture_TrendGADS <- function(filePath1, filePath2) {
  # check levels and keys
  k1 <- eatDB::dbKeys(filePath1)
  k2 <- eatDB::dbKeys(filePath2)

  pkEquals <- unlist(Map(function(pk1, pk2) identical(pk1, pk2), pk1 = k1$pkList, pk2 = k2$pkList))
  fkEquals <- unlist(Map(function(fk1, fk2) identical(fk1, fk2), fk1 = k1$fkList, fk2 = k2$fkList))
  if(!all(pkEquals)) stop("Trend data bases must have the same primary key structure:")
  if(!all(fkEquals)) stop("Trend data bases must have the same foreign key structure.")

  #
  return()
}

# automaticall generate variable selection for linking error data base
make_leSelect <- function(lePath, vSelect) {
  namLE <- namesGADS(lePath)
  if(is.null(vSelect)) return(NULL)
  LE_variables <- grep("^LE_", unlist(namLE), value = TRUE)
  dep_variables <- gsub(pattern = "^LE_", replacement = "", LE_variables)
  out <- LE_variables[dep_variables %in% vSelect]
  names(out) <- NULL
  out
}

