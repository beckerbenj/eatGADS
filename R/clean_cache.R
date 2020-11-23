
#### Clean cache
#############################################################################
#' Clean temporary cache.
#'
#' Deprecated. The cached data base is now cleaned when the R sessions ends automatically.
#'
#' Cleans the temporary cache, specified by \code{tempdir()}. This function had to be executed at the end of an \code{R} session if
#' \code{\link{getGADS_fast}} or \code{\link{getTrendGADS}} with \code{fast = TRUE} had been used.
#'
#'@param tempPath Local directory in which the data base was temporarily be stored.
#'
#'@return Returns nothing.
#'
#'@export
clean_cache <- function(tempPath = tempdir()) {
  cat("Scanning temporary directory:\n", tempdir(), "\n")
  cont <- list.files(path = tempPath, full.names = TRUE)
  nam <- list.files(path = tempPath)
  cat("The following files are in the directory:\n")
  print(nam)
  answ <- readline("Should all these files be deleted? y/n: ")
  if(identical(answ, "y")) {
    cat("Cleaning temporary directory... \n")
    unlink2 <- function(x) unlink(x, recursive = TRUE)
    do.call(unlink2, list(cont))
    message("All files deleted.")
  } else {
    message("No files deleted.")
  }
  return()
}


