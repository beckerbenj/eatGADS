

#### Get data from Gads fast
#############################################################################
#' Get data from GADS data base fast from server directory.
#'
#' Extracts variables from a \code{eatGADS} data base. Only the specified variables are extracted. Note that this selection determines the format
#' of the \code{data.frame} that is extracted. CAREFUL: This function uses a local temporary directory to speed up loading the data base
#' from a server and caches the data base locally for a running R session. The temporary data base is removed automatically when the
#' running \code{R} session is terminated.
#'
#' A random temporary directory is used for caching the data base and is removed, when the \code{R} sessions terminates. See
#' \code{\link[eatDB]{createDB}} and \code{\link[eatDB]{dbPull}} for further explanation of the query and merging processes.
#'
#'@param vSelect Character vector of variable names.
#'@param filePath Path of the existing \code{eatGADS} data base file.
#'@param tempPath Local directory in which the data base can temporarily be stored. Using the default is recommended.
#'
#'@return Returns a \code{GADSdat} object.
#'
#'
#'@export
getGADS_fast <- function(vSelect = NULL, filePath, tempPath = tempdir()) {
  # checks for tempPath
  if(!is.character(tempPath) || length(tempPath) != 1) stop("tempPath is not a character vector of length 1.")
  if(!file.exists(tempPath)) stop("tempPath is not an existing directory.")
  if(file.access(tempPath, mode = 2) != 0) stop("User has no writing permission for tempPath.")
  if(!file.exists(filePath)) stop(filePath, " is not a valid path to a data base")

  fileName <- basename(filePath)
  tempFile <- file.path(tempPath, fileName)
  # if (length(vSelect) >10)browser()
  # create copy
  if(!file.exists(tempFile)) {
    cat("Copy file to local directory...\n")
    if(file.exists(tempFile)) stop(tempFile, "is an existing file and can not be used as local copy.")
    file.copy(from = filePath, to = tempFile, overwrite = FALSE, recursive = FALSE)

    ## remove temporary cache if R sessions ends or eatGADS is detached (data security reasons)
    ## note: most of the time unnecessary, as tempdir is deleted anyway, after R session ends
    auto_remove_cache <- function(x) {
      message("File ", tempFile, " was removed.")
      file.remove(tempFile)
    }
    reg.finalizer(e = as.environment("package:eatGADS"), f = auto_remove_cache, onexit = TRUE)

  } else cat("Using cached data base...\n")


  #
  cat("Pull data from GADS db...\n")
  GADSdat <- getGADS(vSelect = vSelect, filePath = tempFile)
  GADSdat
}





