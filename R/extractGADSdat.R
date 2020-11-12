#### Extract GADSdat from all_GADSdat object (especially useful for Meta changes)
#############################################################################
#' Extract single \code{GADSdat} from \code{all_GADSdat}
#'
#' Function to extract a single \code{GADSdat} from an \code{all_GADSdat} object.
#'
#' \code{GADSdat} objects can be merged into a single \code{all_GADSdat} object via \code{\link{mergeLabels}}. This function, performs the
#' reverse action, extracting a single \code{GADSdat} object.
#'
#'@param all_GADSdat \code{all_GADSdat} object
#'@param name A character vector with length 1 with the name of the \code{GADSdat}
#'
#'@return Returns an \code{GADSdat} object.
#'
#'@examples
#'# see createGADS vignette
#'
#'@export
extractGADSdat <- function(all_GADSdat, name) {
  UseMethod("extractGADSdat")
}
#'@export
extractGADSdat.all_GADSdat <- function(all_GADSdat, name) {
  check_all_GADSdat(all_GADSdat, GADSdatChecks = FALSE)
  if(!is.character(name) || !length(name) == 1) stop("name has to be a character vector of length 1.")
  if(!name %in% names(all_GADSdat[["datList"]])) stop("name has to be the name of a GADSdat element of all_GADSdat.")

  extracted_meta <- all_GADSdat[["allLabels"]][all_GADSdat[["allLabels"]]$data_table == name, ]
  extracted_meta <- extracted_meta[, names(extracted_meta) != "data_table"]
  rownames(extracted_meta) <- NULL
  out_GADSdat <- new_GADSdat(dat = all_GADSdat[["datList"]][[name]], labels = extracted_meta)
  check_GADSdat(out_GADSdat)

  out_GADSdat
}
