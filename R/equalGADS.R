
#############################################################################
#' Test if Two \code{GADSdat} Objects are (Nearly) Equal
#'
#' Run tests to check whether two \code{GADSdat} objects are (nearly) equal. Variable names, number of rows in the data,
#' meta data and data differences are checked and reported as a list output.
#'
#'@param target A \code{GADSdat} object.
#'@param current A \code{GADSdat} object.
#'
#'@return Returns a list.
#'
#'
#'@export
equalGADS <- function(target, current) {
  UseMethod("equalGADS")
}
#'@export
equalGADS.GADSdat <- function(target, current) {
  check_GADSdat(target)
  check_GADSdat(current)

  out <- list()

  #browser()
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
  for(i in intersect(target_names, current_names)) {
    #browser()
    target_single_labs <- target_labs[target_labs$varName == i, ]
    current_single_labs <- current_labs[current_labs$varName == i, ]
    rownames(target_single_labs) <- rownames(current_single_labs) <- NULL
    if(!identical(all.equal(target_single_labs, current_single_labs), TRUE)) {
      out[["meta_data_differences"]] <- c(out[["meta_data_differences"]], i)
    }
    if(!identical(all.equal(target$dat[[i]], current$dat[[i]], scale = 1), TRUE)) {
      out[["data_differences"]] <- c(out[["data_differences"]], i)
    }
  }
  out
}
