
# create S3 object GADSdat for User (needs interface!)
new_GADSdat <- function(dat, labels) {
  stopifnot(is.data.frame(dat) && is.data.frame(labels))
  structure(list(dat = dat, labels = labels), class = c("GADSdat", "list"))
}
# GADSdat validator (allow data_table column for trend compatability)
check_GADSdat <- function(GADSdat) {
  if(!"GADSdat" %in% class(GADSdat)) stop("All input objects have to be of class GADSdat", call. = FALSE)
  if(!is.list(GADSdat) && length(GADSdat) == 2) stop("GADSdat has to be a list with length two", call. = FALSE)
  if(!identical(names(GADSdat), c("dat", "labels"))) stop("List elements of a GADSdat object have to be 'dat' and 'labels'", call. = FALSE)
  if(!is.data.frame(GADSdat$dat)) stop("dat element has to be a data frame", call. = FALSE)
  if(tibble::is_tibble(GADSdat$dat)) stop("dat element has to be a data frame and can not be a tibble.", call. = FALSE)
  if(!is.data.frame(GADSdat$labels)) stop("labels element has to be a data frame", call. = FALSE)
  if(!(identical(names(GADSdat$labels), c("varName", "varLabel", "format", "display_width", "labeled", "value", "valLabel", "missings")) ||
       identical(names(GADSdat$labels), c("varName", "varLabel", "format", "display_width", "labeled", "value", "valLabel", "missings", "data_table")))) {
    stop("Illegal column names in labels data frame.")
  }

  # internals
  only_in_labels <- setdiff(unique(GADSdat$labels$varName), names(GADSdat$dat))
  only_in_dat <- setdiff(names(GADSdat$dat), unique(GADSdat$labels$varName))
  if(length(only_in_labels) > 0) stop("The following variables have meta data but are not in the actual data: ", only_in_labels, call. = FALSE)
  if(length(only_in_dat) > 0) stop("The following variables are in the data but do not have meta data: ", only_in_dat, call. = FALSE)

  if(!is.numeric(GADSdat$labels$value)) stop("Column 'value' in the meta data is not numeric.")

  unlabeled_labels <- GADSdat$labels[GADSdat$labels$labeled == "no", ]
  if(nrow(unlabeled_labels) > 0) {
    by(unlabeled_labels, unlabeled_labels$varName, function(labels) {
      if(any(!is.na(labels$value))) stop("The following variable has value labels but is not marked as labeled: ", unique(labels$varName))
      if(any(!is.na(labels$valLabel))) stop("The following variable has value labels but is not marked as labeled: ", unique(labels$varName))
    })
  }

  var_info <- unique(GADSdat$labels[, c("varName", "varLabel", "format", "display_width", "labeled")])
  if(nrow(var_info) != length(unique(names(GADSdat$dat)))) {
    by(GADSdat$labels, GADSdat$labels$varName, function(labels) {
      if(nrow(unique(labels[, c("varName", "varLabel", "format", "display_width", "labeled")])) > 1) {
        stop("The following variable has inconsistent meta information on variable level: ", unique(labels$varName))
      }
    })
  }

  # maybe later remove this test due to performance?
  by(GADSdat$labels, GADSdat$labels$varName, function(labels) {
    if(any(duplicated(labels$value[!is.na(labels$value)]))) {
      stop("The following variable has duplicate values rows in its meta data: ", unique(labels$varName))
    }
  })
}
