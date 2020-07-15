#### Get Labels from Gads
#############################################################################
#' Labels from relational \code{eatGADS} data base.
#'
#' Returns the variable and value labels of all variables in the \code{eatGADS} data base.
#'
#' Variable, value and missing labels as stored in the original SPSS-files and factors from R files are converted to long format for
#' storage in the data base. \code{labelsGADS} returns them as a long format data frame.
#'
#'
#'@param filePath Path of the existing \code{eatGADS} data base.
#'
#'@return Returns a long format data frame including variable names, labels, values, value labels and missing labels.
#'
#'@examples
#'\dontrun{
#'metaData <- labelsGADS("t:/_R_Tutorials/R_Workshops/04_eatPakete/minigads_2010.db")
#'View(metaData)
#'}
#'
#'@export
labelsGADS <- function(filePath) {
  eatDB::dbSingleDF(dfName = "Meta_data", filePath = filePath)
}
