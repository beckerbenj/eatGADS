
#' @title List of program specific limits
#'
#' @description
#' A list of two matrices, detailing different limits for data sets specific to \code{SPSS} and
#'  \code{Stata}.
#'
#' @details
#' \code{SPSS} and \code{Stata} impose different limits to different aspects of their data sets:
#' \itemize{
#'  \item \code{varNames}: length of variable names
#'  \item \code{varLabels}: length of variable labels
#'  \item \code{valLabels}: length of value labels
#'  \item \code{stringvars}: length of strings in character variables
#'  \item \code{nrows}: number of observations
#'  \item \code{ncols}: number of variables
#' }
#'
#' While \code{SPSS} has only one set of limits (disregarding legacy limits for older versions),
#'  \code{Stata}, employs different limits for different product versions [1]. Here, \code{SPSS}
#'  implies \code{SPSS 30}, and \code{Stata} implies \code{Stata 19/SE}. Limits of
#'  \code{Stata 19/BE} and \code{Stata 19/MP} are implemented as additional options for \link{getProgramLimit}.
#'
#' @references [1] \href{https://www.stata.com/products/comparison-of-limits/}{Stata: Comparison of limits}
#'
#' @format A list of two matrices: \code{x} (numeric size of the limit) and
#'  \code{unit} ("char", "byte", or "generic") of each limit under each program (version).
"program_limits"
