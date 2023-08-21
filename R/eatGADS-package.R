#' eatGADS: Data management of hierarchical SPSS files via R and SQLite
#'
#' The \code{eatGADS} package provides various functionality, mainly:
#' importing data,
#' data and meta data cleaning,
#' creating a fixed form \code{SQLite} data base
#' and using the \code{SQLite} data base.
#'
#' @section Importing data:
#' \code{SPSS} data (\code{.sav}) can be imported via \code{\link{import_spss}}. Further import functions exist as well:
#' \code{\link{import_stata}} for importing \code{Stata} data (\code{.dta}),
#' \code{\link{import_DF}} for importing \code{R} \code{data.frames},
#' \code{\link{import_RDS}} for importing \code{R} \code{data.frames} saved as \code{.RDS} files,
#' and \code{\link{import_raw}} as well as \code{\link{import_raw2}} for importing data from raw data and meta data files.
#'
#' @section Data and meta data cleaning:
#' Data cleaning functions include functions for recoding data (e.g., \code{\link{recodeGADS}})
#' or re-ordering variables (e.g., \code{\link{relocateVariable}}).
#' Meta data cleaning functions include functions for changing variables labels (e.g., \code{\link{changeVarLabels}}),
#' changing value labels \code{\link{changeValLabels}} or modifying missings tags \code{\link{changeMissings}}.
#'
#' @section Creating a GADS data base:
#' Hierarchical data sets are combined via \code{\link{mergeLabels}} and the data base is created via \code{\link{createGADS}}. For this, the package \code{eatDB} is utilized. See also \code{\link[eatDB]{createDB}}.
#'
#' @section Using the GADS:
#' The content of a data base can be obtained via \code{\link{namesGADS}}. Data is extracted from the data base via \code{\link{getGADS}} for a single GADS and via \code{\link{getTrendGADS}} for trend analysis. The resulting object is a \code{GADSdat} object. Meta data can be extracted via \code{\link{extractMeta}}, either from the \code{GADSdat} object or directly from the data base. Data can be extracted from the \code{GADSdat} object via \code{\link{extractData}}.

#' @keywords internal
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL

## quiets concerns of R CMD check regarding NSE by data.table
utils::globalVariables(c("i.value_new"))
