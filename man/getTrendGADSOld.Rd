% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getTrendGADSOld.R
\name{getTrendGADSOld}
\alias{getTrendGADSOld}
\title{Get data for trend reports.}
\usage{
getTrendGADSOld(
  filePath1,
  filePath2,
  lePath = NULL,
  vSelect = NULL,
  years,
  fast = TRUE,
  tempPath = tempdir()
)
}
\arguments{
\item{filePath1}{Path of the first \code{eatGADS} db file.}

\item{filePath2}{Path of the second \code{eatGADS} db file.}

\item{lePath}{Path of the linking error db file. If \code{NULL}, no linking errors are added to the data.}

\item{vSelect}{Variables from both GADS to be selected (as character vector).}

\item{years}{A numeric vector of length 2. The first elements corresponds to \code{filePath1}, the second element to \code{filePath2}.}

\item{fast}{Should \code{\link{getGADS_fast}} be used for data loading instead of \code{\link{getGADS}}? Using the default is heavily recommended.}

\item{tempPath}{The directory, in which both GADS will be temporarily stored. Using the default is heavily recommended.}
}
\value{
Returns a \code{GADSdat} object.
}
\description{
Support for linking error data bases has been removed from \code{eatGADS}.
\code{getGADSold} provides (for the time being) backwards compatibility, so linking errors can still be extracted automatically.
}
\details{
See \code{\link{getGADS}} for the current functionality.
}
\examples{
# See getGADS vignette


}
