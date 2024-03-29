% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/updateMeta.R
\name{updateMeta}
\alias{updateMeta}
\title{Update meta data.}
\usage{
updateMeta(GADSdat, newDat, checkVarNames = TRUE)
}
\arguments{
\item{GADSdat}{\code{GADSdat} or \code{all_GADSdat} object.}

\item{newDat}{\code{data.frame} or list of \code{data.frames} with the modified data. \code{tibbles} and \code{data.tables}
are currently not supported and need to be transformed to \code{data.frames} beforehand.}

\item{checkVarNames}{Logical. Should new variable names be checked by \code{\link{checkVarNames}}?}
}
\value{
Returns the original object with updated meta data (and removes factors from the data).
}
\description{
Update the meta data of a \code{GADSdat} or \code{all_GADSdat} object according to the variables in a new data object.
}
\details{
If the data of a \code{GADSdat} or a \code{all_GADSdat} has changed (supplied via \code{newDat}), \code{updateMeta}
assimilates the corresponding meta data set. If variables have been removed, the corresponding meta data is also removed.
If variables have been added, empty meta data is added for these variables. Factors are transformed to numerical
and their levels added to the meta data set.
}
\examples{
# see createGADS vignette

}
