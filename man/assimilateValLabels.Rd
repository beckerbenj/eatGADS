% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/assimilateValLabels.R
\name{assimilateValLabels}
\alias{assimilateValLabels}
\title{Assimilate value labels.}
\usage{
assimilateValLabels(GADSdat, varNames, lookup = NULL)
}
\arguments{
\item{GADSdat}{\code{GADSdat} object imported via \code{eatGADS}.}

\item{varNames}{Character string of a variable name.}

\item{lookup}{Lookup \code{data.frame}.}
}
\value{
Returns the \code{GADSdat} object with changed meta data and recoded values.
}
\description{
Assimilate all value labels of multiple variables as part of a \code{GADSdat} or \code{all_GADSdat} object.
}
\details{
Assimilation can be performed using all existing value labels or a lookup table containing at least all existing value labels.
Missing codes are reused based on the meta data of the first variable in \code{varNames}.
}
\examples{
# Example data set
facs_df <- data.frame(id = 1:3, fac1 = c("Eng", "Aus", "Ger"),
                      fac2 = c("Ger", "Franz", "Ita"),
                      fac3 = c("Kor", "Chi", "Alg"),
                      stringsAsFactors = TRUE)
facs_gads <- import_DF(facs_df)

assimilateValLabels(facs_gads, varNames = paste0("fac", 1:3))

}
