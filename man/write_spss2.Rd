% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/write_spss2.R
\name{write_spss2}
\alias{write_spss2}
\title{Write a \code{GADSdat} object to \code{txt} and \code{SPSS} syntax}
\usage{
write_spss2(
  GADSdat,
  filePath,
  syntaxPath,
  dec = ".",
  changeMeta = FALSE,
  fileEncoding = "UTF-8"
)
}
\arguments{
\item{GADSdat}{A \code{GADSdat} object.}

\item{filePath}{Path of \code{.txt} file to write.}

\item{syntaxPath}{Path of \code{.sps} file to write.}

\item{dec}{Decimal delimiter for your SPSS version.}

\item{changeMeta}{Meta data information will be changed automatically according to features of the data.}

\item{fileEncoding}{Data file encoding for SPSS. Default is "UTF-8".}
}
\value{
Writes a \code{txt} and an \code{sav} file to disc, returns nothing.
}
\description{
Write a \code{GADSdat} object to a text file (\code{txt}) and an accompanying \code{SPSS} syntax file containing all meta information (e.g. value and variable labels).
}
\details{
This function is based on \code{eatPreps} \code{writeSpss} function and is currently under development.
}
\examples{

# write to spss
tmp_sps <- tempfile(fileext = ".sps")
tmp_txt <- tempfile(fileext = ".txt")
write_spss2(pisa, filePath = tmp_txt, syntaxPath = tmp_sps)

}
