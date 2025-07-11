% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/autoRecode.R
\name{autoRecode}
\alias{autoRecode}
\title{Auto recode a variable in a \code{GADSdat}.}
\usage{
autoRecode(
  GADSdat,
  var,
  var_suffix = "",
  label_suffix = "",
  csv_path = NULL,
  template = NULL
)
}
\arguments{
\item{GADSdat}{A \code{GADSdat} object.}

\item{var}{Character string of the variable name which should be recoded.}

\item{var_suffix}{Variable suffix for the newly created \code{GADSdat}. If an empty character, the existing variables are overwritten.}

\item{label_suffix}{Suffix added to variable label for the newly created variable in the \code{GADSdat}.}

\item{csv_path}{Path for the \code{.csv} file for the look up table.}

\item{template}{Existing look up table.}
}
\value{
Returns a \code{GADSdat} object.
}
\description{
Auto recode a variable in a \code{GADSdat}. A look up table is created containing the respective recode pairs.
An existing look up table can be utilized via \code{template}. This function somewhat mirrors the functionality provided
by the \code{SPSS} function \code{autorecode}.
}
\details{
If an existing \code{template} is used and a look up table is saved as a \code{.csv} file, the resulting look up
table will contain the existing recodes plus additional recode pairs required for the data.
}
\examples{
gads <- import_DF(data.frame(v1 = letters))

# auto recode without saving look up table
gads2 <- autoRecode(gads, var = "v1", var_suffix = "_num")

# auto recode with saving look up table
f <- tempfile(fileext = ".csv")
gads2 <- autoRecode(gads, var = "v1", var_suffix = "_num", csv_path = f)

# auto recode with applying and expanding a look up table
gads3 <- import_DF(data.frame(v2 = c(letters[1:3], "aa")))
gads3 <- autoRecode(gads3, var = "v2", csv_path = f, template = read.csv(f))
}
