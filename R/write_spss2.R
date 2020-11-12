
#### Writing sav files via SPSS syntax
#############################################################################
#' Write a \code{GADSdat} object to \code{txt} and \code{SPSS} syntax
#'
#' Write a \code{GADSdat} object to a text file (\code{txt}) and an accompanying \code{SPSS} syntax file containing all meta information (e.g. value and variable labels).
#'
#' This function is based on \code{eatPreps} \code{writeSpss} function and is currently under development.
#'
#'@param GADSdat A \code{GADSdat} object.
#'@param filePath Path of \code{.txt} file to write.
#'@param syntaxPath Path of \code{.sps} file to write.
#'
#'@return Writes \code{sav} file to disc, returns \code{NULL}.
#'
#'
#'@export
write_spss2 <- function(GADSdat, filePath, syntaxPath) {
  UseMethod("write_spss2")
}

#'@export
write_spss2.GADSdat <- function(GADSdat, filePath, syntaxPath) {

  ## write txt
  utils::write.table(GADSdat$dat, file = filePath, row.names = FALSE, col.names = FALSE,
              sep = "\t", dec = ".", quote = FALSE, na = "", eol = "\n")

  ##### write SPSS syntax
  labels <- GADSdat$labels
  varInfo <- unique(labels[, c("varName", "varLabel", "format")])
  valInfo <- unique(labels[!is.na(labels$value), c("varName", "value", "valLabel", "missings")])

  #browser()
  # write header
  freefield <- " free (TAB)\n"
  cat("DATA LIST FILE=", autoQuote(filePath), freefield, file = syntaxPath)
  cat(" /", varInfo$varName, ".\n\n", file = syntaxPath, append = TRUE,
      fill = 60, labels = " ")

  # write variable labels
  cat("VARIABLE LABELS\n", file = syntaxPath, append = TRUE)
  cat(" ", paste(varInfo$varName, autoQuote(varInfo$varLabel), "\n"), ".\n",
      file = syntaxPath, append = TRUE)

  # write value labels
  if (nrow(valInfo) > 0) {

    cat("\nVALUE LABELS\n", file = syntaxPath, append = TRUE)

    for (v in unique(valInfo$varName)) {
      cat(" /", v, "\n", file = syntaxPath, append = TRUE)
      #if (any(nchar(valueLabels) > 120L)) {
      #  cat(paste(funVersion, "Value labels for variable", v , "longer than 120 characters. Only the first 120 characters will be used.\n"))
       # valueLabels <- substring(valueLabels, 1, 120)
      #}
      cat(paste("  ", valInfo$value, autoQuote(valInfo$valLabel),"\n",  sep = " "),
          file = syntaxPath, append = TRUE)
    }
    cat(" .\n", file = syntaxPath, append = TRUE)
  }

  # write missing codes
  # tbd

  cat("\nEXECUTE.\n", file = syntaxPath, append = TRUE)

  return()
}

autoQuote <- function (x){
  paste("\"", x, "\"", sep = "")
}
