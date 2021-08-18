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
#'@param dec Decimal delimiter for your SPSS version.
#'@param fileEncoding Data file encoding for SPSS. Default is "UTF-8".
#'@param ... Arguments to pass to \code{checkFormat}
#'
#'@return Writes a \code{txt} and an \code{sav} file to disc, returns nothing.
#'
#'@examples
#'
#'# write to spss
#'tmp_sps <- tempfile(fileext = ".sps")
#'tmp_txt <- tempfile(fileext = ".txt")
#'write_spss2(pisa, filePath = tmp_txt, syntaxPath = tmp_sps)
#'
#'@export
write_spss2 <- function(GADSdat, filePath, syntaxPath, dec = ".", fileEncoding = "UTF-8", ...) {
  UseMethod("write_spss2")
}

#'@export
write_spss2.GADSdat <- function(GADSdat, filePath, syntaxPath, dec =".", fileEncoding = "UTF-8", ...) {

  ## Checks
  check_GADSdat(GADSdat)
  check_GADSdat_varLevel_meta(GADSdat)
  checkz <- check4SPSS(GADSdat)

  if(length(checkz$varNames_special) > 0) stop("Please remove special characters in variable names: ", paste(checkz$varNames_special, collapse=" "))
  if(length(checkz$varNames_length) > 0) stop("Please shorten variable names to < 64 byte: ", paste(checkz$varNames_length, collapse=" "))
  if(length(checkz$varLabels) > 0) stop("Please shorten variable labels to < 256 byte: ", paste(checkz$varLabels, collapse=" "))
  if(length(checkz$valLabels) > 0) stop("Please shorten value labels to < 120 byte: ", paste(names(checkz$valLabels), collapse=" "))
  if(length(checkz$missings) > 0) message("Too many missing values for character variables: ", paste(checkz$missings, collapse= " "),". SPSS allows only three missing values for character variables. I will take the first 3.")

  # GADSdat <- checkMissings(GADSdat)
  GADSdat <- checkFormat(GADSdat, ...)

  ## additional Column for SPSS, which is sometimes inadvertently shifts cases that end with NAs
  axxx <- any(is.na(GADSdat$dat))

  ## write data
  GADSdat <- writeData(GADSdat=GADSdat, filePath=filePath, dec=dec, fileEncoding=fileEncoding)

  ## meta info partitioning
  r1 <- list()
  r1$labels <- GADSdat$labels
  stopifnot(identical(unique(r1$labels$varName),names(GADSdat$dat)))
  r1$varInfo <- unique(r1$labels[, c("varName", "varLabel", "format")])
  r1$valInfo <- unique(r1$labels[which(!is.na(r1$labels$value)), c("varName", "value", "valLabel", "missings")])
  r1$misInfo <- unique(r1$labels[which(!is.na(r1$labels$value) & r1$labels$missings == "miss"), c("varName", "value", "valLabel", "missings")])

  ## write header
  writeHeader(r1=r1, filePath=filePath, syntaxPath=syntaxPath)

  ## write variable labels
  writeVaLab(r1=r1, syntaxPath=syntaxPath)

  ## write missing codes
  writeMisCode(r1=r1, syntaxPath=syntaxPath)

  ## delete additional variable
  cat("\nEXECUTE.\n", file = syntaxPath, append = TRUE)
  if(isTRUE(axxx)) {
    cat("DELETE VARIABLES xxxtgw.\nEXECUTE.\n", file = syntaxPath, append = TRUE)
  }

  # Save
  finPath <- gsub(".sps$", ".sav", syntaxPath)
  cat("\nSAVE OUTFILE = ", autoQuote(finPath), ".", file = syntaxPath, append = TRUE)

}

autoQuote <- function (x){
  paste("\"", x, "\"", sep = "")
}


writeData <- function(GADSdat, filePath, dec, fileEncoding) {
  if(any(is.na(GADSdat$dat))) {
    GADSdat$dat <- cbind(GADSdat$dat,data.frame(xxxtgw=rep(1,nrow(GADSdat$dat))))
    GADSdat$labels <- rbind(GADSdat$labels, data.frame(varName="xxxtgw",varLabel=NA,format="F1",display_width=NaN,labeled="no",value=NaN,valLabel=NA,missings=NA,stringsAsFactors = FALSE))
  }

  ## write txt
  utils::write.table(GADSdat$dat, file = filePath, row.names = FALSE, col.names = FALSE,
                     sep = "]&;", dec = dec, quote = FALSE, na = "", eol = "\n", fileEncoding = fileEncoding)

  return(GADSdat)
}

writeHeader <- function(r1, filePath, syntaxPath) {
    freefield <- " free (']&;')\n"
    cat("DATA LIST FILE=", autoQuote(filePath), freefield, file = syntaxPath)
    labs1 <- r1$labels[!duplicated(r1$labels$varName),]
    dl.varnames <- paste0(labs1$varName, " (", labs1$format, ")")
    cat(" /", dl.varnames, ".\n\n", file = syntaxPath, append = TRUE,
        fill = 60, labels = " ")
}

writeVaLab <- function(r1, syntaxPath) {
  r1$varInfo$varLabel[is.na(r1$varInfo$varLabel)] <- ""
  cat("VARIABLE LABELS\n", file = syntaxPath, append = TRUE)
  cat(" ", paste(r1$varInfo$varName, autoQuote(r1$varInfo$varLabel), "\n"), ".\n",
      file = syntaxPath, append = TRUE)

  # write value labels
  if (nrow(r1$valInfo) > 0) {
    cat("\nVALUE LABELS\n", file = syntaxPath, append = TRUE)
    for (v in unique(r1$valInfo$varName)) {
      cat(" /", v, "\n", file = syntaxPath, append = TRUE)
      cat(paste("  ", r1$valInfo[r1$valInfo$varName==v,]$value, autoQuote(r1$valInfo[r1$valInfo$varName==v,]$valLabel),"\n",  sep = " "),
          file = syntaxPath, append = TRUE)
    }
    cat(" .\n", file = syntaxPath, append = TRUE)
  }
}

writeMisCode <- function(r1, syntaxPath) {

  if(nrow(r1$misInfo) > 0) {

    cat("\nMISSING VALUES\n", file = syntaxPath, append = TRUE)

    for (v in unique(r1$misInfo$varName)) {
      if(length(r1$misInfo$value[r1$misInfo$varName==v]) > 3) {
        if(isTRUE(r1$chv[v])) {
         # message(paste0("Too many missing values for character variable \'", v,"\'. SPSS allows only three missing values for character variables. I will take the first 3."))
          span <- paste(r1$misInfo$value[r1$misInfo$varName==v][1:3],collapse="\', \'")
          cat(paste0(v, " ('",  span, "')\n",  sep = " "),
              file = syntaxPath, append = TRUE)
        } else {
          span <- paste(min(as.numeric(r1$misInfo$value[r1$misInfo$varName==v])), "THRU", max(as.numeric(r1$misInfo$value[r1$misInfo$varName==v])))
          cat(paste0(v, " (",  span, ")\n",  sep = " "),
              file = syntaxPath, append = TRUE)
        }
      } else {
        if(isTRUE(r1$chv[v])) {
          span <- paste(r1$misInfo$value[r1$misInfo$varName==v],collapse="\',\'")
          cat(paste0(v, " ('",  span, "')\n",  sep = " "),
              file = syntaxPath, append = TRUE)
        } else {
          span <- paste(r1$misInfo$value[r1$misInfo$varName==v],collapse=",")
          cat(paste0(v, " (",  span, ")\n",  sep = " "),
              file = syntaxPath, append = TRUE)
        }
      }
    }
    cat(".\n", file = syntaxPath, append = TRUE)

  }
}


