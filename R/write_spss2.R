#### Writing sav files via SPSS syntax
#############################################################################
#' Write a \code{GADSdat} object to \code{txt} and \code{SPSS} syntax
#'
#' Write a \code{GADSdat} object to a text file (\code{txt}) and an accompanying \code{SPSS} syntax file containing all meta information (e.g. value and variable labels).
#'
#' This function is based on \code{eatPreps} \code{writeSpss} function and is currently under development.
#'
#'@param GADSdat A \code{GADSdat} object.
#'@param txtPath Path of \code{.txt} file to write, including file name and ending \code{.txt}. No default.
#'@param spsPath Path of \code{.sps} file to write, including file name and ending \code{.sps}. Default Path is \code{txtPath}.
#'@param savPath Path of \code{.sav} file to write, including file name and ending \code{.sav}. Default Path is \code{spsPath}.
#'@param dec Decimal delimiter for your SPSS version. Other values for \code{dec} than \code{","} or \code{"."} are not implemented yet.
#'@param fileEncoding Data file encoding for SPSS. Default is \code{"UTF-8"}.
#'@param chkFormat Whether format checks via \code{checkFormat} should be performed.
#'@param ... Arguments to pass to \code{checkFormat} in particular \code{changeFormat=FALSE} if needed.
#'
#'@return Writes a \code{txt} and an \code{sav} file to disc, returns nothing.
#'
#'@examples
#'
#'# write to spss
#'tmp_txt <- tempfile(fileext = ".txt")
#'write_spss2(pisa, txtPath = tmp_txt)
#'
#'@export
write_spss2 <- function(GADSdat, txtPath, spsPath = NULL, savPath = NULL, dec = ".", fileEncoding = "UTF-8", chkFormat=TRUE, ...) {
  UseMethod("write_spss2")
}

#'@export
write_spss2.GADSdat <- function(GADSdat, txtPath, spsPath = NULL, savPath = NULL, dec =".", fileEncoding = "UTF-8", chkFormat=TRUE, ...) {

  if(is.null(spsPath)) spsPath <- gsub(".txt$", ".sps", txtPath)
  if(is.null(savPath)) savPath <- gsub(".sps$", ".sav", spsPath)

  ## Checks
  check_GADSdat(GADSdat)
  check_GADSdat_varLevel_meta(GADSdat)
  checkz <- check4SPSS(GADSdat)

  if(length(checkz$varNames_special) > 0) stop("Please remove special characters in variable names: ", paste(checkz$varNames_special, collapse=" "))
  if(length(checkz$varNames_length) > 0) stop("Please shorten variable names to < 65 byte: ", paste(checkz$varNames_length, collapse=" "))
  if(length(checkz$varLabels) > 0) stop("Please shorten variable labels to < 257 byte: ", paste(checkz$varLabels, collapse=" "))
  if(length(checkz$valLabels) > 0) stop("Please shorten value labels to < 121 byte: ", paste(names(checkz$valLabels), collapse=" "))
  if(length(checkz$missings) > 0) message("Too many missing values for character variables: ", paste(checkz$missings, collapse= " "),". SPSS allows only three missing values for character variables. I will take the first 3.")

  # GADSdat <- checkMissings(GADSdat)
  if(isTRUE(chkFormat)) GADSdat <- checkFormat(GADSdat, ...)

  ## write data
  GADSdat <- writeData(GADSdat=GADSdat, txtPath=txtPath, dec=dec, fileEncoding=fileEncoding)

  ## meta info partitioning
  r1 <- list()
  r1$labels <- GADSdat$labels
  stopifnot(identical(unique(r1$labels$varName),names(GADSdat$dat)))
  r1$varInfo <- unique(r1$labels[, c("varName", "varLabel", "format")])
  r1$valInfo <- unique(r1$labels[which(!is.na(r1$labels$value)), c("varName", "value", "valLabel", "missings")])
  r1$misInfo <- unique(r1$labels[which(!is.na(r1$labels$value) & r1$labels$missings == "miss"), c("varName", "value", "valLabel", "missings")])
  r1$chv <- sapply(GADSdat$dat, is.character)

  ## write header
  writeHeader(r1=r1, txtPath=txtPath, spsPath=spsPath, fileEncoding=fileEncoding, dec=dec)

  ## write variable labels
  writeVaLab(r1=r1, spsPath=spsPath)

  ## write missing codes
  writeMisCode(r1=r1, spsPath=spsPath)

  ## execute
  cat("\nEXECUTE.\n", file = spsPath, append = TRUE)

  # Save
  cat("\nSAVE OUTFILE = ", autoQuote(savPath), ".", file = spsPath, append = TRUE)

}

autoQuote <- function (x){
  paste("\"", x, "\"", sep = "")
}


writeData <- function(GADSdat, txtPath, dec, fileEncoding) {

  # remove text qualifier in string
  chv1 <- which(sapply(GADSdat$dat, is.character))
  if(length(chv1) > 0) {
    chv2 <- sapply(GADSdat$dat[chv1], function(ij) any(grepl("\"",ij)))
    GADSdat$dat[,chv1] <- apply(GADSdat$dat[,chv1],2,function(pq) gsub("\"", "'", pq))
    message("In character variable(s) ", paste(names(chv2)[chv2], collapse=", "), " quotation marks (\") had to be replaced with inverted commas (').")
  }
  ## write txt
  utils::write.table(GADSdat$dat, file = txtPath, row.names = FALSE, col.names = FALSE,
                     sep = ";", dec = dec, quote = TRUE, na = "", eol = "\n", fileEncoding = fileEncoding)

  return(GADSdat)
}



writeHeader <- function(r1, txtPath, spsPath, fileEncoding, dec) {
  if(dec==",") {
    decstr <- "COMMA"
  } else {
    if(dec==".") {
      decstr <- "DOT"
    } else {
     stop("Other values for dec than COMMA oder DOT are not implemented yet.")
    }
  }
  txtPath <- gsub("^[a-z]",toupper(substr(txtPath,1,1)),txtPath)
  partI <- paste0("* Encoding: ", fileEncoding, ".\n\nPRESERVE.\n SET DECIMAL ", decstr, ".\n\nGET DATA  /TYPE=TXT\n  /FILE=", autoQuote(txtPath), "\n  /DELCASE=LINE\n  /DELIMITERS=\";\"\n  /QUALIFIER=\'\"\'\n  /ARRANGEMENT=DELIMITED\n  /FIRSTCASE=1\n  /DATATYPEMIN PERCENTAGE=95.0\n  /VARIABLES=")
  cat(partI, file = spsPath)
  labs1 <- r1$labels[!duplicated(r1$labels$varName),]
  labs1$format <- gsub("\\.0$", "", labs1$format)
  if(any(is.na(labs1$format))) warning("Format statement still contains 'NA' values, SPSS syntax will probably not work. Consider changeFormat=TRUE.")
  dl.varnames <- paste0(labs1$varName, " ", labs1$format, "\n")
  cat("\n ",dl.varnames, "  /MAP.\nRESTORE.\n\nCACHE.\nEXECUTE.\nDATASET NAME DataSet1 WINDOW=FRONT.\n\n", file = spsPath, append = TRUE, sep="")
}


writeHeaderOld <- function(r1, txtPath, spsPath) {
    # This function is deprecated, because SPSS sometimes in large data sets in SPSS some cases were slipping out of place
    freefield <- " free (';')\n"
    cat("DATA LIST FILE=", autoQuote(txtPath), freefield, file = spsPath)
    labs1 <- r1$labels[!duplicated(r1$labels$varName),]
    dl.varnames <- paste0(labs1$varName, " (", labs1$format, ")")
    cat(" /", dl.varnames, ".\n\n", file = spsPath, append = TRUE,
        fill = 60, labels = " ")
}

writeVaLab <- function(r1, spsPath) {
  r1$varInfo$varLabel[is.na(r1$varInfo$varLabel)] <- ""
  cat("VARIABLE LABELS\n", file = spsPath, append = TRUE)
  cat(" ", paste(r1$varInfo$varName, autoQuote(r1$varInfo$varLabel), "\n"), ".\n",
      file = spsPath, append = TRUE)

  # write value labels
  if (nrow(r1$valInfo) > 0) {
    cat("\nVALUE LABELS\n", file = spsPath, append = TRUE)
    for (v in unique(r1$valInfo$varName)) {
      cat(" /", v, "\n", file = spsPath, append = TRUE)
      cat(paste("  ", r1$valInfo[r1$valInfo$varName==v,]$value, autoQuote(r1$valInfo[r1$valInfo$varName==v,]$valLabel),"\n",  sep = " "),
          file = spsPath, append = TRUE)
    }
    cat(" .\n", file = spsPath, append = TRUE)
  }
}

writeMisCode <- function(r1, spsPath) {

  if(nrow(r1$misInfo) > 0) {

    cat("\nMISSING VALUES\n", file = spsPath, append = TRUE)

    for (v in unique(r1$misInfo$varName)) {
      if(length(r1$misInfo$value[r1$misInfo$varName==v]) > 3) {
        if(isTRUE(r1$chv[v])) {
         # message(paste0("Too many missing values for character variable \'", v,"\'. SPSS allows only three missing values for character variables. I will take the first 3."))
          span <- paste(r1$misInfo$value[r1$misInfo$varName==v][1:3],collapse="\', \'")
          cat(paste0(v, " ('",  span, "')\n",  sep = " "),
              file = spsPath, append = TRUE)
        } else {
          span <- paste(min(as.numeric(r1$misInfo$value[r1$misInfo$varName==v])), "THRU", max(as.numeric(r1$misInfo$value[r1$misInfo$varName==v])))
          cat(paste0(v, " (",  span, ")\n",  sep = " "),
              file = spsPath, append = TRUE)
        }
      } else {
        if(isTRUE(r1$chv[v])) {
          span <- paste(r1$misInfo$value[r1$misInfo$varName==v],collapse="\',\'")
          cat(paste0(v, " ('",  span, "')\n",  sep = " "),
              file = spsPath, append = TRUE)
        } else {
          span <- paste(r1$misInfo$value[r1$misInfo$varName==v],collapse=",")
          cat(paste0(v, " (",  span, ")\n",  sep = " "),
              file = spsPath, append = TRUE)
        }
      }
    }
    cat(".\n", file = spsPath, append = TRUE)

  }
}


