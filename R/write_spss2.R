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
#'@param changeMeta Meta data information will be changed automatically according to features of the data.
#'@param fileEncoding Data file encoding for SPSS. Default is "UTF-8".
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
write_spss2 <- function(GADSdat, filePath, syntaxPath, dec = ".", changeMeta=FALSE, fileEncoding = "UTF-8") {
  UseMethod("write_spss2")
}

#'@export
write_spss2.GADSdat <- function(GADSdat, filePath, syntaxPath, dec =".", changeMeta=FALSE, fileEncoding = "UTF-8") {

  ## Checks
  check_GADSdat(GADSdat)
  check_GADSdat_varLevel_meta(GADSdat)

  ## additional Column for SPSS, which is sometimes inadvertently shifts cases that end with NAs
  # write data
  axxx <- any(is.na(GADSdat$dat))
  GADSdat <- writeData(GADSdat=GADSdat, filePath=filePath, dec=dec, fileEncoding=fileEncoding)

  ## checkMissings
  GADSdat$labels <- checkMissings2(GADSdat$labels, changeMeta)

  ## create input for sub-functions
  r1 <- createInputWriteFunctions(GADSdat)

  ## write header
  writeHeader(r1=r1, filePath=filePath, syntaxPath=syntaxPath, changeMeta=changeMeta)

  ## write variable labels
  writeVaLab(r1=r1, syntaxPath=syntaxPath)

  ## write missing codes
  writeMisCode(r1=r1, syntaxPath=syntaxPath)

  ## delete additional variable
  cat("\nEXECUTE.\n", file = syntaxPath, append = TRUE)
  if(isTRUE(axxx)) {
    cat("DELETE VARIABLES xxxtgw.\nEXECUTE.\n", file = syntaxPath, append = TRUE)
  }
}

autoQuote <- function (x){
  paste("\"", x, "\"", sep = "")
}

readMultisep <- function(file,sep) {
  lines <- readLines(file)
  datf <- data.frame(do.call(rbind,strsplit(lines, sep, fixed = TRUE)), stringsAsFactors=FALSE)
  datf <- data.frame(lapply(datf,utils::type.convert,as.is=TRUE), stringsAsFactors=FALSE)
  return(datf)
}

checkMissings2 <- function(labels, changeMeta){

  if(isTRUE(any(labels$missings[grep("missing", labels$valLabel)] == "valid"))) {
    message("Info: Some values are labelled \'missing\' but are not declared as missing.")
    if(isTRUE(changeMeta)) {
      message("Declaration will be changed, because changeMeta=TRUE.")
      labels$missings[grep("missing", labels$valLabel)] <- "miss"
    }
  }

  if(isTRUE(any(labels$missings[!grepl("missing", labels$valLabel)] == "miss"))) {
    message("Info: Some missings are labelled without the keyword \'missing\' in their label.")
    if(isTRUE(changeMeta)) {
      message("Declaration will be changed, because changeMeta=TRUE.")
      labels$missings[!grepl("missing", labels$valLabel)] <- "valid"
    }
  }

  return(labels)
}

createInputWriteFunctions <- function(GADSdat) {
  r1 <- list()

  r1$labels <- GADSdat$labels

  stopifnot(identical(unique(r1$labels$varName),names(GADSdat$dat)))

  r1$varInfo <- unique(r1$labels[, c("varName", "varLabel", "format")])
  r1$valInfo <- unique(r1$labels[!is.na(r1$labels$value), c("varName", "value", "valLabel", "missings")])
  r1$misInfo <- unique(r1$labels[!is.na(r1$labels$value) & r1$labels$missings == "miss", c("varName", "value", "valLabel", "missings")])

  r1$varnames <- gsub("[^[:alnum:]_\\$@#]", "\\.", colnames(GADSdat$dat))
  r1$dl.varnames <- r1$varnames

  r1$chv <- sapply(GADSdat$dat, is.character)

  r1$lengths <- sapply(names(GADSdat$dat), function(ll) { if(isTRUE(is.numeric(GADSdat$dat[,ll]) & all(is.numeric(utils::type.convert(r1$labels$value[r1$labels$varName==ll],as.is=TRUE))|is.na(r1$labels$value[r1$labels$varName==ll])))) {
    max(nchar(as.character(round(abs(as.numeric(stats::na.omit(c(GADSdat$dat[,ll],r1$labels$value[r1$labels$varName==ll])))), digits=0))))
  } else {
    if(max(nchar(c(GADSdat$dat[,ll],r1$labels$value[r1$labels$varName==ll]))) > 53) {
      max(nchar(c(GADSdat$dat[,ll],r1$labels$value[r1$labels$varName==ll]))) + round(max(nchar(c(GADSdat$dat[,ll],r1$labels$value[r1$labels$varName==ll])))/8,0)
    } else {
      max(nchar(c(GADSdat$dat[,ll],r1$labels$value[r1$labels$varName==ll])))
    }
  }
  })

  r1$decimals <- sapply(names(GADSdat$dat), function(ll) { if(isTRUE(is.numeric(GADSdat$dat[,ll]) & all(is.numeric(utils::type.convert(r1$labels$value[r1$labels$varName==ll],as.is=TRUE))|is.na(r1$labels$value[r1$labels$varName==ll])))) {
    max(nchar(as.character(stats::na.omit(abs(c(GADSdat$dat[,ll],utils::type.convert(r1$labels$value[r1$labels$varName==ll],as.is=TRUE)))))))
  } else {
    if(max(nchar(c(GADSdat$dat[,ll],r1$labels$value[r1$labels$varName==ll]))) > 53) {
      max(nchar(c(GADSdat$dat[,ll],r1$labels$value[r1$labels$varName==ll]))) + round(max(nchar(c(GADSdat$dat[,ll],r1$labels$value[r1$labels$varName==ll])))/8,0)
    } else {
      max(nchar(c(GADSdat$dat[,ll],r1$labels$value[r1$labels$varName==ll])))
    }
  }
  })

  r1$varsWithDecimals <-  names(which(r1$lengths != r1$decimals))

  r1$decimals2 <- sapply(r1$varsWithDecimals, function(ll) {if(isTRUE(is.numeric(GADSdat$dat[,ll]) & all(is.numeric(utils::type.convert(r1$labels$value[r1$labels$varName==ll],as.is=TRUE))|is.na(r1$labels$value[r1$labels$varName==ll])))) {
    ast <- max(nchar(stats::na.omit(unlist(lapply(strsplit(as.character(stats::na.omit(abs(c(GADSdat$dat[,ll],utils::type.convert(r1$labels$value[r1$labels$varName==ll],as.is=TRUE))))),"\\."), function(b) b[2])))))
    if(isTRUE(ast > 16)) {
      message(paste0("Variable ", ll, " has more decimals than SPSS allows (", ast, ") and will be rounded to 16 decimal places."))
      ast <- 16
      GADSdat$dat[,ll] <- round(GADSdat$dat[,ll],16)
    }
  } else {
    ast <- max(nchar(stats::na.omit(unlist(lapply(strsplit(stats::na.omit(c(GADSdat$dat[,ll],r1$labels$value[r1$labels$varName==ll])),"\\."), function(b) b[2])))))
  }
    return(ast)
  })

  if (any(r1$chv)) {
    r1$lengths2 <- paste("(", ifelse(r1$chv, "A", "F"), r1$lengths, ")", sep = "")
  }  else {
    r1$lengths2 <- paste ( "(F", r1$lengths, ")", sep = "")
  }

  if(!is.null(r1$varsWithDecimals)){
    r1$lengths2[which(r1$dl.varnames %in% r1$varsWithDecimals)] <-sapply(seq(along=r1$lengths2[which(r1$dl.varnames %in% r1$varsWithDecimals)]), function(i) {
      x <- paste0(r1$decimals[r1$varsWithDecimals][i], ".", r1$decimals2[r1$varsWithDecimals][i],")")
      gsub("[0-9]+)$", x, r1$lengths2[which(r1$dl.varnames %in% r1$varsWithDecimals)][i])
    })
  }
  r1$dl.varnames <- paste(r1$dl.varnames, r1$lengths2)
  return(r1)
}

writeData <- function(GADSdat, filePath, dec, fileEncoding) {
  if(any(is.na(GADSdat$dat))) {
    GADSdat$dat <- cbind(GADSdat$dat,data.frame(xxxtgw=rep(1,nrow(GADSdat$dat))))
    GADSdat$labels <- rbind(GADSdat$labels, data.frame(varName="xxxtgw",varLabel=NA,format=NA,display_width=NaN,labeled="no",value=NaN,valLabel=NA,missings=NA,stringsAsFactors = FALSE))
  }

  ## write txt
  utils::write.table(GADSdat$dat, file = filePath, row.names = FALSE, col.names = FALSE,
                     sep = "]&;", dec = dec, quote = FALSE, na = "", eol = "\n", fileEncoding = fileEncoding)

  return(GADSdat)
}

writeHeader <- function(r1, filePath, syntaxPath, changeMeta) {
  if(isTRUE(changeMeta)) {
    freefield <- " free (']&;')\n"
    cat("DATA LIST FILE=", autoQuote(filePath), freefield, file = syntaxPath)
    cat(" /", r1$dl.varnames, ".\n\n", file = syntaxPath, append = TRUE,
        fill = 60, labels = " ")
  } else {
    if(any(!is.na(r1$labels$format))) {
      ninfo <- unique(r1$labels$varName[!is.na(r1$labels$format)])
      fmneu <- unlist(sapply(r1$dl.varnames, function(xx) {
        if((aj <- strsplit(xx, " \\(")[[1]][1]) %in% ninfo) {
          aa <- paste0(aj, " (", stats::na.omit(r1$labels$format[r1$labels$varName==aj])[1], ")")
          return(aa)
        }
      }))
    for(ll in r1$dl.varnames) {
      if(any(names(fmneu) %in% ll)) {
        r1$dl.varnames[r1$dl.varnames==ll] <- fmneu[which(names(fmneu) %in% ll)]
      }
    }
    }
    freefield <- " free (']&;')\n"
    cat("DATA LIST FILE=", autoQuote(filePath), freefield, file = syntaxPath)
    cat(" /", r1$dl.varnames, ".\n\n", file = syntaxPath, append = TRUE,
        fill = 60, labels = " ")
  }
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

  if (nrow(r1$misInfo) > 0) {

    cat("\nMISSING VALUES\n", file = syntaxPath, append = TRUE)

    for (v in unique(r1$misInfo$varName)) {
      if(length(r1$misInfo$value[r1$misInfo$varName==v]) > 3) {
        if(isTRUE(r1$chv[v])) {
          message(paste0("Too many missing values for character variable \'", v,"\'. SPSS allows only three missing values for character variables. I will take the first 3."))
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


