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

  if(axxx <- any(is.na(GADSdat$dat))) {
    GADSdat$dat <- cbind(GADSdat$dat,data.frame(xxxtgw=rep(1,nrow(GADSdat$dat))))
    GADSdat$labels <- rbind(GADSdat$labels, c("xxxtgw", NA,NA,NA,"no",NA,NA,NA))
  }

  ## write txt
  utils::write.table(GADSdat$dat, file = filePath, row.names = FALSE, col.names = FALSE,
                     sep = ";;;", dec = dec, quote = FALSE, na = "", eol = "\n", fileEncoding = fileEncoding)

  ##### write SPSS syntax
  labels <- GADSdat$labels

  if(isTRUE(any(labels$missings[grep("missing", labels$valLabel)] == "valid"))) {
    message("Info: Some values are labelled \'missing\' but are not declared as missing.")
    if(isTRUE(changeMeta)) {
      message("Declaration will be changed.")
      labels$missings[grep("missing", labels$valLabel)] <- "miss"
    }
  }

  if(isTRUE(any(labels$missings[!grepl("missing", labels$valLabel)] == "miss"))) {
    message("Info: Some missings are labelled without the keyword \'missing\' in their label.")
    if(isTRUE(changeMeta)) {
      message("Declaration will be changed.")
      labels$missings[!grepl("missing", labels$valLabel)] <- "valid"
    }
  }

  stopifnot(identical(unique(labels$varName),names(GADSdat$dat)))

  varInfo <- unique(labels[, c("varName", "varLabel", "format")])
  valInfo <- unique(labels[!is.na(labels$value), c("varName", "value", "valLabel", "missings")])
  misInfo <- unique(labels[!is.na(labels$value) & labels$missings == "miss", c("varName", "value", "valLabel", "missings")])

  varnames <- gsub("[^[:alnum:]_\\$@#]", "\\.", colnames(GADSdat$dat))
  dl.varnames <- varnames

  chv <- sapply(GADSdat$dat, is.character)

  lengths <- sapply(names(GADSdat$dat), function(ll) { if(isTRUE(is.numeric(GADSdat$dat[,ll]) & all(is.numeric(utils::type.convert(labels$value[labels$varName==ll]))|is.na(labels$value[labels$varName==ll])))) {
    max(nchar(as.character(round(abs(as.numeric(stats::na.omit(c(GADSdat$dat[,ll],labels$value[labels$varName==ll])))), digits=0))))
  } else {
    max(nchar(c(GADSdat$dat[,ll],labels$value[labels$varName==ll])))
  }
  })

  decimals <- sapply(names(GADSdat$dat), function(ll) { if(isTRUE(is.numeric(GADSdat$dat[,ll]) & all(is.numeric(utils::type.convert(labels$value[labels$varName==ll]))|is.na(labels$value[labels$varName==ll])))) {
    max(nchar(as.character(stats::na.omit(abs(c(GADSdat$dat[,ll],utils::type.convert(labels$value[labels$varName==ll])))))))
  } else {
    max(nchar(c(GADSdat$dat[,ll],labels$value[labels$varName==ll])))
  }
  })

  varsWithDecimals <-  names(which(lengths != decimals))

  decimals2 <- sapply(varsWithDecimals, function(ll) {if(isTRUE(is.numeric(GADSdat$dat[,ll]) & all(is.numeric(utils::type.convert(labels$value[labels$varName==ll]))|is.na(labels$value[labels$varName==ll])))) {
     ast <- max(nchar(stats::na.omit(unlist(lapply(strsplit(as.character(stats::na.omit(abs(c(GADSdat$dat[,ll],utils::type.convert(labels$value[labels$varName==ll]))))),"\\."), function(b) b[2])))))
     if(isTRUE(ast > 16)) {
       message(paste0("Variable ", ll, " has more decimals than SPSS allows (", ast, ") and will be rounded to 16 decimal places."))
       ast <- 16
       GADSdat$dat[,ll] <- round(GADSdat$dat[,ll],16)
     }
  } else {
    ast <- max(nchar(stats::na.omit(unlist(lapply(strsplit(stats::na.omit(c(GADSdat$dat[,ll],labels$value[labels$varName==ll])),"\\."), function(b) b[2])))))
  }
    return(ast)
  })

  if (any(chv)) {
    lengths2 <- paste("(", ifelse(chv, "A", "F"), lengths, ")", sep = "")
  }  else {
    lengths2 <- paste ( "(F", lengths, ")", sep = "")
  }

  if(!is.null(varsWithDecimals)){
    lengths2[which(dl.varnames %in% varsWithDecimals)] <-sapply(seq(along=lengths2[which(dl.varnames %in% varsWithDecimals)]), function(i) {
      x <- paste0(decimals[varsWithDecimals][i], ".", decimals2[varsWithDecimals][i],")")
      gsub("[0-9]+)$", x, lengths2[which(dl.varnames %in% varsWithDecimals)][i])
    })
  }
  dl.varnames <- paste(dl.varnames, lengths2)

  # write header
  if(isTRUE(changeMeta)) {
    freefield <- " free (';;;')\n"
    cat("DATA LIST FILE=", autoQuote(filePath), freefield, file = syntaxPath)
    cat(" /", dl.varnames, ".\n\n", file = syntaxPath, append = TRUE,
        fill = 60, labels = " ")
  } else {
    if(any(!is.na(labels$format))) {
      ninfo <- unique(labels$varName[!is.na(labels$format)])
      sapply(dl.varnames, function(xx) {
        if((aj <- strsplit(xx, " \\(")[[1]][1]) %in% ninfo) {
          aa <- paste0(aj, " (", stats::na.omit(labels$format[labels$varName==aj])[1], ")")
          return(aa)
        }
      })
    }
    freefield <- " free (';;;')\n"
    cat("DATA LIST FILE=", autoQuote(filePath), freefield, file = syntaxPath)
    cat(" /", dl.varnames, ".\n\n", file = syntaxPath, append = TRUE,
        fill = 60, labels = " ")
  }

  # write variable labels
  varInfo$varLabel[is.na(varInfo$varLabel)] <- ""
  cat("VARIABLE LABELS\n", file = syntaxPath, append = TRUE)
  cat(" ", paste(varInfo$varName, autoQuote(varInfo$varLabel), "\n"), ".\n",
      file = syntaxPath, append = TRUE)

  # write value labels
  if (nrow(valInfo) > 0) {

    cat("\nVALUE LABELS\n", file = syntaxPath, append = TRUE)
    for (v in unique(valInfo$varName)) {
      cat(" /", v, "\n", file = syntaxPath, append = TRUE)
      cat(paste("  ", valInfo[valInfo$varName==v,]$value, autoQuote(valInfo[valInfo$varName==v,]$valLabel),"\n",  sep = " "),
          file = syntaxPath, append = TRUE)
    }
    cat(" .\n", file = syntaxPath, append = TRUE)
  }

  # write missing codes
  if (nrow(misInfo) > 0) {

    cat("\nMISSING VALUES\n", file = syntaxPath, append = TRUE)

    for (v in unique(misInfo$varName)) {
      if(length(misInfo$value[misInfo$varName==v]) > 3) {
        if(isTRUE(chv[v])) {
          message(paste0("Too many missing values for character variable \'", v,"\'. SPSS allows only three missing values for character variables. I will take the first 3."))
          span <- paste(misInfo$value[misInfo$varName==v][1:3],collapse="\', \'")
          cat(paste0(v, " ('",  span, "')\n",  sep = " "),
              file = syntaxPath, append = TRUE)
        } else {
          span <- paste(min(as.numeric(misInfo$value[misInfo$varName==v])), "THRU", max(as.numeric(misInfo$value[misInfo$varName==v])))
          cat(paste0(v, " (",  span, ")\n",  sep = " "),
              file = syntaxPath, append = TRUE)
        }
      } else {
        span <- paste(misInfo$value[misInfo$varName==v],collapse=",")
        if(isTRUE(chv[v])) {
          cat(paste0(v, " ('",  span, "')\n",  sep = " "),
              file = syntaxPath, append = TRUE)
        } else {
          cat(paste0(v, " (",  span, ")\n",  sep = " "),
              file = syntaxPath, append = TRUE)
        }
      }
    }
    cat(".\n", file = syntaxPath, append = TRUE)

  }

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
