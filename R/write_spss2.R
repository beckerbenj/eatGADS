
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
#'
#'@return Writes a \code{txt} and an \code{sav} file to disc, returns nothing.
#'
#'
#'@export
write_spss2 <- function(GADSdat, filePath, syntaxPath, dec = ".") {
  UseMethod("write_spss2")
}

#'@export
write_spss2.GADSdat <- function(GADSdat, filePath, syntaxPath, dec =".") {

  ## write txt
  utils::write.table(GADSdat$dat, file = filePath, row.names = FALSE, col.names = FALSE,
              sep = "\t", dec = dec, quote = FALSE, na = "", eol = "\n")

  ##### write SPSS syntax
  labels <- GADSdat$labels
  varInfo <- unique(labels[, c("varName", "varLabel", "format")])
  valInfo <- unique(labels[!is.na(labels$value) & labels$missings == "valid", c("varName", "value", "valLabel", "missings")])
  misInfo <- unique(labels[!is.na(labels$value) & labels$missings == "miss", c("varName", "value", "valLabel", "missings")])

  varnames <- gsub("[^[:alnum:]_\\$@#]", "\\.", colnames(GADSdat$dat))
  dl.varnames <- varnames

  chv <- sapply(GADSdat$dat, is.character)
  lengths <- sapply(names(GADSdat$dat), function(ll) { if(is.numeric(GADSdat$dat[,ll]) & is.numeric(labels$value[labels$varName==ll])) {
                            max(nchar(round(stats::na.omit(abs(c(GADSdat$dat[,ll],labels$value[labels$varName==ll]))), digits=0)))
                           } else {
                            max(nchar(c(GADSdat$dat[,ll],labels$value[labels$varName==ll])))
                            }
                          })
  decimals <- sapply(names(GADSdat$dat), function(ll) { if(is.numeric(GADSdat$dat[,ll]) & is.numeric(labels$value[labels$varName==ll])) {
                            max(nchar(as.character(stats::na.omit(abs(c(GADSdat$dat[,ll],labels$value[labels$varName==ll]))))))
                          } else {
                            max(nchar(c(GADSdat$dat[,ll],labels$value[labels$varName==ll])))
                            }
                          })

  varsWithDecimals <-  names(which(lengths != decimals))

  decimals2 <- sapply(varsWithDecimals, function(ll) {if(is.numeric(GADSdat$dat[,ll]) & is.numeric(labels$value[labels$varName==ll])) {
max(nchar(stats::na.omit(unlist(lapply(strsplit(as.character(stats::na.omit(abs(c(GADSdat$dat[,ll],labels$value[labels$varName==ll])))),"\\."), function(b) b[2])))))
                          } else {
                            max(nchar(stats::na.omit(unlist(lapply(strsplit(stats::na.omit(c(GADSdat$dat[,ll],labels$value[labels$varName==ll])),"\\."), function(b) b[2])))))
                            }
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
  freefield <- " free (TAB)\n"
  cat("DATA LIST FILE=", autoQuote(filePath), freefield, file = syntaxPath)
  cat(" /", dl.varnames, ".\n\n", file = syntaxPath, append = TRUE,
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
      cat(paste("  ", valInfo$value, autoQuote(valInfo$valLabel),"\n",  sep = " "),
          file = syntaxPath, append = TRUE)
    }
    cat(" .\n", file = syntaxPath, append = TRUE)
  }

  # write missing codes
  if (nrow(misInfo) > 0) {

    cat("\nMISSING VALUES\n", file = syntaxPath, append = TRUE)

    for (v in unique(misInfo$varName)) {
      if(isTRUE(chv[v])) {
        cat(paste0(v, " ('",  paste(misInfo$value[misInfo$varName==v],collapse="','"), "')\n",  sep = " "),
            file = syntaxPath, append = TRUE)

      } else {
      cat(paste0(v, " (",  paste(misInfo$value[misInfo$varName==v],collapse=","), ")\n",  sep = " "),
          file = syntaxPath, append = TRUE)
      }
    }
    cat(".\n", file = syntaxPath, append = TRUE)

  }

  cat("\nEXECUTE.\n", file = syntaxPath, append = TRUE)

  return()
}

autoQuote <- function (x){
  paste("\"", x, "\"", sep = "")
}
