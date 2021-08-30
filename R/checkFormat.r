#### Check consistency of SPSS Format
#############################################################################
#' Check and Adjust SPSS Format
#'
#' Function to check if SPSS format statements are specified correctly in a \code{GADSdat} object.
#'
#' The function compares SPSS format statements \code{"format"} and actual character length and
#' decimal places of all variables in a \code{GADSdat} object and its
#' meta data information. Mismatches are reported and can be automatically adjusted.
#'
#'@param GADSdat \code{GADSdat} object imported via \code{eatGADS}.
#'@param type If \code{type='other'}, the function \code{nchar} will be used to determine character lengths and decimals are not rounded to 16 decimal places. With \code{type='SPSS'} additional width for character variables will be added in order to let SPSS read in lengthy characters correctly and .
#'@param changeFormat If \code{changeFormat=TRUE} the \code{GADSdat} meta data will be updated otherwise only information will be reported.
#'
#'@return Returns a \code{GADSdat} object.
#'
#'@examples
#'# Change example meta information (create a value label with incorrect missing code)
#' pisa2 <- checkFormat(pisa)
#'
#'
#'@export
checkFormat <- function(GADSdat, type = "SPSS", changeFormat = TRUE) {
  UseMethod("checkFormat")
}

#'@export
checkFormat.GADSdat <- function(GADSdat, type = "SPSS", changeFormat = TRUE) {
  check_GADSdat(GADSdat)
  labels <- GADSdat$labels

  chv <- sapply(GADSdat$dat, is.character)

  lengths <- sapply(names(GADSdat$dat), function(ll) {
    if(isTRUE(is.numeric(GADSdat$dat[,ll]) & all(is.numeric(utils::type.convert(labels$value[labels$varName==ll],as.is=TRUE))|is.na(labels$value[labels$varName==ll])))) {
      max(nchar(as.character(round(abs(as.numeric(stats::na.omit(c(GADSdat$dat[,ll],labels$value[labels$varName==ll])))), digits=0))))
    } else {
      if(type == "SPSS") {
        max(nchar_4_spss(c(GADSdat$dat[,ll],as.character(labels$value[labels$varName==ll]))), na.rm=TRUE)
      } else {
        max(nchar(c(GADSdat$dat[,ll],as.character(labels$value[labels$varName==ll]))), na.rm=TRUE)
      }
    }
  })

  decimals <- sapply(names(GADSdat$dat), function(ll) { if(isTRUE(is.numeric(GADSdat$dat[,ll]) & all(is.numeric(utils::type.convert(labels$value[labels$varName==ll],as.is=TRUE))|is.na(labels$value[labels$varName==ll])))) {
    max(nchar(as.character(stats::na.omit(abs(c(GADSdat$dat[,ll],utils::type.convert(labels$value[labels$varName==ll],as.is=TRUE)))))))
  } else {
    if(type == "SPSS") {
      max(nchar_4_spss(c(GADSdat$dat[,ll],as.character(labels$value[labels$varName==ll]))), na.rm=TRUE)
    } else {
      max(nchar(c(GADSdat$dat[,ll],as.character(labels$value[labels$varName==ll]))), na.rm=TRUE)
    }
  }
  })

  varsWithDecimals <-  names(which(lengths != decimals))

  decimals2 <- sapply(varsWithDecimals, function(ll) {if(isTRUE(is.numeric(GADSdat$dat[,ll]) & all(is.numeric(utils::type.convert(labels$value[labels$varName==ll],as.is=TRUE))|is.na(labels$value[labels$varName==ll])))) {
    ast <- max(nchar(stats::na.omit(unlist(lapply(strsplit(as.character(stats::na.omit(abs(c(GADSdat$dat[,ll],utils::type.convert(labels$value[labels$varName==ll],as.is=TRUE))))),"\\."), function(b) b[2])))))
    if(isTRUE(ast > 16 & type == "SPSS")) {
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
    lengths2 <- paste0(ifelse(chv, "A", "F"), lengths)
  }  else {
    lengths2 <- paste0("F", lengths)
  }


  if(length(varsWithDecimals) > 0){
    lengths2[which(names(GADSdat$dat) %in% varsWithDecimals)] <-sapply(seq(along=lengths2[which(names(GADSdat$dat) %in% varsWithDecimals)]), function(i) {
      x <- paste0(decimals[varsWithDecimals][i], ".", decimals2[varsWithDecimals][i])
      gsub("[0-9]+$", x, lengths2[which(names(GADSdat$dat) %in% varsWithDecimals)][i])
    })
  }

  lablengths <- sapply(names(GADSdat$dat), function(gg) unique(stats::na.omit(labels$format[labels$varName==gg])))

  lablengths_r <- gsub("\\.0$", "", lablengths)
  for(uu in seq(along=lablengths)) {
    if(lablengths_r[uu] != lengths2[uu]) {
      if(isTRUE(changeFormat)) {
        message(paste0("Format of Variable ", names(lablengths)[uu], " will be changed from ", lablengths[uu], " to ", lengths2[uu]))
        labels$format[labels$varName == names(lablengths)[uu]] <- lengths2[uu]
      } else {
        message(paste0("Format mismatch for Variable ", names(lablengths)[uu], ": ", lablengths[uu], " vs. ", lengths2[uu]))
      }
    }
  }

  GADSdat$labels <- labels
  GADSdat
}









