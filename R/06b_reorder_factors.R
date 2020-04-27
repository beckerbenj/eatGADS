
reorderFac <- function(dat, GADSdat) {
  facs <- sapply(dat, function(x) is.factor(x))
  facs <- names(facs)[facs]

  for(fac in facs) {
    facMeta <- extractMeta(GADSdat, vars = fac)[, c("value", "valLabel")]
    facMeta[, "alphValue"] <- sort(facMeta[, "valLabel"])
    dat[, fac] <- dat[, fac]
  }

  dat
}
