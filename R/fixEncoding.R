####
#############################################################################
#' Fix encoding problems.
#'
#' Fix some encoding problems of a character vector or a \code{GADSdat} object, which was encoded presumably using \code{UTF-8}
#' and imported using \code{ASCII} encoding.
#'
#'@param x A character vector or \code{GADSdat} object.
#'
#'@return The modified character vector or \code{GADSdat} object.
#'
#'@examples
#' # tbd
#'
#'@export
fixEncoding <- function(x) {
  UseMethod("fixEncoding")
}
#'@export
fixEncoding.GADSdat <- function(x) {
  check_GADSdat(x)
  GADSdat <- x

  # fix variable names
  nam_lookup <- data.frame(oldNames = namesGADS(GADSdat), stringsAsFactors = FALSE)
  nam_lookup[["newNames"]] <- fixEncoding(nam_lookup[["oldNames"]])
  nam_lookup_diff <- nam_lookup[nam_lookup$oldNames != nam_lookup$newNames, ]
  #browser()
  for(oldNam in nam_lookup_diff$oldNames) {
    newNam <- nam_lookup_diff[nam_lookup_diff$oldNames == oldNam, "newNames"]
    GADSdat <- changeVarNames(GADSdat, oldNames = oldNam, newNames = newNam)
  }

  # fix data
  character_vars <- sapply(GADSdat$dat, is.character)
  character_vars <- names(character_vars)[character_vars]
  for(nam in character_vars) {
    GADSdat$dat[[nam]] <- fixEncoding(GADSdat$dat[[nam]])
  }

  # fix meta data
  for(col_nam in c("varLabel", "valLabel")) {
    GADSdat$labels[[col_nam]] <- fixEncoding(GADSdat$labels[[col_nam]])
  }

  GADSdat
}
# inspecting encoding via spss:
# SYSFILE INFO FILE = 'q:/BT2021/BT/40_Daten/01_Rohdaten/10_FinaleDaten_211223/Daten/D_BT2021_Primar_LFB_ALLG_0-0-1.sav'

#'@export
fixEncoding.character <- function(x) {
  lookup <- data.frame(unicode = c("C\026", "C..\023", "C\034", "C..\\$", "C\\$", "C..6", "C6", "C..<", "C<", "C..8", "C\037", "\001", "\025", "\005"),
             substitute = c("Oe", "Ue", "Ue", "ae", "ae", "oe", "oe", "ue", "ue", "ss", "ss", "", "", "..."),
             stringsAsFactors = FALSE)
  for(i in seq(nrow(lookup))) {
    x <- gsub(lookup[i, "unicode"], lookup[i, "substitute"], x)
  }
  x
}

