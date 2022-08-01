####
#############################################################################
#' Remove special characters.
#'
#' Remove special characters from a character vector or a \code{GADSdat} object.
#' Also suitable to fix encoding problems of a character vector or a \code{GADSdat} object. See details for available options.
#'
#' The option \code{"other"} replaces correctly encoded special signs.
#' The option \code{"ASCII"} works for strings which were encoded presumably using \code{UTF-8} and imported using \code{ASCII} encoding.
#' The option \code{"windows1250"} works for strings which were encoded presumably using \code{UTF-8}
#' and imported using \code{windows-1250} encoding.
#' The option \code{"BRISE"} covers a unique case used at the \code{FDZ at IQB}.
#'
#' If entries are all upper case, special characters are also transformed to all upper case (e.g., \code{"AE"} instead
#' of \code{"Ae"}).
#'
#'@param x A character vector or \code{GADSdat} object.
#'@param input Which encoding was used in \code{\link{import_spss}}.
#'
#'@return The modified character vector or \code{GADSdat} object.
#'
#'@examples
#' fixEncoding(c("\U00C4pfel", "\U00C4PFEL", paste0("\U00DC", "ben"), paste0("\U00DC", "BEN")))
#'
#'@export
fixEncoding <- function(x, input = c("other", "ASCII", "windows1250", "BRISE")) {
  UseMethod("fixEncoding")
}
#'@export
fixEncoding.GADSdat <- function(x, input = c("other", "ASCII", "windows1250", "BRISE")) {
  check_GADSdat(x)
  GADSdat <- x

  # fix variable names
  nam_lookup <- data.frame(oldNames = namesGADS(GADSdat), stringsAsFactors = FALSE)
  nam_lookup[["newNames"]] <- fixEncoding(nam_lookup[["oldNames"]], input = input)
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
    GADSdat$dat[[nam]] <- fixEncoding(GADSdat$dat[[nam]], input = input)
  }

  # fix meta data
  for(col_nam in c("varLabel", "valLabel")) {
    GADSdat$labels[[col_nam]] <- fixEncoding(GADSdat$labels[[col_nam]], input = input)
  }

  GADSdat
}
# inspecting encoding via spss:
# SYSFILE INFO FILE = 'q:/BT2021/BT/40_Daten/01_Rohdaten/10_FinaleDaten_211223/Daten/D_BT2021_Primar_LFB_ALLG_0-0-1.sav'

#https://www.ibm.com/docs/de/spss-statistics/28.0.0?topic=utilities-data-file-comments
#https://www.loc.gov/preservation/digital/formats/fdd/fdd000469.shtml
#https://github.com/tidyverse/haven/issues/615

# BRISE has been added manually via https://www.cogsci.ed.ac.uk/~richard/utf-8.cgi?input=%C5%93&mode=char

#'@export
fixEncoding.character <- function(x, input = c("other", "ASCII", "windows1250", "BRISE")) {
  input <- match.arg(input)
  # https://resources.german.lsa.umich.edu/schreiben/unicode/
  lookup <- switch(input, other = data.frame(unicode = c("\U00DF", "\U00E4", "\U00F6", "\U00FC",
                                                         "\U00C4", "\U00D6", "\U00DC", "\U2026"),
                                         substitute = c("ss", "ae", "oe", "ue",
                                                        "Ae", "Oe", "Ue", "..."), ## better? Ae, Oe, Ue
                                         stringsAsFactors = FALSE),
               ASCII = data.frame(unicode = c("C\026", "C..\023", "C\034", "C..\\$", "C\\$",
                                              "C..6", "C6", "C..<", "C<", "C..8", "C\037", "\001", "\025", "\005"),
                                  substitute = c("Oe", "Ue", "Ue", "ae", "ae", "oe", "oe", "ue", "ue", "ss", "ss", "", "", "..."),
                                  stringsAsFactors = FALSE),
               windows1250 = data.frame(unicode = c("\u0102\u00A4", "\u0102\u00B6", "\u0102\u013D",
                                              "\u0102\u201E", "\u0102\u2013", "\u0102\u015B",
                                              "\u0102\u017A"),
                                  substitute = c("ae", "oe", "ue",
                                                 "Ae", "Oe", "Ue",
                                                 "ss"),
                                  stringsAsFactors = FALSE),
               BRISE = data.frame(unicode = c("\u00C3\u00BC", "\u00C3\u00A4", "\u00C3\u00B6",
                                              "\u00C3\u201E", "\u00C3\u2013", "\u00C3\u0153",
                                              "\u00C3\u0178"),
                                  substitute = c("ue", "ae", "oe",
                                                 "Ae", "Oe", "Ue",
                                                 "ss"),
                                  stringsAsFactors = FALSE))
  lookup_caps <- lookup
  upper_in_lookup <- grepl("A|O|U|ss", lookup_caps$substitute)
  lookup_caps$substitute[upper_in_lookup] <- toupper(lookup$substitute[upper_in_lookup])

  caps_cases <- grepl("^[^a-z]*$", x)

  for(i in seq(nrow(lookup))) {
    x[!caps_cases] <- gsub(lookup[i, "unicode"], lookup[i, "substitute"], x[!caps_cases])
    x[caps_cases] <- gsub(lookup_caps[i, "unicode"], lookup_caps[i, "substitute"], x[caps_cases])
  }
  x
}


