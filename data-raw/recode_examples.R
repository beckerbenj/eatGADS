## Recoding forced choice example data sets
# -----------------------------------------------------------------------------------------------
library(eatGADS)
# create example numerical, labeled variable
mcvar <- factor(c("other", "Poland", "other", "other", "other", NA,
                  "Germany", "Poland", "other", "other"),
                levels = c("Germany", "Poland", "other", "missing"))
# create example character variable
stringvar <- c(NA, "Italy", "England", "Ita", "Eng", "German",
               "Germ", NA, "", "Eng")
#create example GADS
dat <- data.frame(ID = 1:10, mcvar = mcvar,
                  stringvar = stringvar,
                  stringsAsFactors = FALSE)
gads <- import_DF(dat)
# recode missings to common numerical value
gads <- recodeGADS(gads, "mcvar", oldValues = 4, newValues = -99)
# set missing codes for values labeled "missing"
gads <- checkMissings(gads, missingLabel = "missing")
eatGADS::write_spss(gads, filePath = "inst/extdata/forcedChoice.sav")

## Missing illustration
#############################
# set up example gads with all possible combinations
mc <- factor(rep(c("other", "valid", "missing omitted", "special missing"), 3),
             levels = c("valid", "other", "missing omitted", "special missing"))
string <- c(rep("new valid", 4), rep(NA, 4),
            rep("special missing", 4))
dat <- data.frame(ID = 1:12, mc = mc, string = string, stringsAsFactors = FALSE)
gads_miss <- import_DF(dat)
gads_miss <- recodeGADS(gads_miss, "mc", oldValues = c(3,4), newValues = c(-99, -98))
gads_miss <- checkMissings(gads_miss, missingLabel = "missing")
eatGADS::write_spss(gads_miss, filePath = "inst/extdata/forcedChoice_missings.sav")


## Recoding multiple choice example data sets
# -----------------------------------------------------------------------------------------------
#create example GADS
dat <- data.frame(ID = 1:12,
                  mcvar1 = c("yes", "missing", "no", "yes", "missing", "no",
                             "yes", "missing", "no","yes", "missing", "no"),
                  mcother = c("missing", "no", "yes","missing", "no", "yes",
                              "missing", "no", "yes","missing", "no", "yes"),
                  stringvar = rep("missing by design", 12),
                  stringsAsFactors = TRUE)
gads_mc <- import_DF(dat)
# variable labels
gads_mc <- changeVarLabels(gads_mc, "mcvar1", varLabel = "German")
gads_mc <- changeVarLabels(gads_mc, "stringvar", varLabel = "other")
# recode values
oldValues <- 1:3
newValues <- c(-94, 0, 1)
for(varName in c("mcvar1", "mcother")) {
  gads_mc <- recodeGADS(GADSdat = gads_mc, varName = varName,
                     oldValues = oldValues, newValues = newValues)
}
gads_mc <- recodeGADS(GADSdat = gads_mc, varName = "stringvar",
                   oldValues = 1, newValues = -99)
# fill in strings
gads_mc$dat$stringvar <- c("German", "Ger", "Ger", "", "Eng, Pol, Ita", "Pol, Ita, Germ", "eng, ita", "germ, pol", "polish", "eng, ita", -99, "Star Trek")
# assign variable labels
gads_mc <- changeVarLabels(gads_mc, varName = c("mcvar1", "mcother", "stringvar"),
                        varLabel = c("Language: German",
                                     "Language: other",
                                     "Language: text"))
# Missing codes
gads_mc <- checkMissings(gads_mc)
eatGADS::write_spss(gads_mc, filePath = "inst/extdata/multipleChoice.sav")
