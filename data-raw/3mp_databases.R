
library(eatGADS)
load("t:/Sebastian/minidat.rda")
dat$idstud <- seq(nrow(dat))
dat$gender <- NA

set.seed(1543)
dat2 <- do.call(rbind, by(dat, dat$idstud, function(subdat) {
  subdat$gender <- sample(1:2, 1)
  subdat
}))
names(dat2) <- car::recode(names(dat2), "'comp' = 'traitLevel'")

dat_list <- list(listening = dat2, reading = dat2)
dat3 <- eatTools::do_call_rbind_withName(dat_list, colName = "dimension")
dat3$dimension <- as.factor(dat3$dimension)

# --------------------------------------------------------------
out <- by(dat3, dat3$year, function(subdat) {
  year <- unique(subdat$year)
  unimp <- unique(subdat[, c("idstud", "gender")])
  imp <- subdat[, setdiff(names(subdat), c("gender", "year"))]

  unimp_g <- import_DF(unimp)
  imp_g <- import_DF(imp)

  all_g <- mergeLabels(unImp = unimp_g, imp = imp_g)
  pkList <- list(unImp = "idstud", imp = c("idstud", "imp", "dimension"))
  fkList <- list(unImp = list(References = NULL, Keys = NULL),
                 imp = list(References = "unImp", Keys = "idstud"))

  filepath <- paste0("inst/extdata/trend_gads_", year, ".db")
  createGADS(all_g, pkList = pkList, fkList = fkList,
             filePath = filepath)
  all_g
})

# malfunctioning examples
# --------------------------------------------------------------
gads2015 <- out$`2015`
pkList <- list(unImp = "idstud", imp = c("idstud", "imp", "dimension"))
pkList2 <- list(unImp = c("idstud", "gender"), imp = c("idstud", "imp", "dimension"))
fkList <- list(unImp = list(References = NULL, Keys = NULL),
               imp = list(References = "unImp", Keys = "idstud"))
#fkList2 <- list(unImp = list(References = NULL, Keys = NULL),
#               imp = list(References = "unImp", Keys = c("idstud", "gender")))
createGADS(gads2015, pkList = pkList2, fkList = fkList,
           filePath = "inst/extdata/trend_gads_2015_pkList.db")
#createGADS(gads2015, pkList = pkList, fkList = fkList2,
#           filePath = "inst/extdata/trend_gads_2015_fkList.db")


# 2 Trend
# -------------------------------------------------------
test2 <- getTrendsGADS(filePaths = c("inst/extdata/trend_gads_2020.db",
             "inst/extdata/trend_gads_2015.db"), fast = FALSE, years = c(2020, 2015))
test2

# 3 Trend
# -------------------------------------------------------
fp1 <- system.file("extdata", "trend_gads_2020.db", package = "eatGADS")
fp2 <- system.file("extdata", "trend_gads_2015.db", package = "eatGADS")
fp3 <- system.file("extdata", "trend_gads_2010.db", package = "eatGADS")

gads_3mp <- getTrendsGADS(filePaths = c(fp1, fp2, fp3), years = c(2020, 2015, 2010), fast = FALSE)
dat_3mp <- extractData(gads_3mp)


# LEs
# --------------------------------------------------------------
load("t:/Sebastian/linking_error_template.rda")
#load("C:/Users/benjb/Desktop/linking_error_template.rda")
le

le_long <- le[, c("trendLevel1", "trendLevel2", "dimension", "depVar", "parameter", "linkingError")]
le_long$depVar <- car::recode(le_long$depVar, "'traitLevel' = 'LE_traitLevel'; 'logit' = 'LE_score'; 'minVerfehlt' = 'LE_failMin';
                                                'regErreicht' = 'LE_passReg'; 'optErreicht' = 'LE_passOpt';
                                                'transfBista' = 'LE_transfBista'" )
le_long$dimension <- car::recode(le_long$dimension, "'kbhoeren' = 'listening'; 'kblesen' = 'reading'; 'kbortho' = 'spelling'")
le_long2 <- le_long[le_long$dimension != "spelling", ]
names(le_long2) <- car::recode(names(le_long2), c("'trendLevel1' = 'year1'; 'trendLevel2' = 'year2'"))
le_long2$dimension <- as.factor(le_long2$dimension)

le_wide <- as.data.frame(tidyr::pivot_wider(le_long2, names_from = depVar, values_from = "linkingError"))
le_stufe <- le_wide[!le_wide$parameter %in% c("mean", "0"), c("year1", "year2", "dimension", "parameter", "LE_traitLevel")]
le_mean <- unique(le_wide[le_wide$parameter %in% c("mean", "0"), -5])
le_mean[le_mean$parameter == "0", c("LE_score", "LE_transfBista")] <- le_mean[le_mean$parameter == "mean", c("LE_score", "LE_transfBista")]
le_mean2 <- le_mean[le_mean$parameter == "0", -4]
le_mean_gads <- import_DF(le_mean2)
le_stufe_gads <- import_DF(le_stufe)


pkList <- list(simple = c("dimension", "year1", "year2"), traitLevel = c("dimension", "year1", "year2", "parameter"))
fkList <- list(simple = list(References = NULL, Keys = NULL),
               traitLevel = list(References = "simple", Keys = c("dimension", "year1", "year2")))
all_le <- mergeLabels(simple = le_mean_gads, traitLevel = le_stufe_gads)
createGADS(all_le, pkList = pkList, fkList = fkList, filePath = "inst/extdata/gads_LEs.db")


# 3 Trend with LEs
# -------------------------------------------------------
fp1 <- system.file("extdata", "trend_gads_2020.db", package = "eatGADS")
fp2 <- system.file("extdata", "trend_gads_2015.db", package = "eatGADS")
fp3 <- system.file("extdata", "trend_gads_2010.db", package = "eatGADS")
lep <- system.file("extdata", "gads_LEs.db", package = "eatGADS")

gads_3mp_le <- getTrendsGADS(filePaths = c(fp1, fp2, fp3), years = c(2020, 2015, 2010), fast = FALSE, lePath = lep)
dat_3mp <- extractData(gads_3mp_le)
les_3mp <- extractLEs(gads_3mp_le)

checkLEStructure(filePaths = c(fp1, fp2, fp3), lePath = lep)
