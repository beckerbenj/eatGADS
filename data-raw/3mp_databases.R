
library(eatGADS)
load("t:/Sebastian/minidat.rda")
dat$idstud <- seq(nrow(dat))
dat$gender <- NA

set.seed(1543)
dat2 <- do.call(rbind, by(dat, dat$idstud, function(subdat) {
  subdat$gender <- sample(1:2, 1)
  subdat
}))

# --------------------------------------------------------------
out <- by(dat2, dat2$year, function(subdat) {
  year <- unique(subdat$year)
  unimp <- unique(subdat[, c("idstud", "gender")])
  imp <- subdat[, setdiff(names(subdat), c("gender", "year"))]

  unimp_g <- import_DF(unimp)
  imp_g <- import_DF(imp)

  all_g <- mergeLabels(unImp = unimp_g, imp = imp_g)
  pkList <- list(unImp = "idstud", imp = c("idstud", "imp", "comp"))
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
pkList <- list(unImp = "idstud", imp = c("idstud", "imp", "comp"))
pkList2 <- list(unImp = c("idstud", "gender"), imp = c("idstud", "imp", "comp"))
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
test <- getTrendsGADS(filePaths = c("inst/extdata/trend_gads_2020.db", "inst/extdata/trend_gads_2015.db",
                      "inst/extdata/trend_gads_2010.db"), fast = FALSE, years = c(2020, 2015, 2010))
test

dat <- extractData(test)

table(dat$year)

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
le

le_long <- le[, c("trendLevel1", "trendLevel2", "dimension", "depVar", "parameter", "linkingError")]

le_wide <- as.data.frame(tidyr::pivot_wider(le_long, names_from = depVar, values_from = "linkingError"))
le_stufe <- le_wide[!le_wide$parameter %in% c("mean", "0"), c("trendLevel1", "trendLevel2", "dimension", "parameter", "traitLevel")]
le_mean <- unique(le_wide[le_wide$parameter %in% c("mean", "0"), -5])
le_mean[le_mean$parameter == "0", c("logit", "transfBista")] <- le_mean[le_mean$parameter == "mean", c("logit", "transfBista")]
le_mean2 <- le_mean[le_mean$parameter == "0", -4]

