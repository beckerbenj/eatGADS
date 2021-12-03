
library(eatGADS)
load("t:/Sebastian/minidat.rda")
dat$idstud <- seq(nrow(dat))
dat$gender <- NA

dat2 <- do.call(rbind, by(dat, dat$idstud, function(subdat) {
  subdat$gender <- sample(1:2, 1)
  subdat
}))


by(dat2, dat2$year, function(subdat) {
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
})

# 2 Trend
# -------------------------------------------------------
test2 <- getTrendGADS(filePath1 = "inst/extdata/trend_gads_2020.db",
             filePath2 = "inst/extdata/trend_gads_2015.db", fast = FALSE, years = c(2020, 2015))
test2

# 3 Trend
# -------------------------------------------------------
test <- getTrend3GADS(filePaths = c("inst/extdata/trend_gads_2020.db", "inst/extdata/trend_gads_2015.db",
                      "inst/extdata/trend_gads_2010.db"), fast = FALSE, years = c(2020, 2015, 2010))
test2
