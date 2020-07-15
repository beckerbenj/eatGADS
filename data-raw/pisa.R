## code to prepare `pisa` dataset goes here


sav_path <- system.file("extdata", "LV_2011_CF.sav", package = "eatGADS")
dat <- import_spss(sav_path)

## Reshapen
pvs <- grep("pv0", namesGADS(dat), value = T)

splitted_gads <- splitGADS(dat, list(noImp = namesGADS(dat)[!namesGADS(dat) %in% pvs],
                    PVs = c("idstud_FDZ", pvs)))

noImp <- extractGADSdat(splitted_gads, "noImp")
pvs_wide <- extractGADSdat(splitted_gads, "PVs")

out <- tidyr::pivot_longer(pvs_wide$dat, cols = pvs)
out2 <- tidyr::separate(out, col = "name", sep = c(2, 4, 5), into = c("pv", "imp", "sepa", "dimension"))
out3 <- out2[, c("idstud_FDZ", "imp", "dimension", "value")]
out3$imp <- as.numeric(out3$imp)

pvs_long <- import_DF(out3)


merged_gads <- mergeLabels(noImp = noImp, PVs = pvs_long)

str(merged_gads)

pkList <- list(noImp = "idstud_FDZ",
               PVs = c("idstud_FDZ", "imp", "dimension"))
fkList <- list(noImp = list(References = NULL, Keys = NULL),
               PVs = list(References = "noImp", Keys = "idstud_FDZ"))
createGADS(merged_gads, pkList = pkList, fkList = fkList,
           filePath = "inst/extdata/pisa.db")

namesGADS("inst/extdata/pisa.db")



## create data base
usethis::use_data("pisa")
