

## prepare Campus Files for internal use in extdata
# -----------------------------------------------------------------------------------------------
#pisa <- import_spss("C:/Users/benjb/Documents/Promotion/Campusfiles/PISA-Plus-2012-2013_CF_v1/SPSS/PISA-Plus-2012-2013_Dataset_CF.sav")
pisa_ori <- import_spss("C:/Benjamin_Becker/21_IQB_Projekte/Campus Files/PISA-Plus-2012-2013_CF_v1/SPSS/PISA-Plus-2012-2013_Dataset_CF.sav")

pisat1 <- removeVars(pisa_ori, vars = c("Version_v1_2020_04_18", grep("t2$", namesGADS(pisa), value = TRUE)))

t1names <- grep("t1$", namesGADS(pisat1), value = TRUE)
pisa_s <- changeVarNames(pisat1, t1names, gsub("_t1$", "", t1names))
pisa_s <- removeVars(pisa_s, vars = grep("se$|eap$|wle$", namesGADS(pisa_s), value = TRUE))

# keep only 500 persons
pisa_s$dat <- pisa_s$dat[1:500,]

# make variable labels ascii compliant
pisa_s$labels$varLabel <- gsub(" Don’t ", " Do Not ", pisa_s$labels$varLabel)

out <- lapply(pisa_s$labels, function(x){
  suppressMessages(out <- tools::showNonASCII(x))
  out[!is.na(out)]
})
out

pisa_tbl <- export_tibble(pisa_s)
haven::write_sav(pisa_tbl, path = "inst/extdata/pisa.zsav", compress = TRUE)


## transform to data base for use in extdata
# -----------------------------------------------------------------------------------------------
sav_path <- system.file("extdata", "pisa.zsav", package = "eatGADS")
dat <- import_spss(sav_path)

##
pvs <- grep("pv", namesGADS(dat), value = T)
splitted_gads <- splitGADS(dat, nameList = list(noImp = namesGADS(dat)[!namesGADS(dat) %in% pvs],
                    PVs = c("idstud", pvs)))

noImp_gads <- extractGADSdat(splitted_gads, "noImp")
pvs_wide_gads <- extractGADSdat(splitted_gads, "PVs")


##
pvs_long1 <- tidyr::pivot_longer(pvs_wide_gads$dat, cols = all_of(pvs))
pvs_long2 <- tidyr::separate(pvs_long1, col = "name", sep = "_", into = c("dimension", "imp"))
pvs_long2$imp <- as.numeric(gsub("pv", "", pvs_long2$imp))

pvs_long2$dimension <- as.factor(pvs_long2$dimension)
pvs_long_gads <- updateMeta(pvs_wide_gads, newDat = as.data.frame(pvs_long2))

##
pvs_long_gads <- changeVarLabels(pvs_long_gads, varName = c("dimension", "imp", "value"),
                varLabel = c("Achievement dimension (math, reading, science)",
                             "Number of imputation of plausible values",
                             "Plausible Value"))

merged_gads <- mergeLabels(noImp = noImp_gads, PVs = pvs_long_gads)

pkList <- list(noImp = "idstud",
               PVs = c("idstud", "imp", "dimension"))
fkList <- list(noImp = list(References = NULL, Keys = NULL),
               PVs = list(References = "noImp", Keys = "idstud"))
createGADS(merged_gads, pkList = pkList, fkList = fkList,
           filePath = "inst/extdata/pisa.db")

namesGADS("inst/extdata/pisa.db")


## Create GADSdat and all_GADSdat objects for internal use
# -----------------------------------------------------------------------------------------------
sav_path <- system.file("extdata", "pisa.zsav", package = "eatGADS")
pisa <- import_spss(sav_path)

usethis::use_data(pisa, overwrite = TRUE)





