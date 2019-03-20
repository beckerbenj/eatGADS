# eatGADS

## Overview

eatGADS (educational assessment tools: GADS) is the data management and data handling tool used by the IQB. It uses SQLlite3 as a back end and handles and converts data stored as SPSS or R files. `eatDB` is used for data base creating and use, `haven` is used for importing SPSS files.

## Installation

```
# Install eatGADS from GitHub via
devtools::install_github("beckerbenj/eatGADS")
```

## Usage

```
library(eatGADS)
### Import Data
dat <- import_SPSS("someDat.sav")
r_dat <- import_DF(iris)

### Check Missings (consistency of value labels and missing codes)
dat_checked <- checkMissings(dat)

### Prepare Data
datList <- mergeLabels(list(lvl1 = dat, lvl2 = r_dat))

### Create Data Base
pkList <- list(lvl1 = "ID1", lvl2 = c("ID1", "imp"))
fkList <- list(lvl1 = list(References = NULL, Keys = NULL), lvl2 = list(References = "lvl1", Keys = "ID1"))
createGADS(datList, pkList = pkList, fkList = fkList, filePath = "someDB.db")

### Extract Meta Data
extractMeta(GADSobject = "someDB.db", vars = c("var1", "var2"))
extractMeta(GADSobject = dat, vars = c("var1", "var2"))

### get information from data base
namesGADS("someDB.db")

### pull data from data base
gads <- getGADS(filePath = "someDB.db", vSelect = c("ID1", "var1", "var2"))

### Extract data
df <- extractData(gads)

### FDZ anonymization
?fdz
```
